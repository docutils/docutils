# -*- coding: utf-8 -*-
"""
    sphinx.web.application
    ~~~~~~~~~~~~~~~~~~~~~~

    A simple WSGI application that serves an interactive version
    of the python documentation.

    :copyright: 2007 by Georg Brandl, Armin Ronacher.
    :license: Python license.
"""
from __future__ import with_statement

import re
import cPickle as pickle
from os import path

from .feed import Feed
from .admin import AdminPanel
from .antispam import AntiSpam
from .database import connect, set_connection, Comment
from .userdb import UserDatabase
from .util import Request, Response, RedirectResponse, SharedDataMiddleware, \
     NotFound, render_template, get_target_uri
from ..search import SearchFrontend
from ..util import relative_uri, shorten_result


_mail_re = re.compile(r'^([a-zA-Z0-9_\.\-])+\@'
                      r'(([a-zA-Z0-9\-])+\.)+([a-zA-Z0-9]{2,})+$')


class DocumentationApplication(object):
    """
    Serves the documentation.
    """

    special_urls = set(['index', 'genindex', 'modindex'])

    def __init__(self, conf):
        self.cache = {}
        self.generated_stylesheets = {}
        self.data_root = conf['data_root_path']
        self.debug = conf['debug']
        with file(path.join(self.data_root, 'environment.pickle')) as f:
            self.env = pickle.load(f)
        with file(path.join(self.data_root, 'searchindex.pickle')) as f:
            self.search_frontend = SearchFrontend(pickle.load(f))
        self.db_con = connect(path.join(self.data_root, 'sphinx.db'))
        self.antispam = AntiSpam(path.join(self.data_root, 'bad_content'))
        self.userdb = UserDatabase(path.join(self.data_root, 'docusers'))
        self.admin_panel = AdminPanel(self)

    def search(self, req):
        """
        Search the database. Currently just a keyword based search.
        """
        if not req.args.get('q'):
            return RedirectResponse('')
        return RedirectResponse('q/%s/' % req.args['q'])

    def show_source(self, req, page):
        """
        Show the highlighted source for a given page.
        """
        page_id = self.env.get_real_filename(page)
        if page_id is None:
            raise NotFound()
        filename = path.join(self.data_root, 'sources', page_id)[:-3] + 'txt'
        with file(filename) as f:
            return Response(f.read(), mimetype='text/plain')

    def suggest_changes(self, req, page):
        """
        Show a "suggest changes" form.
        """
        page_id = self.env.get_real_filename(page)
        if page_id is None:
            raise NotFound()
        filename = path.join(self.data_root, 'sources', page_id)[:-3] + 'txt'
        with file(filename) as f:
            contents = f.read()

        return Response(render_template(req, 'edit.html', dict(
            contents=contents,
            pagename=page,
            submiturl=relative_uri('/@edit/'+page+'/', '/@submit/'+page),
        )))

    def submit_changes(self, req, page):
        author = req.args.get('name')
        email = req.args.get('email')
        contents = req.args.get('contents')

        if not author or not email or not contents:
            raise NotFound()

    def get_page(self, req, url):
        """
        Show the requested documentation page or raise an
        `NotFound` exception to display a page with close matches.
        """
        cache_possible = True
        context = {
            'known_designs':    sorted(self.known_designs),
            'on_index':         url == 'index'
        }

        # these are special because they have different templates
        if url in self.special_urls:
            rstfilename = '@' + url  # only used as cache key
            filename = path.join(self.data_root, url + '.fpickle')
            with open(filename, 'rb') as f:
                context.update(pickle.load(f))
            templatename = url + '.html'
            comments = False

        # it's a normal page (or a 404)
        else:
            rstfilename = self.env.get_real_filename(url)
            if rstfilename is None:
                raise NotFound(show_keyword_matches=True)
            comments = self.env.metadata[rstfilename].get('comments_enabled', True)

            # generate comments feed if wanted
            if comments and req.args.get('feed') == 'comments':
                return self.get_comments_feed(req, url, rstfilename)

            # else load the page
            filename = path.join(self.data_root, rstfilename[:-3] + 'fpickle')
            with open(filename, 'rb') as f:
                context.update(pickle.load(f))
            templatename = 'page.html'

            # default values for the comment form
            title = comment_body = author = author_mail = ''
            form_error = None
            preview = None

            # do form validation and comment saving if the request method is POST.
            if comments and req.method == 'POST':
                title = req.form.get('title', '').strip()
                author = req.form.get('author', '').strip()
                author_mail = req.form.get('author_mail', '')
                comment_body = req.form.get('comment_body', '')
                fields = (title, author, author_mail, comment_body)

                if req.form.get('preview'):
                    preview = Comment(rstfilename, title, author, author_mail,
                                      comment_body)
                # 'homepage' is a forbidden field to thwart bots
                elif req.form.get('homepage') or self.antispam.is_spam(fields):
                    form_error = 'Your text contains blocked URLs or words.'
                else:
                    if not all(fields):
                        form_error = 'You have to fill out all fields.'
                    elif _mail_re.search(author_mail) is None:
                        form_error = 'You have to provide a valid mail address.'
                    elif len(comment_body) < 20:
                        form_error = 'You comment is too short.'
                    else:
                        self.cache.pop(rstfilename, None)
                        comment = Comment(rstfilename, title, author, author_mail,
                                          comment_body)
                        comment.save()
                        return RedirectResponse(comment.url)
                cache_possible = False

            context.update(
                comments_enabled = comments,
                comments = Comment.get_for_page(rstfilename),
                preview = preview,
                comments_form = {
                    'title':         title,
                    'author':        author,
                    'author_mail':   author_mail,
                    'comment_body':  comment_body,
                    'error':         form_error,
                },
            )

        # if the form validation failed, the cache is used so that
        # we can put error messages and defaults to the page.
        if cache_possible:
            try:
                filename, mtime, text = self.cache[rstfilename]
            except KeyError:
                pass
            else:
                if path.getmtime(filename) == mtime:
                    return Response(text)
            text = render_template(req, templatename, context)
            self.cache[rstfilename] = (filename, path.getmtime(filename), text)
        else:
            text = render_template(req, templatename, context)
        return Response(text)

    def get_comments_feed(self, req, url, rstfilename):
        # XXX: nice title instead of "url"
        feed = Feed(req, 'Comments for "%s"' % url, 'List of comments for '
                    'the topic "%s"' % url, url)
        for comment in Comment.get_for_page(rstfilename):
            feed.add_item(comment.title, comment.author, comment.url,
                          comment.parsed_comment_body, comment.pub_date)
        return Response(feed.generate(), mimetype='application/rss+xml')

    def get_recent_comments_feed(self, req):
        """
        Get the feed of recent comments.
        """
        feed = Feed(req, 'Recent Comments', 'Recent Comments', '')
        for comment in Comment.get_recent():
            feed.add_item(comment.title, comment.author, comment.url,
                          comment.parsed_comment_body, comment.pub_date)
        return Response(feed.generate(), mimetype='application/rss+xml')

    def get_error_404(self, req):
        """
        Show a simple error 404 page.
        """
        return Response(render_template(req, 'not_found.html'))

    pretty_type = {
        'data': 'module data',
        'cfunction': 'C function',
        'cmember': 'C member',
        'cmacro': 'C macro',
        'ctype': 'C type',
        'cvar': 'C variable',
    }

    def get_keyword_matches(self, req, term=None, avoid_fuzzy=False,
                            is_error_page=False):
        """
        Find keyword matches. If there is an exact match, just redirect:
        http://docs.python.org/os.path.exists would automatically
        redirect to http://docs.python.org/modules/os.path/#os.path.exists.
        Else, show a page with close matches.

        Module references are processed first so that "os.path" is handled as
        a module and not as member of os.
        """
        if term is None:
            term = req.path.strip('/')

        matches = self.env.find_keyword(term, avoid_fuzzy)

        # if avoid_fuzzy is False matches can be None
        if matches is None:
            return

        if isinstance(matches, tuple):
            url = get_target_uri(matches[1])
            if matches[0] != 'module':
                url += '#' + matches[2]
            return RedirectResponse(url)
        else:
            # get some close matches
            close_matches = []
            good_matches = 0
            for ratio, type, filename, anchorname, desc in matches:
                link = get_target_uri(filename)
                if type != 'module':
                    link += '#' + anchorname
                good_match = ratio > 0.75
                good_matches += good_match
                close_matches.append({
                    'href':         relative_uri(req.path, link),
                    'title':        anchorname,
                    'good_match':   good_match,
                    'type':         self.pretty_type.get(type, type),
                    'description':  desc,
                })
            return Response(render_template(req, 'keyword_not_found.html', {
                'close_matches':        close_matches,
                'good_matches_count':   good_matches,
                'keyword':              term
            }), status=404 if is_error_page else 404)

    known_designs = {
        'default':      ['default.css', 'pygments.css'],
        'rightsidebar': ['default.css', 'rightsidebar.css', 'pygments.css']
    }

    def get_user_stylesheet(self, req):
        """
        Stylesheets are exchangeable. Handle them here and
        cache them on the server side until server shuts down
        and on the client side for 1 hour but not if in debug mode.
        """
        style = req.session.get('design')
        if style not in self.known_designs:
            style = 'default'

        new_style = req.args.get('new_design')
        if new_style:
            if new_style in self.known_designs:
                req.session['design'] = new_style
            return RedirectResponse('')

        if style in self.generated_stylesheets:
            stylesheet = self.generated_stylesheets[style]
        else:
            stylesheet = []
            for filename in self.known_designs[style]:
                with file(path.join(self.data_root, 'style', filename)) as f:
                    stylesheet.append(f.read())
            stylesheet = '\n'.join(stylesheet)
            if not self.debug:
                self.generated_stylesheets[style] = stylesheet

        if req.args.get('admin') == 'yes':
            with file(path.join(self.data_root, 'style', 'admin.css')) as f:
                stylesheet += '\n' + f.read()

        # XXX: add timestamp based http caching
        return Response(stylesheet, mimetype='text/css')

    def __call__(self, environ, start_response):
        """
        Dispatch requests.
        """
        set_connection(self.db_con)
        req = Request(environ)
        url = req.path.strip('/') or 'index'

        try:
            # require a trailing slash on GET requests
            # this ensures nice looking urls and working relative
            # links for cached resources.
            if not req.path.endswith('/') and req.method == 'GET':
                query = req.environ.get('QUERY_STRING', '')
                if query:
                    query = '?' + query
                resp = RedirectResponse(req.path + '/' + query)
            # go to the search page. this is currently just a redirect
            # to /q/ which is handled below
            elif url == 'search':
                resp = self.search(req)
            # index page is special
            elif url == 'index':
                # alias for fuzzy search
                if 'q' in req.args:
                    resp = RedirectResponse('q/%s/' % req.args['q'])
                # feeds
                elif req.args.get('feed') == 'recent_comments':
                    resp = self.get_recent_comments_feed(req)
                # stylesheet
                elif req.args.get('do') == 'stylesheet':
                    resp = self.get_user_stylesheet(req)
                else:
                    resp = self.get_page(req, 'index')
            # start the fuzzy search
            elif url[:2] == 'q/':
                resp = self.get_keyword_matches(req, url[2:])
            # source view
            elif url[:8] == '@source/':
                resp = self.show_source(req, url[8:])
            # suggest changes view
            elif url[:6] == '@edit/':
                resp = self.suggest_changes(req, url[6:])
            # suggest changes submit
            elif url[:8] == '@submit/':
                resp = self.submit_changes(req, url[8:])
            # dispatch requests to the admin panel
            elif url == '@admin' or url[:7] == '@admin/':
                resp = self.admin_panel.dispatch(req, url[7:])
            # everything else is handled as page or fuzzy search
            # if a page does not exist.
            else:
                resp = self.get_page(req, url)
        # views can raise a NotFound exception to show an error page.
        # Either a real not found page or a similar matches page.
        except NotFound, e:
            if e.show_keyword_matches:
                resp = self.get_keyword_matches(req, is_error_page=True)
            else:
                resp = self.get_error_404(req)
        return resp(environ, start_response)


def make_app(conf):
    """
    Create the WSGI application based on a configuration dict.
    Handled configuration values so far:

    `data_root_path`
        the folder containing the documentation data as generated
        by sphinx with the web builder.
    """
    orig_app = DocumentationApplication(conf)
    app = SharedDataMiddleware(orig_app, {
        '/style':   path.join(conf['data_root_path'], 'style')
    })
    return orig_app, app
