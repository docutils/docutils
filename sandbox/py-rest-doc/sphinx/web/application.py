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

import os
import re
import copy
import time
import heapq
import difflib
import tempfile
import threading
import cPickle as pickle
import cStringIO as StringIO
from os import path
from itertools import groupby
from collections import defaultdict

from .feed import Feed
from .mail import Email
from .admin import AdminPanel
from .userdb import UserDatabase
from .antispam import AntiSpam
from .database import connect, set_connection, Comment
from .util import Request, Response, RedirectResponse, SharedDataMiddleware, \
     NotFound, render_template, render_simple_template, get_target_uri, \
     blackhole_dict

from ..util import relative_uri, shorten_result
from ..search import SearchFrontend
from ..writer import HTMLWriter
from ..builder import LAST_BUILD_FILENAME, ENV_PICKLE_FILENAME

from docutils.io import StringOutput
from docutils.utils import Reporter
from docutils.frontend import OptionParser

_mail_re = re.compile(r'^([a-zA-Z0-9_\.\-])+\@'
                      r'(([a-zA-Z0-9\-])+\.)+([a-zA-Z0-9]{2,})+$')

env_lock = threading.Lock()


PATCH_MESSAGE = '''\
A new documentation patch has been submitted.
  Author:  %(author)s <%(email)s>
  Date:    %(asctime)s
  Page:    %(page_id)s
  Summary: %(summary)s

'''

known_designs = {
    'default':      (['default.css', 'pygments.css'],
                     'The default design, with the sidebar on the left side.'),
    'rightsidebar': (['default.css', 'rightsidebar.css', 'pygments.css'],
                     'Display the sidebar on the right side.'),
    'stickysidebar': (['default.css', 'stickysidebar.css', 'pygments.css'],
                      '''\
                      Display the sidebar on the left and don\'t scroll it
                      with the content. This can cause parts of the content to
                      become inaccessible when the table of contents is too long.'''),
}

comments_methods = {
    'in': 'Show all comments inline.',
    'bot': 'Show all comments at the page bottom.',
    'no': 'Don\'t show comments at all.',
}


class MockBuilder(object):
    def get_relative_uri(self, from_, to):
        return ''


class DocumentationApplication(object):
    """
    Serves the documentation.
    """

    special_urls = set(['index', 'genindex', 'modindex', 'settings'])

    def __init__(self, config):
        self.cache = blackhole_dict() if config['debug'] else {}
        self.freqmodules = defaultdict(int)
        self.last_most_frequent = []
        self.generated_stylesheets = {}
        self.config = config
        self.data_root = config['data_root_path']
        self.buildfile = path.join(self.data_root, LAST_BUILD_FILENAME)
        self.buildmtime = -1
        self.load_env(0)
        self.db_con = connect(path.join(self.data_root, 'sphinx.db'))
        self.antispam = AntiSpam(path.join(self.data_root, 'bad_content'))
        self.userdb = UserDatabase(path.join(self.data_root, 'docusers'))
        self.admin_panel = AdminPanel(self)

    def load_env(self, new_mtime):
        env_lock.acquire()
        try:
            if self.buildmtime == new_mtime:
                # happens if another thread already reloaded the env
                return
            print "* Loading the environment..."
            with file(path.join(self.data_root, ENV_PICKLE_FILENAME)) as f:
                self.env = pickle.load(f)
            with file(path.join(self.data_root, 'globalcontext.pickle')) as f:
                self.globalcontext = pickle.load(f)
            with file(path.join(self.data_root, 'searchindex.pickle')) as f:
                self.search_frontend = SearchFrontend(pickle.load(f))
            self.buildmtime = path.getmtime(self.buildfile)
            self.cache.clear()
        finally:
            env_lock.release()

    def search(self, req):
        """
        Search the database. Currently just a keyword based search.
        """
        if not req.args.get('q'):
            return RedirectResponse('')
        return RedirectResponse('q/%s/' % req.args['q'])

    def get_page_source(self, page):
        """
        Get the reST source of a page.
        """
        page_id = self.env.get_real_filename(page)
        if page_id is None:
            raise NotFound()
        filename = path.join(self.data_root, 'sources', page_id)[:-3] + 'txt'
        with file(filename) as f:
            return page_id, f.read()

    def show_source(self, req, page):
        """
        Show the highlighted source for a given page.
        """
        return Response(self.get_page_source(page)[1], mimetype='text/plain')

    def suggest_changes(self, req, page):
        """
        Show a "suggest changes" form.
        """
        page_id, contents = self.get_page_source(page)

        return Response(render_template(req, 'edit.html', self.globalcontext, dict(
            contents=contents,
            pagename=page,
            doctitle=self.globalcontext['titles'].get(page_id) or 'this page',
            submiturl=relative_uri('/@edit/'+page+'/', '/@submit/'+page),
        )))

    def _generate_preview(self, page_id, contents):
        """
        Generate a preview for suggested changes.
        """
        handle, pathname = tempfile.mkstemp()
        os.write(handle, contents.encode('utf-8'))
        os.close(handle)

        warning_stream = StringIO.StringIO()
        env2 = copy.deepcopy(self.env)
        destination = StringOutput(encoding='utf-8')
        writer = HTMLWriter(env2.config)
        doctree = env2.read_file(page_id, pathname, save_parsed=False)
        doctree = env2.get_and_resolve_doctree(page_id, MockBuilder(), doctree)
        doctree.settings = OptionParser(defaults=env2.settings,
                                        components=(writer,)).get_default_values()
        doctree.reporter = Reporter(page_id, 2, 4, stream=warning_stream)
        output = writer.write(doctree, destination)
        writer.assemble_parts()
        return writer.parts['fragment']

    def submit_changes(self, req, page):
        """
        Submit the suggested changes as a patch.
        """
        if req.method != 'POST':
            # only available via POST
            raise NotFound()
        if req.form.get('cancel'):
            # handle cancel requests directly
            return RedirectResponse(page)
        # raises NotFound if page doesn't exist
        page_id, orig_contents = self.get_page_source(page)
        author = req.form.get('name')
        email = req.form.get('email')
        summary = req.form.get('summary')
        contents = req.form.get('contents')
        fields = (author, email, summary, contents)

        form_error = None
        rendered = None

        if not all(fields):
            form_error = 'You have to fill out all fields.'
        elif not _mail_re.search(email):
            form_error = 'You have to provide a valid e-mail address.'
        elif req.form.get('homepage') or self.antispam.is_spam(fields):
            form_error = 'Your text contains blocked URLs or words.'
        else:
            if req.form.get('preview'):
                rendered = self._generate_preview(page_id, contents)

            else:
                asctime = time.asctime()
                contents = contents.splitlines()
                orig_contents = orig_contents.splitlines()
                diffname = 'suggestion on %s by %s <%s>' % (asctime, author, email)
                diff = difflib.unified_diff(orig_contents, contents, n=3,
                                            fromfile=page_id, tofile=diffname,
                                            lineterm='')
                diff_text = '\n'.join(diff)
                try:
                    mail = Email(
                        self.config['patch_mail_from'], 'Python Documentation Patches',
                        self.config['patch_mail_to'], '',
                        'Patch for %s by %s' % (page_id, author),
                        PATCH_MESSAGE % locals(),
                        self.config['patch_mail_smtp'],
                    )
                    mail.attachments.add_string('patch.diff', diff_text, 'text/x-diff')
                    mail.send()
                except:
                    import traceback
                    traceback.print_exc()
                    # XXX: how to report?
                    pass
                return Response(render_template(req, 'submitted.html',
                                                self.globalcontext, dict(
                    backlink=relative_uri('/@submit/'+page+'/', page+'/')
                )))

        return Response(render_template(req, 'edit.html', self.globalcontext, dict(
            contents=contents,
            author=author,
            email=email,
            summary=summary,
            pagename=page,
            form_error=form_error,
            rendered=rendered,
            submiturl=relative_uri('/@edit/'+page+'/', '/@submit/'+page),
        )))


    def get_settings_page(self, req):
        """
        Handle the settings page.
        """
        referer = req.environ.get('HTTP_REFERER') or ''
        if referer:
            rel_referer = referer.lstrip(
                req.environ['wsgi.url_scheme'] + '://' + req.environ['HTTP_HOST'])
            if rel_referer == referer:
                referer = ''
            else:
                referer = rel_referer

        if req.method == 'POST':
            new_style = req.form.get('design')
            if new_style and new_style in known_designs:
                req.session['design'] = new_style
            new_comments = req.form.get('comments')
            if new_comments and new_comments in comments_methods:
                req.session['comments'] = new_comments
            if req.form.get('goback') and req.form.get('referer'):
                return RedirectResponse(req.form['referer'])
            # else display the same page again
            referer = ''

        context = {
            'known_designs':    sorted(known_designs.iteritems()),
            'comments_methods': comments_methods.items(),
            'on_index':         False,
            'curdesign':        req.session.get('design') or 'default',
            'curcomments':      req.session.get('comments') or 'in',
            'referer':          referer,
        }

        return Response(render_template(req, 'settings.html',
                                        self.globalcontext, context))


    def get_module_index(self, req):
        """
        Get the module index or redirect to a module from the module index.
        """
        modname = req.args.get('mod')
        if modname:
            info = self.env.modules.get(modname)
            if not info:
                raise NotFound()
            return RedirectResponse(relative_uri(
                '/modindex/', info[0][:-4] + '#module-' + modname))

        context = {
            'known_designs':    sorted(known_designs.iteritems()),
            'on_index':         False,
        }

        most_frequent = heapq.nlargest(30, self.freqmodules.iteritems(),
                                       lambda x: x[1])
        most_frequent = sorted(x[0] for x in most_frequent)

        if most_frequent != self.last_most_frequent or \
               '_modindex' not in self.cache:
            filename = path.join(self.data_root, 'modindex.fpickle')
            with open(filename, 'rb') as f:
                context.update(pickle.load(f))
            context['freqentries'] = most_frequent
            resp = render_template(req, 'modindex.html',
                                   self.globalcontext, context)
            self.cache['_modindex'] = resp
            self.last_most_frequent = most_frequent
        else:
            resp = self.cache['_modindex']
        return Response(resp)

    def handle_comments(self, page_id, context):
        """
        Insert inline comments into a page body.
        """
        if 'body' not in context:
            return None

        tx = context['body']
        all_comments = Comment.get_for_page(page_id)
        global_comments = []
        for name, comments in groupby(all_comments, lambda x: x.associated_name):
            if not name:
                global_comments = list(comments)
                continue
            tx = re.sub('<!--#%s#-->' % name,
                        render_simple_template('inlinecomments.html',
                                               {'comments' : list(comments),
                                                'id' : name}),
                        tx)
        tx = re.sub('<!--#([^#]*)#-->',
                    (lambda match: render_simple_template('inlinecomments.html',
                                                          {'id': match.group(1)})),
                    tx)
        context['body'] = tx
        return global_comments

    def get_page(self, req, url):
        """
        Show the requested documentation page or raise an
        `NotFound` exception to display a page with close matches.
        """
        cache_possible = True
        context = {
            'known_designs':    sorted(known_designs.iteritems()),
            'on_index':         url == 'index',
            'curdesign':        req.session.get('design') or 'default',
            'curcomments':      req.session.get('comments') or 'in',
        }

        # these are special because they have different templates
        if url in self.special_urls:
            page_id = '@' + url  # only used as cache key
            filename = path.join(self.data_root, url + '.fpickle')
            with open(filename, 'rb') as f:
                context.update(pickle.load(f))
            templatename = url + '.html'
            comments = False

        # it's a normal page (or a 404)
        else:
            page_id = self.env.get_real_filename(url)
            if page_id is None:
                raise NotFound(show_keyword_matches=True)
            # increment view count of all modules on that page
            for modname in self.env.filemodules.get(page_id, ()):
                self.freqmodules[modname] += 1
            # comments enabled?
            comments = self.env.metadata[page_id].get('comments_enabled', True)

            # generate comments feed if wanted
            if comments and req.args.get('feed') == 'comments':
                return self.get_comments_feed(req, url, page_id)

            # else load the page
            filename = path.join(self.data_root, page_id[:-3] + 'fpickle')
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
                    preview = Comment(page_id, title, author, author_mail,
                                      comment_body)
                # 'homepage' is a forbidden field to thwart bots
                elif req.form.get('homepage') or self.antispam.is_spam(fields):
                    form_error = 'Your text contains blocked URLs or words.'
                else:
                    if not all(fields):
                        form_error = 'You have to fill out all fields.'
                    elif _mail_re.search(author_mail) is None:
                        form_error = 'You have to provide a valid e-mail address.'
                    elif len(comment_body) < 20:
                        form_error = 'You comment is too short ' \
                                     '(must have at least 20 characters).'
                    else:
                        self.cache.pop(page_id, None)
                        # XXX
                        comment = Comment(page_id, req.form.get('descname'),
                                          title, author, author_mail,
                                          comment_body)
                        comment.save()
                        return RedirectResponse(comment.url)
                cache_possible = False

            context.update(
                comments_enabled = comments,
                preview = preview,
                comments_form = {
                    'title':         title,
                    'author':        author,
                    'author_mail':   author_mail,
                    'comment_body':  comment_body,
                    'error':         form_error,
                },
            )

            context['comments'] = self.handle_comments(page_id, context)

        # if the form validation failed, the cache is used so that
        # we can put error messages and defaults to the page.
        # XXX: caching disabled for now
        if cache_possible:
            try:
                filename, mtime, text = self.cache[page_id]
            except KeyError:
                pass
            else:
                if path.getmtime(filename) == mtime:
                    return Response(text)
            text = render_template(req, templatename, self.globalcontext, context)
            self.cache[page_id] = (filename, path.getmtime(filename), text)
        else:
            text = render_template(req, templatename, self.globalcontext, context)
        return Response(text)

    def get_comments_feed(self, req, url, page_id):
        # XXX: nice title instead of "url"
        feed = Feed(req, 'Comments for "%s"' % url, 'List of comments for '
                    'the topic "%s"' % url, url)
        for comment in Comment.get_for_page(page_id):
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
        return Response(render_template(req, 'not_found.html', self.globalcontext))

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
            }, self.globalcontext), status=404 if is_error_page else 404)

    def get_user_stylesheet(self, req):
        """
        Stylesheets are exchangeable. Handle them here and
        cache them on the server side until server shuts down
        and on the client side for 1 hour but not if in debug mode.
        """
        style = req.session.get('design')
        if style not in known_designs:
            style = 'default'

        if style in self.generated_stylesheets:
            stylesheet = self.generated_stylesheets[style]
        else:
            stylesheet = []
            for filename in known_designs[style][0]:
                with file(path.join(self.data_root, 'style', filename)) as f:
                    stylesheet.append(f.read())
            stylesheet = '\n'.join(stylesheet)
            if not self.config.get('debug'):
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

        # check if the environment was updated
        new_mtime = path.getmtime(self.buildfile)
        if self.buildmtime != new_mtime:
            self.load_env(new_mtime)

        try:
            if req.path == 'favicon.ico':
                resp = self.get_error_404()
            # require a trailing slash on GET requests
            # this ensures nice looking urls and working relative
            # links for cached resources.
            elif not req.path.endswith('/') and req.method == 'GET':
                query = req.environ.get('QUERY_STRING', '')
                if query:
                    query = '?' + query
                resp = RedirectResponse(req.path + '/' + query)
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
            # go to the search page. this is currently just a redirect
            # to /q/ which is handled below
            elif url == 'search':
                resp = self.search(req)
            elif url == 'settings':
                resp = self.get_settings_page(req)
            # module index page is special
            elif url == 'modindex':
                resp = self.get_module_index(req)
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


def _check_superuser(app):
    """Check if there is a superuser and create one if necessary."""
    if not app.userdb.users:
        print 'Warning: you have no user database or no master "admin" account.'
        create = raw_input('Do you want to create an admin account now? [y/n] ')
        if not create or create.lower().startswith('y'):
            import getpass
            print 'Creating "admin" user.'
            pw1 = getpass.getpass('Enter password: ')
            pw2 = getpass.getpass('Enter password again: ')
            if pw1 != pw2:
                print 'Error: Passwords don\'t match.'
                sys.exit(1)
            app.userdb.set_password('admin', pw1)
            app.userdb.privileges['admin'].add('master')
            app.userdb.save()


def setup_app(config, check_superuser=False):
    """
    Create the WSGI application based on a configuration dict.
    Handled configuration values so far:

    `data_root_path`
        the folder containing the documentation data as generated
        by sphinx with the web builder.
    """
    app = DocumentationApplication(config)
    if check_superuser:
        _check_superuser(app)
    app = SharedDataMiddleware(app, {
        '/style':   path.join(config['data_root_path'], 'style')
    })
    return app
