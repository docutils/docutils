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

import cPickle as pickle
from os import path

from .util import Request, Response, RedirectResponse, SharedDataMiddleware, \
     NotFound, jinja_env
from ..search import SearchFrontend
from ..util import relative_uri, shorten_result
from .database import connect, set_connection, Comment


special_urls = set(['index', 'genindex', 'modindex'])


def get_target_uri(source_filename):
    if source_filename == 'index.rst':
        return ''
    if source_filename.endswith('/index.rst'):
        return source_filename[:-9] # up to /
    return source_filename[:-4] + '/'


def render_template(req, template_name, context=None):
    context = context or {}
    tmpl = jinja_env.get_template(template_name)

    def relative_path_to(otheruri, resource=False):
        if not resource:
            otheruri = get_target_uri(otheruri)
        return relative_uri(req.path, otheruri)
    context['pathto'] = relative_path_to

    # add it here a second time for templates that don't
    # get the builder information from the environment (such as search)
    context['builder'] = 'web'

    return tmpl.render(context)


class DocumentationApplication(object):
    """
    Serves the documentation.
    """

    def __init__(self, conf):
        self.cache = {}
        self.data_root = conf['data_root_path']
        with file(path.join(self.data_root, 'environment.pickle')) as f:
            self.env = pickle.load(f)
        with file(path.join(self.data_root, 'searchindex.pickle')) as f:
            self.search_frontend = SearchFrontend(pickle.load(f))
        self.db_con = connect(path.join(self.data_root, 'sphinx.db'))

    def search(self, req):
        """
        Search the database.
        """
        # disabled for now
        if not req.args.get('q'):
            return RedirectResponse('')
        return RedirectResponse('q/%s/' % req.args['q'])

        results = None
        if 'q' in req.args:
            # sidebar checks for keywords too.
            if req.args.get('check_keywords') == 'yes':
                resp = self.get_keyword_matches(req, req.args['q'], True)
                if resp is not None:
                    return resp
            results = []
            for fn, title in self.search_frontend.search(req.args['q'],
                                            req.args.getlist('area')):
                with file(path.join(self.data_root, 'sources', fn[:-4] + '.txt')) as f:
                    source = f.read().decode('utf-8')
                context = shorten_result(source, req.args['q'].split())
                results.append((fn, title, context))
        return Response(render_template(req, 'search.html', {
            'search_results':       results,
            'search_performed':     bool(req.args.get('q')),
            'current_page_name':    'search'
        }))

    def show_source(self, req, page):
        """
        Show the highlighted source for a given page.
        """
        source_name = path.join(self.data_root, 'sources', page + '.txt')
        if not path.exists(source_name):
            return self.get_keyword_matches(req)
        with file(source_name) as f:
            return Response(f.read(), mimetype='text/plain')

    def get_page(self, req, url):
        """
        Show the requested documentation page or raise an
        `NotFound` exception to display a page with close matches.
        """
        page_id = url + '.rst'
        cache_possible = True

        # do the form validation and comment saving if the
        # request method is post.
        title = author = author_mail = comment_body = ''
        form_error = False
        if req.method == 'POST':
            title = req.form.get('title')
            author = req.form.get('author')
            author_mail = req.form.get('author_mail')
            comment_body = req.form.get('comment_body')

            form_error = not (title and author and author_mail and
                              comment_body)

            if not form_error:
                self.cache.pop(url, None)
                comment = Comment(page_id, title, author, author_mail,
                                  comment_body)
                comment.save()
                return RedirectResponse(comment.url)
            cache_possible = False

        # if the form validation fails the cache is used so that
        # we can put error messages and defaults to the page.
        if cache_possible:
            try:
                filename, mtime, text = self.cache[url]
            except KeyError:
                pass
            else:
                if path.getmtime(filename) == mtime:
                    return Response(text)

        # render special templates such as the index
        if url in special_urls:
            filename = path.join(self.data_root, 'specials.pickle')
            with open(filename, 'rb') as f:
                context = pickle.load(f)
            templatename = url + '.html'

        # render the page based on the settings in the pickle
        else:
            for filename in [path.join(self.data_root, url) + '.fpickle',
                             path.join(self.data_root, url, 'index.fpickle')]:
                if not path.exists(filename):
                    continue
                with open(filename, 'rb') as f:
                    context = pickle.load(f)
                    break
            else:
                raise NotFound()
            templatename = 'page.html'

        context['comments'] = Comment.get_for_page(page_id)
        context['form'] = {
            'title':            title,
            'author':           author,
            'author_mail':      author_mail,
            'comment_body':     comment_body,
            'error':            form_error
        }
        text = render_template(req, templatename, context)

        if cache_possible:
            self.cache[url] = (filename, path.getmtime(filename), text)
        return Response(text)

    pretty_type = {
        'data': 'module data',
        'cfunction': 'C function',
        'cmember': 'C member',
        'cmacro': 'C macro',
        'ctype': 'C type',
        'cvar': 'C variable',
    }

    def get_keyword_matches(self, req, term=None, avoid_fuzzy=False):
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
        # module references
        if term in self.env.modules:
            filename, title, system = self.env.modules[term]
            url = get_target_uri(filename)
            return RedirectResponse(url)
        # direct references
        if term in self.env.descrefs:
            filename, ref_type = self.env.descrefs[term]
            url = get_target_uri(filename) + '#' + term
            return RedirectResponse(url)

        if avoid_fuzzy:
            return

        # get some close matches
        close_matches = []
        good_matches = 0
        for ratio, type, filename, title, desc in self.env.get_close_matches(term):
            link = get_target_uri(filename)
            if type != 'module':
                link += '#' + title
            good_match = ratio > 0.75
            good_matches += good_match
            close_matches.append({
                'href':         relative_uri(req.path, link),
                'title':        title,
                'good_match':   good_match,
                'type':         self.pretty_type.get(type, type),
                'description':  desc
            })
        return Response(render_template(req, 'not_found.html', {
            'close_matches':        close_matches,
            'good_matches_count':   good_matches,
            'keyword':              term
        }), status=404)

    def __call__(self, environ, start_response):
        """
        Dispatch requests.
        """
        set_connection(self.db_con)
        req = Request(environ)
        if not req.path.endswith('/') and req.method == 'GET':
            query = req.environ.get('QUERY_STRING', '')
            if query:
                query = '?' + query
            resp = RedirectResponse(req.path + '/' + query)
        elif req.path.startswith('/source/'):
            sourcename = req.path[8:].strip('/')
            resp = self.show_source(req, sourcename)
        else:
            url = req.path.strip('/') or 'index'
            if url == 'search':
                resp = self.search(req)
            elif url == 'index' and 'q' in req.args:
                resp = RedirectResponse('q/%s/' % req.args['q'])
            elif url.startswith('q/'):
                resp = self.get_keyword_matches(req, url[2:])
            else:
                try:
                    resp = self.get_page(req, url)
                except NotFound:
                    resp = self.get_keyword_matches(req)
        return resp(environ, start_response)


def make_app(conf):
    """
    Create the WSGI application based on a configuration dict.
    Handled configuration values so far:

    `data_root_path`
        the folder containing the documentation data as generated
        by sphinx with the web builder.
    """
    app = DocumentationApplication(conf)
    app = SharedDataMiddleware(app, {
        '/style':   path.join(conf['data_root_path'], 'style')
    })
    return app
