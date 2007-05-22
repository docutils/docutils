# -*- coding: utf-8 -*-
"""
    sphinx.web.application
    ~~~~~~~~~~~~~~~~~~~~~~

    A simple WSGI application that serves an interactive version
    of the python documentation.

    :copyright: 2007 by Armin Ronacher.
    :license: Python license.
"""
from __future__ import with_statement
import cPickle as pickle
from os import path
from ..util import relative_uri
from .util import Request, Response, RedirectResponse, SharedDataMiddleware, \
     NotFound, jinja_env


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

    return tmpl.render(context)


class DocumentationApplication(object):
    """
    Serves the documentation.
    """

    def __init__(self, conf):
        self.cache = {}
        self.data_root = conf['data_root_path']
        with file(path.join(self.data_root, 'environment.pickle')) as f:
            self.environment = pickle.load(f)

    def search(self, req):
        """
        Search the database.
        """
        if req.method == 'POST':
            pass
        return Response(render_template(req, 'search.html'))

    def get_page(self, req, url):
        """
        Show the requested documentation page or raise an
        `NotFound` exception to display a page with close matches.
        """
        try:
            filename, mtime, text = self.cache[url]
        except KeyError:
            pass
        else:
            if path.getmtime(filename) == mtime:
                return Response(text)

        if url in special_urls:
            filename = path.join(self.data_root, 'specials.pickle')
            with open(filename, 'rb') as f:
                context = pickle.load(f)
            templatename = url + '.html'

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

        text = render_template(req, templatename, context)
        self.cache[url] = (filename, path.getmtime(filename), text)
        return Response(text)

    def get_close_matches(self, req, url):
        """
        Lookup close matches. If there is an exact match (for example
        http://docs.python.org/os.path.exists would automatically
        redirect to http://docs.python.org/modules/os.path/#os.path.exists.

        Module references are processed first so that "os.path" is
        handled as module and not as member of os.
        """
        # module references
        if url in self.environment.modules:
            filename, title, system = self.environment.modules[url]
            url = get_target_uri(filename)
            return RedirectResponse(url)
        # direct references
        if url in self.environment.descrefs:
            filename, ref_type = self.environment.descrefs[url]
            url = get_target_uri(filename) + '#' + url
            return RedirectResponse(url)
        # get some close matches
        close_matches = []
        for type, filename, title, desc in self.environment.get_close_matches(url):
            link = get_target_uri(filename)
            if type == 'ref':
                link += '#' + title
            close_matches.append({
                'href':         relative_uri(url + '/', link),
                'title':        title,
                'type':         type,
                'description':  desc
            })
        return Response(render_template(req, 'not_found.html', {
            'close_matches':    close_matches,
            'keyword':          url
        }), status=404)

    def __call__(self, environ, start_response):
        """
        Dispatch the requests.
        """
        req = Request(environ)
        if not req.path.endswith('/') and req.method == 'GET':
            query = req.environ.get('QUERY_STRING', '')
            if query:
                query = '?' + query
            resp = RedirectResponse(req.path + '/' + query)
        else:
            url = req.path.strip('/') or 'index'
            if url == 'search':
                resp = self.search(req)
            else:
                try:
                    resp = self.get_page(req, url)
                except NotFound:
                    resp = self.get_close_matches(req, url)
        return resp(environ, start_response)


def make_app(conf):
    """
    Creates the WSGI application based on a configuration passed.
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
