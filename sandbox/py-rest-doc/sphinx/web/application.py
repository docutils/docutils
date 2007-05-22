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
from .util import Request, Response, SharedDataMiddleware, NotFound, \
     render_template


special_urls = set(['index', 'genindex', 'modindex', 'search'])


def get_target_uri(source_filename):
    if source_filename == 'index.rst':
        return ''
    if source_filename.endswith('/index.rst'):
        return source_filename[:-9] # up to /
    return source_filename[:-4] + '/'


class DocumentationApplication(object):

    def __init__(self, conf):
        self.cache = {}
        self.data_root = conf['data_root_path']

    def get_page(self, req, url):
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

        def relative_path_to(otheruri, resource=False):
            if not resource:
                otheruri = get_target_uri(otheruri)
            return relative_uri(url + '/', otheruri)
        context['pathto'] = relative_path_to

        text = render_template(templatename, context)
        self.cache[url] = (filename, path.getmtime(filename), text)
        return Response(text)

    def get_close_matches(self, req, url):
        raise NotImplementedError(url)

    def __call__(self, environ, start_response):
        req = Request(environ)
        url = req.path.strip('/') or 'index'
        try:
            resp = self.get_page(req, url)
        except NotFound:
            resp = self.get_close_matches(req, url)
        return resp(environ, start_response)


def make_app(conf=None):
    app = DocumentationApplication(conf or {})
    app = SharedDataMiddleware(app, {
        '/style':   path.join(path.dirname(__file__), '..', 'style')
    })
    return app
