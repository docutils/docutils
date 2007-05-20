# -*- coding: utf-8 -*-
"""
    sphinx.web.views
    ~~~~~~~~~~~~~~~~

    :copyright: 2007 by Georg Brandl.
    :license: Python license.
"""

import cPickle as pickle
from os import path

from django.http import HttpResponse, Http404
from django.conf import settings
from django.views import static

from ..util import relative_uri

from jinja import Environment, FileSystemLoader

static_path = path.join(path.dirname(__file__), '..', 'style')
templates_path = path.join(path.dirname(__file__), '..', 'templates')
jinja_env = Environment(loader=FileSystemLoader(templates_path,
                                                use_memcache=True),
                        friendly_traceback=False)

rootpath = settings.DOC_ROOT_PATH
cache = {}

specials = [
    'index',
    'genindex',
    'modindex',
    'search',
]


def get_target_uri(source_filename):
    if source_filename == 'index.rst':
        return ''
    if source_filename.endswith('/index.rst'):
        return source_filename[:-9] # up to /
    return source_filename[:-4] + '/'


def showpage(request, url):
    try:
        filename, mtime, text = cache[url]
    except KeyError:
        pass
    else:
        if path.getmtime(filename) == mtime:
            return HttpResponse(text)

    if url in specials:
        filename = path.join(rootpath, 'specials.pickle')
        tfile = open(filename, 'rb')
        context = pickle.load(tfile)
        tfile.close()
        templatename = url + '.html'
    else:
        for filename in [path.join(rootpath, url)+'.fpickle',
                         path.join(rootpath, url, 'index.fpickle')]:
            try:
                tfile = open(filename, 'rb')
            except:
                pass
            else:
                context = pickle.load(tfile)
                tfile.close()
                break
        else:
            raise Http404('page not found')
        templatename = 'page.html'

    def relpath_to(otheruri, resource=False):
        if not resource:
            otheruri = get_target_uri(otheruri)
        return relative_uri(url + '/', otheruri)
    context['pathto'] = relpath_to

    text = jinja_env.get_template(templatename).render(context)
    cache[url] = (filename, path.getmtime(filename), text)
    return HttpResponse(text)


def staticfile(request, fn):
    return static.serve(request, path=fn, document_root=static_path)
