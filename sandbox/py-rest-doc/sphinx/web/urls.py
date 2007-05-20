# -*- coding: utf-8 -*-
"""
    sphinx.web.urls
    ~~~~~~~~~~~~~~~

    :copyright: 2007 by Georg Brandl.
    :license: Python license.
"""

from django.conf.urls.defaults import *

urlpatterns = patterns('sphinx.web.views',
    (r'^style/(?P<fn>.*)$', 'staticfile'),
    (r'^(?P<url>.*)/@comment/$', 'comment'),

    # special case
    (r'^$', 'showpage', {'url': 'index'}),
    (r'^(?P<url>.*)/$', 'showpage'),
)
