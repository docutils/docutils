#!/usr/bin/python -tt

# main_index.py
#
# Create main index for reStructuredBlog
#
# (C) Magnus Lyckå, Thinkware AB, 2003

import glob, restblog
import cPickle as pickle

BLOGS_TO_SHOW = 5

db = []
for fn in glob.glob(restblog.source_pattern):
    meta = restblog.Metadata(fn)
    db.append((meta.timestamp(), fn))

db.sort()
db.reverse()

name = "Magnus Lyckå's Web Log".decode('latin1')
body = ['%s\n%s' % (name, '-'*len(name))]
body.append('*Here are my latest web logs*')
for ts, fn in db[:5]:
    body.append("-----\n\n*%s*" % ts.strftime('%Y-%m-%d %H:%M'))
    body.append(".. include:: %s" % fn)
restblog.makepage('index', "\n\n".join(body))
