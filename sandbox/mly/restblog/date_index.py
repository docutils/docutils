#!/usr/bin/python -tt

# date_index.py
#
# Create chronological index for reStructuredBlog
#
# (C) Magnus Lyckå, Thinkware AB, 2003

import glob, restblog
import cPickle as pickle

IX_FILE_NAME = 'date_index.dat'

try:
    db = pickle.load(file(IX_FILE_NAME))
except:
    db = {'filenames':[], 'index':[]}

for fn in glob.glob(restblog.source_pattern):
    if not fn in db['filenames']:
        db['filenames'].append(fn)
        meta = restblog.Metadata(fn)
        db['index'].append((meta.timestamp(),
                            meta.targetname(),
                            meta.subject()))
db['index'].sort()
db['index'].reverse()
pickle.dump(db, file(IX_FILE_NAME, 'w'))

body = ['Index By Date\n--------------------\n\n']
oldMonthYear = ''
for ts, fn, subj in db['index']:
    monthYear = ts.strftime('%B %Y')
    if monthYear != oldMonthYear:
        body.append("\n\n%s\n.............................\n\n" % monthYear)
        oldMonthYear = monthYear
    body.append("* `%s %s`__\n\n__ %s\n" % (
        ts.strftime('%Y-%m-%d %H:%M'), subj, fn))
restblog.makepage('date_index', "\n".join(body))
