#!/usr/bin/env python2
#
# $Id$
# $Source$
#

"""docutils to HTML CGI converter.

CGI script that automatically converts docutils files into html and serves
it. The script caches the HTML files and uses the timestamps to regenerate if
necessary.  Also, the indexes are generated automatically.


"""

__author__ = 'Martin Blais <blais@iro.umontreal.ca>'
__version__ = '$Revision$'

#===============================================================================
# EXTERNAL DECLARATIONS
#===============================================================================

import sys
assert sys.version_info[0] == 2
if sys.version_info[1] > 1:
    import cgitb; cgitb.enable()

import os, cgi, stat, re
from os.path import *
import commands
from pprint import pprint

from ConfigParser import ConfigParser
#cgi.print_environ()

if not os.environ.has_key('NO_CONTENT_TYPE'):
    print "Content-type: text/html"


#===============================================================================
# LOCAL DECLARATIONS
#===============================================================================

configfn = join(os.getcwd(), 'rst-server.conf')
if not exists(configfn):
    print 'Error: config file "%s" does not exist' % configfn
    sys.exit(1)

confparser = ConfigParser()
confparser.add_section('options')
confparser.read(configfn)

class Dummy: pass
opts = Dummy()

for o in ['source', 'cache', 'converter']:
    if not confparser.has_option('options', o):
        print 'Error: must configure variable', o
        raise SystemExit()
    setattr(opts, o.replace('-', '_'), confparser.get('options', o))

for o in ['docutils-config']:
    val = None
    if confparser.has_option('options', o):
        val = confparser.get('options', o)
    setattr(opts, o.replace('-', '_'), val)

form = cgi.FieldStorage()
if form.has_key('p'):
    fn = form["p"].value
else:
    fn = ''

if os.environ.has_key('CONVERTER'):
    opts.converter = os.environ['CONVERTER']

# if from_php:
#     if len(sys.argv) > 1:
#         fn = sys.argv[1]

#===============================================================================
# LOCAL DECLARATIONS
#===============================================================================

rootstr = '(notes)'

rejre = map(re.compile, ['^\s*[-=]+\s*$', '^\.\.', '^\s*$', '\s*:\w+:'])

def gettitle( fn ):

    #print '<pre>'
    try:
        f = open(fn, 'r')
        while 1:
            l = f.readline()
            if not l:
                break
            rej = 0
            for r in rejre:
                if r.match(l):
                    rej = 1
                    break
            if rej:
                #print l
                continue

            title = l.strip()
            break
        f.close()
    except:
        title = fn
    #print '</pre>'

    # prevent tag-only titles, at least we'll get an error message
    if title[0] == '<' and title[-1] == '>':
        title = title[1:-1]
    return title

valre = re.compile('(.*)\.txt')

def nav( fn, script ):
    if fn and isfile(join(opts.source, fn)):
        mo = valre.match(fn)
        assert(mo)
        fn = mo.group(1)
    
    comps = []
    if fn:
        fn = normpath(fn)
        comps += fn.split(os.sep)

    print '<table class="toptable">'
    print '<tr><td>'
    ccomps = []
    cpath = ''
    if comps:
        for i in comps[0:-1]:
            cpath = join(cpath, i)
            ccomps.append('<a href="%s?p=%s">%s</a>' % (script, cpath, i))
        ccomps.append('%s' % comps[-1])
    ccomps = ['<a href="%s">%s</a>' % (script, rootstr)] + ccomps
    print '&nbsp;&raquo;&nbsp;'.join(ccomps)
    print '</td></tr>'
    print '</table>'

print """
<?xml version="1.0" encoding="iso8859-1" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
  <meta name="GENERATOR" content="home made recipe in python">
   <style type="text/css"><!--

.toptable {
   width: 100%;
   background-color: #EEEEEE }

.sep {
   width: 100px }
   
.conversion-error {
   background-color: #FF8888;
   margin: 0.5em;
   padding: 1em }

--></style>
</head>

<body>
"""



script = os.environ['SCRIPT_NAME']

# make sure the cache directory exists
if not exists(opts.cache):
    os.mkdir(opts.cache)

if fn:
    srcfn = join(opts.source, fn)
else:
    srcfn = opts.source

nav(fn, script)

class Page:
    def __init__( self, fn, base ):
        self.fn = fn
        self.base = base
        self.title = base

if not exists(srcfn):
    print 'Error: this file does not exist'
else:    
    if isdir(srcfn):
        #cachefn = join(cache, fn, 'index.html')
        
        files, dirs = [], []
        for f in os.listdir(srcfn):
            if f.startswith('.') or f.startswith('#'):
                continue
            if f == 'CVS':
                continue
            af = join(srcfn, f)
            if isfile(af):
                mo = valre.match(f)
                if mo:
                    files.append( Page(f, mo.group(1)) )
            elif isdir(af):
                dirs.append(f)

        print '<ul>'
        for f in files:
            f.title = gettitle(join(srcfn, f.fn))
        files.sort(lambda x, y: cmp(x.title, y.title))

        for f in files:
            print '<li><a href="%s?p=%s">%s</li>' % \
                  (script, join(fn, f.fn), f.title)
        print '</ul>'

        print '<ul>'
        for f in dirs:
            print '<li><a href="%s?p=%s">%s</li>' % (script, join(fn, f), f)
        print '</ul>'
    else:
        if not fn.endswith('.txt'):
            print 'request for file not ending in .txt', fn
            sys.exit(0)

        cachefn = join(opts.cache, '%s.html' % splitext(fn)[0])
        regen = 1
        if exists(srcfn) and exists(cachefn):
            fnstat, cachestat = map(os.stat, [srcfn, cachefn])
            if fnstat[stat.ST_MTIME] <= cachestat[stat.ST_MTIME]:
                # use cache
                regen = 0

        if regen:
            cachedir = dirname(cachefn)
            if not exists(cachedir):
                os.makedirs(cachedir)
            
            cmd = opts.converter
            if opts.docutils_config:
                cmd += ' --config="%s"' % opts.docutils_config
            cmd += ' "%s" "%s" 2>&1' % (srcfn, cachefn)
            if 0:
                chin, chout, cherr = os.popen3(cmd)
                chin.close()
                err = chout.read()
                out = chout.read()
            else:
                s, out = commands.getstatusoutput(cmd)
                if out:
                    print '<div class="conversion-error">'
                    print 'Error: converting document:'
                    print '<pre>'
                    print out
                    print '</pre>'
                    print '</div>'
                    

        print open(cachefn, 'r').read()

print """
</body>
</html>
"""

