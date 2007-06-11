# -*- coding: utf-8 -*-
"""
    Sphinx
    ~~~~~~

    The Python documentation toolchain.

    :copyright: 2007 by Georg Brandl.
    :license: Python license.
"""

import sys
import getopt
from os import path

from .builder import builders
from .console import nocolor


def usage(argv, msg=None):
    if msg:
        print >>sys.stderr, msg
        print >>sys.stderr
    print >>sys.stderr, """\
usage: %s [options] restdirname outdirname [filenames...]"
options: -b <builder> -- builder to use (one of %s)
         -u -- only build new and changed files (requires a saved environment)
         -O <option[=value]> -- give an option to to the builder
         -N -- do not do colored output
modi:
* without -u and without filenames, build all source files.
* with -u, build new and changed files.
* with filenames, build these.""" % (argv[0], ', '.join(builders))


def main(argv):
    try:
        opts, args = getopt.getopt(argv[1:], 'ub:O:N')
        srcdirname = path.abspath(args[0])
        if not path.isdir(srcdirname):
            print >>sys.stderr, 'Cannot find source directory.'
            return 1
        if not path.isfile(path.join(srcdirname, 'conf.py')):
            print >>sys.stderr, 'Source directory doesn\'t contain conf.py file.'
            return 1
        outdirname = path.abspath(args[1])
        if not path.isdir(outdirname):
            print >>sys.stderr, 'Cannot find output directory.'
            return 1
    except (IndexError, getopt.error):
        usage(argv)
        return 1

    filenames = args[2:]
    err = 0
    for filename in filenames:
        if not path.isfile(filename):
            print >>sys.stderr, 'Cannot find file %r.' % filename
            err = 1
    if err:
        return 1

    builder = update = None
    options = {}
    for opt, val in opts:
        if opt == '-b':
            if val not in builders:
                usage(argv, 'Invalid builder value specified.')
                return 1
            builder = val
        elif opt == '-u':
            if filenames:
                usage(argv, 'Cannot combine update option and filenames.')
                return 1
            update = True
        elif opt == '-O':
            if '=' in val:
                key, val = val.split('=')
                try:
                    val = int(val)
                except: pass
            else:
                key, val = val, True
            options[key] = val
        elif opt == '-N':
            nocolor()

    if builder is None:
        print 'No builder selected, using default: html'
        builder = 'html'

    builderobj = builders[builder](srcdirname, outdirname, options,
                                   status_stream=sys.stdout,
                                   warning_stream=sys.stderr)
    if update:
        builderobj.build_update()
    elif filenames:
        builderobj.build_specific(filenames)
    else:
        builderobj.build_all()


if __name__ == '__main__':
    sys.exit(main(sys.argv))
