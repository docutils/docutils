#!/usr/bin/env python

"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.2.1
"""

import glob, os, webbrowser
from docutils.core import publish_cmdline

def convert():
    for file in glob.glob('*.txt'):
        htmlfile = '%s.html' % os.path.splitext(file)[0]
        print '%s --> %s' % (file, htmlfile)
        publish_cmdline(writer_name='html',
                        argv = [file, htmlfile])

if __name__=='__main__':
    print '\nGenerating HTML ...\n'
    convert()
    os.chdir('doc')
    convert()
    print '\nOpen doc/index.html with default webbrowser ...\n'
    webbrowser.open('index.html')
