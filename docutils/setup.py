#!/usr/bin/env python
# $Id$

from distutils.core import setup

def do_setup():
    dist = setup(
          name = 'docutils',
          description = 'Python Documentation Utilities',
          #long_description = '',
          url = 'http://docutils.sourceforge.net/',
          version = '0.1',
          author = 'David Goodger',
          author_email = 'goodger@users.sourceforge.net',
          license = 'public domain, Python (see COPYING.txt)',
          packages = ['docutils', 'docutils.readers', 'docutils.writers',
                      'docutils.transforms', 'docutils.languages', 
                      'docutils.parsers', 'docutils.parsers.rst',
                      'docutils.parsers.rst.directives',
                      'docutils.parsers.rst.languages'])
    return dist

if __name__ == '__main__' :
    do_setup()
