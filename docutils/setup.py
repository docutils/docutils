#!/usr/bin/env python
# $Id$
# Copyright: This file has been placed in the public domain.

from distutils.core import setup

def do_setup():
    dist = setup(
          name = 'docutils',
          description = 'Python Documentation Utilities',
          #long_description = '',
          url = 'http://docutils.sourceforge.net/',
          version = '0.2+',
          author = 'David Goodger',
          author_email = 'goodger@users.sourceforge.net',
          license = 'public domain, Python, BSD (see COPYING.txt)',
          packages = ['docutils', 'docutils.languages',
                      'docutils.parsers', 'docutils.parsers.rst',
                      'docutils.parsers.rst.directives',
                      'docutils.parsers.rst.languages',
                      'docutils.readers', 'docutils.readers.python',
                      'docutils.transforms',
                      'docutils.writers',])
    return dist

if __name__ == '__main__' :
    do_setup()
