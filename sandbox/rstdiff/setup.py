#!/usr/bin/env python

from distutils.core import setup

from version import version

setup(name='rstdiff',
      version=version,
      description='Tool for creating a diff highlighting changes from two reStructuredText input files',
      author='Stefan Merten',
      author_email='smerten@oekonux.de',
      url='http://docutils.sourceforge.net/sandbox/rstdiff/',
      license='GPL 2',
      requires=[ 'docutils' ],
      scripts=[ 'rstdiff.py' ],
      packages=[ 'treediff' ],
     )
