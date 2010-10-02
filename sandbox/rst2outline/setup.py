#!/usr/bin/env python

from distutils.core import setup

from version import version

setup(name='rst2outline',
      version=version,
      description='reStructuredText writer for tabbed outline format usable by mS Powerpoint',
      author='Stefan Merten',
      author_email='smerten@oekonux.de',
      url='http://docutils.sourceforge.net/sandbox/rst2outline/',
      license='GPL 2',
      requires=[ 'docutils' ],
      scripts=[ 'rst2outline.py' ],
     )
