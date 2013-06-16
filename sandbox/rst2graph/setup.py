#!/usr/bin/env python

from distutils.core import setup

from version import version

setup(name='rst2gxl',
      version=version,
      description='reStructuredText writer for GXL format for visualizing structures of a text using dot',
      author='Stefan Merten',
      author_email='smerten@oekonux.de',
      url='http://docutils.sourceforge.net/sandbox/rst2gxl/',
      license='GPL 2',
      requires=[ 'docutils' ],
      scripts=[ 'rst2gxl.py' ],
     )
