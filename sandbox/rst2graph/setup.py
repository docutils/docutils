#!/usr/bin/env python

from distutils.core import setup

from version import version

setup(name='rst2graph',
      version=version,
      description='reStructuredText writer transforming a reStructuredText file to a graph of its link structure',
      author='Stefan Merten',
      author_email='smerten@oekonux.de',
      url='http://docutils.sourceforge.net/sandbox/rst2graph/',
      license='GPL 2',
      requires=[ 'docutils' ],
      scripts=[ 'rst2gxl.py', 'rst2dot.py', 'rst2gv.py', 'rst2graph.py' ],
     )
