#!/usr/bin/env python

from distutils.core import setup

from version import version

setup(name='odf2docutils',
      version=version,
      description='Convert Open Document Format to Docutils XML',
      author='Stefan Merten',
      author_email='smerten@oekonux.de',
      url='http://docutils.sourceforge.net/sandbox/odf2docutils/',
      license='GPL 2',
      requires=[ 'lxml', 'docutils_xml' ],
      scripts=[ 'odf2docutils.py' ],
      packages=[ 'odf2docutilslib' ],
      package_data={ 'odf2docutilslib': [ 'odf2docutils.xsl' ], },
     )
