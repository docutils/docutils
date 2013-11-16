#!/usr/bin/env python

from distutils.core import setup

from version import version

setup(name='docutils_xml',
      version=version,
      description='Docutils support for XML based processing',
      author='Stefan Merten',
      author_email='smerten@oekonux.de',
      url='http://docutils.sourceforge.net/sandbox/docutils_xml/',
      license='GPL 2',
      requires=[ 'lxml' ],
      packages=[ 'docutils_xml',
                 'docutils_xml/parsers', 'docutils_xml/writers' ],
     )
