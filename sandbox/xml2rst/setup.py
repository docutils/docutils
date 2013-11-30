#!/usr/bin/env python

from distutils.core import setup

from version import version

setup(name='xml2rst',
      version=version,
      description='A reStructuredText writer based on Docutils XML',
      author='Stefan Merten',
      author_email='smerten@oekonux.de',
      url='http://docutils.sourceforge.net/sandbox/xml2rst/',
      license='GPL 2',
      requires=[ 'docutils', 'lxml', 'docutils_xml', ],
      scripts=[ 'xml2rst.py' ],
      packages=[ 'xml2rstlib' ],
      package_data={ 'xml2rstlib': [ 'xml2rst.xsl',
                                     'xml2rst-nopy.xsl',
                                     'xml2rst-noexslt.xsl' ], },
     )
