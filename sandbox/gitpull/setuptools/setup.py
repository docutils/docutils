#!/usr/bin/env python
# $Id$
# Copyright: This file has been placed in the public domain.

import sys
import os
import glob

try:
    from setuptools import setup, find_packages
except ImportError:
    import ez_setup
    ez_setup.use_setuptools()
    from setuptools import setup, find_packages


"""Distutils setup parameters."""

classifiers = [
    'Development Status :: 4 - Beta',
    'Environment :: Console',
    'Intended Audience :: End Users/Desktop',
    'Intended Audience :: Other Audience',
    'Intended Audience :: Developers',
    'Intended Audience :: System Administrators',
    'License :: Public Domain',
    'License :: OSI Approved :: Python Software Foundation License',
    'License :: OSI Approved :: BSD License',
    'License :: OSI Approved :: GNU General Public License (GPL)',
    'Operating System :: OS Independent',
    'Programming Language :: Python :: 2.4',
    'Programming Language :: Python :: 2.5',
    'Programming Language :: Python :: 2.6',
    'Programming Language :: Python :: 2.7',
    'Programming Language :: Python :: 3',
    'Topic :: Documentation',
    'Topic :: Software Development :: Documentation',
    'Topic :: Text Processing',
    'Natural Language :: English',      # main/default language, keep first
    'Natural Language :: Afrikaans',
    'Natural Language :: Catalan',
    'Natural Language :: Chinese (Simplified)',
    'Natural Language :: Chinese (Traditional)',
    'Natural Language :: Czech',
    'Natural Language :: Dutch',
    'Natural Language :: Esperanto',
    'Natural Language :: Finnish',
    'Natural Language :: French',
    'Natural Language :: Galician',
    'Natural Language :: German',
    'Natural Language :: Italian',
    'Natural Language :: Japanese',
    'Natural Language :: Polish',
    'Natural Language :: Portuguese (Brazilian)',
    'Natural Language :: Russian',
    'Natural Language :: Slovak',
    'Natural Language :: Spanish',
    'Natural Language :: Swedish',
    ]

# BUG pypi did not like fllowing languages
#   'Natural Language :: Lithuanian',
"""Trove classifiers for the Distutils "register" command."""

setup(
    name='docutils',
    classifiers=classifiers,
    use_2to3=True,
    version='0.13',
    description='Docutils -- Python Documentation Utilities',
    long_description="""\
Docutils is a modular system for processing documentation
into useful formats, such as HTML, XML, and LaTeX.  For
input Docutils supports reStructuredText, an easy-to-read,
what-you-see-is-what-you-get plaintext markup syntax.""",  # wrap at col 60
    url='http://docutils.sourceforge.net/',
    maintainer='docutils-develop list',
    maintainer_email='docutils-develop@lists.sourceforge.net',
    license='public domain, Python, 2-Clause BSD, GPL 3 (see COPYING.txt)',
    platforms='OS-independent',
    package_dir={
        'docutils': 'docutils',
        'docutils.tools': 'tools'
    },
    include_package_data=True,
    scripts=[
        'tools/rst2html.py',
        'tools/rst2html5.py',
        'tools/rst2s5.py',
        'tools/rst2latex.py',
        'tools/rst2xetex.py',
        'tools/rst2man.py',
        'tools/rst2xml.py',
        'tools/rst2pseudoxml.py',
        'tools/rstpep2html.py',
        'tools/rst2odt.py',
        'tools/rst2odt_prepstyles.py',
    ],
)
