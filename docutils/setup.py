#!/usr/bin/env python
# $Id$
# Copyright: This file has been placed in the public domain.

from __future__ import print_function

import glob
import os
import sys

try:
    from setuptools import setup
except ImportError:
    print('Error: The "setuptools" module, which is required for the')
    print('installation of Docutils, could not be found.  You may need to ')
    print('install a package called "python-setuptools" (or similar) on your ')
    print('system using your package manager.')
    sys.exit(1)


package_data = {
    'name': 'docutils',
    'description': 'Docutils -- Python Documentation Utilities',
    'long_description': """\
Docutils is a modular system for processing documentation
into useful formats, such as HTML, XML, and LaTeX.  For
input Docutils supports reStructuredText, an easy-to-read,
what-you-see-is-what-you-get plaintext markup syntax.""",  # wrap at col 60
    'url': 'http://docutils.sourceforge.net/',
    'version': '0.17b.dev',
    'author': 'David Goodger',
    'author_email': 'goodger@python.org',
    'maintainer': 'docutils-develop list',
    'maintainer_email': 'docutils-develop@lists.sourceforge.net',
    'license': 'public domain, Python, 2-Clause BSD, GPL 3 (see COPYING.txt)',
    'platforms': 'OS-independent',
    'python_requires': '>=2.7, !=3.0.*, !=3.1.*, !=3.2.*, !=3.3.*, !=3.4.*',
    'include_package_data': True,
    'exclude_package_data': {"": ["docutils.conf"]},                
    'package_dir': {
        'docutils': 'docutils',
        'docutils.tools': 'tools'
    },
    'packages': [
        'docutils',
        'docutils.languages',
        'docutils.parsers',
        'docutils.parsers.rst',
        'docutils.parsers.rst.directives',
        'docutils.parsers.rst.languages',
        'docutils.readers',
        'docutils.transforms',
        'docutils.utils',
        'docutils.utils.math',
        'docutils.writers',
        'docutils.writers.html4css1',
        'docutils.writers.html5_polyglot',
        'docutils.writers.pep_html',
        'docutils.writers.s5_html',
        'docutils.writers.latex2e',
        'docutils.writers.xetex',
        'docutils.writers.odf_odt',
    ],
    'scripts': [
        'tools/rst2html.py',
        'tools/rst2html4.py',
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
    'classifiers': [
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
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Topic :: Documentation',
        'Topic :: Software Development :: Documentation',
        'Topic :: Text Processing',
        'Natural Language :: English',  # main/default language, keep first
        'Natural Language :: Afrikaans',
        'Natural Language :: Catalan',
        'Natural Language :: Chinese (Simplified)',
        'Natural Language :: Chinese (Traditional)',
        'Natural Language :: Czech',
        'Natural Language :: Danish',
        'Natural Language :: Dutch',
        'Natural Language :: Esperanto',
        'Natural Language :: Finnish',
        'Natural Language :: French',
        'Natural Language :: Galician',
        'Natural Language :: German',
        'Natural Language :: Hebrew',
        'Natural Language :: Italian',
        'Natural Language :: Japanese',
        'Natural Language :: Korean',
        'Natural Language :: Latvian',
        'Natural Language :: Lithuanian',
        'Natural Language :: Persian',
        'Natural Language :: Polish',
        'Natural Language :: Portuguese (Brazilian)',
        'Natural Language :: Russian',
        'Natural Language :: Slovak',
        'Natural Language :: Spanish',
        'Natural Language :: Swedish',
    ],
}
"""Distutils setup parameters."""


def do_setup():
    # Install data files properly.
    dist = setup(**package_data)
    return dist


if __name__ == '__main__':
    do_setup()
