#!/usr/bin/env python3
# $Id$
# Copyright: This file has been placed in the public domain.

import sys

try:
    from setuptools import setup
except ImportError:
    print("""\
Error: The "setuptools" module, which is required for the
  installation of Docutils, could not be found.

  You may install  it with `python -m pip install setuptools`
  or from a package called "python-setuptools" (or similar)
  using your system\'s package manager.

  Alternatively, install a release from PyPi with
  `python -m pip install docutils`.'

  If all this fails, try a "manual install".
  https://docutils.sourceforge.io/docs/dev/repository.html#install-manually
""")
    sys.exit(1)


package_data = {
    'name': 'docutils',
    'description': 'Docutils -- Python Documentation Utilities',
    'long_description': """\
Docutils is a modular system for processing documentation
into useful formats, such as HTML, XML, and LaTeX.  For
input Docutils supports reStructuredText, an easy-to-read,
what-you-see-is-what-you-get plaintext markup syntax.""",  # wrap at col 60
    'url': 'https://docutils.sourceforge.io/',
    'version': '0.20b.dev',
    'author': 'David Goodger',
    'author_email': 'goodger@python.org',
    'maintainer': 'docutils-develop list',
    'maintainer_email': 'docutils-develop@lists.sourceforge.net',
    'license': 'public domain, Python, 2-Clause BSD, GPL 3 (see COPYING.txt)',
    'platforms': 'OS-independent',
    'python_requires': '>=3.7',
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
        'docutils.parsers.rst.include',
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
        'docutils.writers.s5_html.themes',
        'docutils.writers.s5_html.themes.default',
        'docutils.writers.latex2e',
        'docutils.writers.xetex',
        'docutils.writers.odf_odt',
    ],
    'entry_points': {
        'console_scripts': ['docutils=docutils.__main__:main']
    },
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
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Topic :: Documentation',
        'Topic :: Software Development :: Documentation',
        'Topic :: Text Processing',
        'Natural Language :: English',  # main/default language, keep first
        'Natural Language :: Afrikaans',
        'Natural Language :: Arabic',
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
        'Natural Language :: Ukrainian',
    ],
}
"""Distutils setup parameters."""


def do_setup():
    # Install data files properly.
    return setup(**package_data)


if __name__ == '__main__':
    do_setup()
