# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Author: Chris Liechti <cliechti@gmx.net>
# Copyright: This file has been placed in the public domain.

from setuptools import setup, find_packages
setup(
    name = 'docutils-aafigure',
    version = '0.2',
    description = "ASCII art figures for reStructuredText",
    long_description = """\
This module provides:

- a plugin for docutils (>= 0.5) -> aafigure directive
- a command line application docutils-aafigure

reST example::

    .. aafigure::

            +-----+   ^
            |     |   |
        --->+     +---o--->
            |     |   |
            +-----+   V

Command line example::

  docutils-aafigure test.txt -t svg -o test.svg

Please see README.txt for examples.
""",
    author = 'Chris Liechti',
    author_email = 'cliechti@gmx.net',
    install_requires = 'docutils>=0.5',
    classifiers = [
        'Development Status :: 4 - Beta',
        'Environment :: Console',
        'Environment :: Web Environment',
        'Intended Audience :: Developers',
        #~ 'License :: OSI Approved :: BSD License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Topic :: Documentation',
        'Topic :: Utilities',
    ],
    platforms = 'any',
    packages = find_packages(),
    entry_points = {
        'docutils.parsers.rst.directives': [
            'aafigure = aafigure.aafigure_directive:AAFigureDirective'
        ],
        'console_scripts': [
            'docutils-aafigure = aafigure.aafigure:main',
        ],
    },

)
