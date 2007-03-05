# $Id$
# Author: Felix Wiemann <Felix.Wiemann@ososo.de>
# Copyright: This file has been placed in the public domain.

from setuptools import setup, find_packages
setup(
    name = 'docutils-aafigure',
    version = '0.1',
    description = 'ASCII art figures for reStructuredText',
    author = 'Chris Liechti',
    author_email = '<cliechti@gmx.net>',
    install_requires = 'docutils>=0.5',
    packages = find_packages(),
    entry_points = {'docutils.parsers.rst.directives': [
    'aafigure = aafigure.aafigure_directive:AAFigureDirective']},
)
