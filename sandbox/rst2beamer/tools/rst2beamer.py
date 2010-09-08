#!/usr/bin/python

# Author: Stefan Merten
# Contact:
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
A minimal front end to the Docutils Publisher, producing LaTeX Beamer.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description

from docutils.writers.latexbeamer import Writer, Reader

description = ('Generates LaTeX Beamer documents from standalone reStructuredText '
               'sources.  ' + default_description)

writer = Writer()
reader = Reader()
publish_cmdline(reader=reader, writer=writer, description=description)
