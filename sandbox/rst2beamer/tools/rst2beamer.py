#!/usr/bin/python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
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


description = ('Generates LaTeX Beamer documents from standalone reStructuredText '
               'sources.  ' + default_description)

publish_cmdline(writer_name='latexbeamer', description=description)
