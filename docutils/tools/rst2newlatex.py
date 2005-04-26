#!/usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
A minimal front end to the Docutils Publisher, producing LaTeX using
the new LaTeX writer.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description


description = ('Generates LaTeX documents from standalone reStructuredText '
               'sources. This writer is EXPERIMENTAL and should not be used '
               'in a production environment. ' + default_description)

publish_cmdline(writer_name='newlatex2e', description=description)
