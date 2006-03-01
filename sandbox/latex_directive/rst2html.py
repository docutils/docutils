#!/usr/bin/env python

# Author: John Gill
# Contact: swfiua@gmail.com
# Revision: $$
# Date: $ $
# Copyright: This module has been placed in the public domain.

"""
A minimal front end to the Docutils Publisher, producing HTML.

Extended to add support for a latex directive.

Latex code is processed by latex and turned into a png.
"""

import locale
try:
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, Publisher, default_description

import latex_directive
latex_directive.register()  # Enable the ABC directive

description = ('Generates (X)HTML documents from standalone reStructuredText '
               'sources.  ' + default_description)

publish_cmdline(writer_name='html', description=description)
