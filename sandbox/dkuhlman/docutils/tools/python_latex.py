#!/usr/bin/env python

"""
:Author: Dave Kuhlman
:Contact: dkuhlman@rexx.com
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

A minimal front end to the Docutils Publisher, producing LaTeX
that conforms to Documenting Python 
(http://www.python.org/dev/doc/devel/doc/doc.html).
"""

#import locale
#locale.setlocale(locale.LC_ALL, '')

from docutils.core import publish_cmdline, default_description


description = ('Generates LaTeX for "Documenting Python" documents '
    'from standalone reStructuredText '
    'sources.  ' + default_description)

publish_cmdline(writer_name='python_latex', description=description)
