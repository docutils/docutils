#!/usr/bin/env python

# Author: Lea Wiemann
# Contact: LeWiemann@gmail.com
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
A minimal front end to the Docutils Publisher, producing PythonPoint XML.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description


description = ('Generates PythonPoint documents from standalone reStructuredText '
               'sources.  ' + default_description)

publish_cmdline(writer_name='pythonpoint', description=description)
