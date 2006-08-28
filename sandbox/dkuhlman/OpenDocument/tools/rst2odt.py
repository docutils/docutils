#!/usr/local/bin/python

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
A minimal front end to the Docutils Publisher, producing Docutils XML.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description
#from docutils.writers.docutils_xml import Writer
#from docutils.writers.html4css1 import Writer
#from OOwriter import Writer
from docutils.writers.odtwriter import Writer


description = ('Generates Docutils-native XML from standalone '
               'reStructuredText sources.  ' + default_description)

#publish_cmdline(writer_name='xml', description=description)

writer = Writer()
output = publish_cmdline(writer=writer, description=description)


