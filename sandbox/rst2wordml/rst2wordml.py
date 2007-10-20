#!C:\dev\Python25\python.exe

# Author: Jay Kint
# Contact: bilbo@hobbit-hole.org
# Revision: $Revision$
# Date: $Date$
# Copyright: This module is copyright 2007 by Jay Kint, licensed by the BSD License.

"""
A minimal front end to the Docutils Publisher, producing WordML for Microsoft Word 2003+.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils.core import publish_cmdline, default_description


description = ('Generates WordML from standalone '
               'reStructuredText sources.  ' + default_description)

publish_cmdline(writer_name='docutils_wordml', description=description)
