#!/usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

A minimal front end to the Docutils Publisher, producing Docutils XML.
"""

import locale
locale.setlocale(locale.LC_ALL, '')

from docutils.core import publish, default_description


description = ('Generates Docutils-native XML from standalone '
               'reStructuredText sources.  ' + default_description)

publish(writer_name='xml', description=description)
