#!/usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

A minimal front-end to the Docutils Publisher, producing pseudo-XML.
"""

from docutils.core import publish


usage = '%prog [options] [source [destination]]'
description = ('Generate pseudo-XML from standalone reStructuredText sources '
               '(for testing purposes).')

publish(usage=usage, description=description)
