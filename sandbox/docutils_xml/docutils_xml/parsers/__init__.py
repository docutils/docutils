# Copyright (C) 2013 Stefan Merten

# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License,
# or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

"""
Module definitions.
"""

__docformat__ = 'reStructuredText' # Formatted to be rendered by epydoc

###############################################################################
###############################################################################
# Import

import re

###############################################################################
###############################################################################
# Constants

###############################################################################
###############################################################################
# Variables

###############################################################################
###############################################################################
# Functions

def encodeForXmlParser(s):
    """
    `lxml.etree` XML parser does not accept unicode strings with encoding
    attributes. This function normalizes such strings according to the encoding
    attribute found.

    :Parameters:

      s : basestring
        The string to normalize.

    :rtype: basestring
    :return: The normalized string suitable to be given to the XML parser.

    :Exceptions:

      * `LookupError`: If `s` contains an encoding unknown to Python.

      * `UnicodeError`: If `s` contains characters which are not encodable with
        the encoding given.
    """
    if not isinstance(s, unicode):
        # A non-unicode string is already encoded
        return s
    line1 = s.splitlines()[0]
    mtc = re.search("<?xml.*encoding\\s*=\\s*[\"']([-.\w]+)[\"']", line1)
    if not mtc:
        # No encoding found - then unicode is fine
        return s
    enc = mtc.group(1)
    try:
        return s.encode(enc)
    except LookupError:
        raise LookupError("Encoding of XML input %r unknown to Python"
                          % ( enc, ))
    except UnicodeError, e:
        raise UnicodeError("Encoding of XML input given as %r but there "
                           "are invalid characters while reencoding: %s"
                           % ( enc, e ))

###############################################################################
###############################################################################
# Classes
