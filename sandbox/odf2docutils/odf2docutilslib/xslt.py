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
Do conversion by using an XSLT script.
"""

__docformat__ = 'reStructuredText' # Formatted to be rendered by epydoc

###############################################################################
###############################################################################
# Import

import os.path

from docutils_xml.parsers.xslt import XsltParser
from docutils_xml.writers.xslt import XsltWriter

###############################################################################
###############################################################################
# Constants

###############################################################################
###############################################################################
# Variables

###############################################################################
###############################################################################
# Functions

###############################################################################
###############################################################################
# Classes

class Parser(XsltParser):
    """
    Parse the input file and translate it to the output file by using XSLT.
    """

    MainXsltNm = 'odf2docutils.xsl'
    """
    :type: str

    Name of the main XSLT source file.
    """

    def __init__(self):
        modP = os.path.dirname(__file__)
        xsltPath = os.path.join(modP, self.MainXsltNm)
        try:
            xsltF = open(xsltPath)
        except IOError, e:
            raise Exception("Can't open main XSLT file %r: %s"
                            % ( xsltPath, e, ))
        XsltParser.__init__(self, xsltF)

###############################################################################

class Writer(XsltWriter):
    """
    Writer transparently writing the result from the parser.
    """

    supported = ( 'xml', ) 
    """Formats this writer supports.""" 
