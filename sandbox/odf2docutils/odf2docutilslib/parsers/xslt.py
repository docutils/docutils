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
Parse and produce result string by XSLT.
"""

__docformat__ = 'reStructuredText' # Formatted to be rendered by epydoc

###############################################################################
###############################################################################
# Import

import docutils.parsers

from lxml import etree

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

class XsltParser(docutils.parsers.Parser):
    """
    Parses XML input by XSLT and stores the result in the attribute
    `xslt_result` of the document. Works together with `XsltWriter`.
    """

    def __init__(self, xsltPath):
        """
        See instance variables for parameter documentation.
        """
        self.xsltPath = xsltPath
        """
        Path to the XSLT to use.
        """
        self.xslt = None
        """
        :type: Return type of `etree.XSLT`()

        The XSLT to use for parsing.
        """

        # Find XSLT
        try:
            xsltF = open(self.xsltPath)
        except IOError, e:
            raise Exception("Can't open main XSLT file %r: %s"
                            % ( self.xsltPath, e, ))

        # Parse and prepare XSLT
        try:
            xsltDoc = etree.parse(xsltF)
        except Exception, e:
            raise Exception("Error parsing main XSLT file %r: %s"
                            % ( self.xsltPath, e, ))
        xsltF.close()
        self.xslt = etree.XSLT(xsltDoc)

    def parse(self, inputstring, document):
        self.setup_parse(inputstring, document)
        inDoc = etree.fromstring(inputstring)
        document.xslt_result = self.xslt(inDoc, sourceName="'%s'"
                                         % ( document.current_source, ))
        if self.xslt.error_log:
            document.reporter.error(self.xslt.error_log)
        self.finish_parse()
