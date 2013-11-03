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

import docutils.parsers, docutils.writers

import lxml.etree

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

class Parser(docutils.parsers.Parser):
    """
    Parse the input file and translate it to the output file by using XSLT.
    """

    MainXsltNm = 'odf2docutils.xsl'
    """
    :type: str

    Name of the main XSLT source file.
    """

    def parse(self, inputstring, document):
        # Find XSLT
        modP = os.path.dirname(__file__)
        mainXsltNm = os.path.join(modP, self.MainXsltNm)
        try:
            mainXsltF = open(mainXsltNm)
        except IOError, e:
            raise Exception("Can't open main XSLT file %r: %s"
                            % ( mainXsltNm, e, ))

        # Parse and prepare XSLT
        try:
            mainXsltDoc = lxml.etree.parse(mainXsltF)
        except Exception, e:
            raise Exception("Error parsing main XSLT file %r: %s"
                            % ( mainXsltNm, e, ))
        mainXsltF.close()
        mainXslt = lxml.etree.XSLT(mainXsltDoc)

        inDoc = lxml.etree.fromstring(inputstring)
        document.xslt_result = mainXslt(inDoc,
                                        sourceName="'"
                                        + document.current_source + "'")
        if mainXslt.error_log:
            document.reporter.error(mainXslt.error_log)

###############################################################################

class Writer(docutils.writers.Writer):
    """
    Writer transparently writing the result from the parser.
    """

    supported = ( 'xml', ) 
    """Formats this writer supports.""" 

    def translate(self):
        self.output = str(self.document.xslt_result)
