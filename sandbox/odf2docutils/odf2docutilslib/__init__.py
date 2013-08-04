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
Glue code for XSLT and Python based conversion.
"""

###############################################################################
###############################################################################
# Import

import os.path
import sys

import zipfile

try:
    from lxml import etree
except ImportError:
    raise Exception("""
Python package 'lxml' is not available.
You may try to use a standalone XSLT processor like 'xalan' or 'xsltproc'""")

__docformat__ = 'reStructuredText'

###############################################################################
###############################################################################
# Constants

MainXsltNm = 'odf2docutils.xsl'
"""
`MainXsltNm`: ``str``
   Name of the main XSLT source file.
"""

ContentXmlNm = 'content.xml'
"""
`ContentXmlNm`: ``str``
  Name of the file containing the document content in the ODF ZIP file.
"""

###############################################################################
###############################################################################
# Classes

###############################################################################
###############################################################################
# Specialized functions

def convert(inNm, outNm, settings):
    """
    Do the conversion.

    `inNm`: ``str``
      Filename of input file.

    `outNm`: ``str`` | None
      Filename of output file or None for stdout.

    `settings`: ``optparse.Values``
      Options from command line.

    return: ``str`` | None
      The log created by XSLT or None if the log could not be get.
    """
    # Find XSLT
    modP = os.path.dirname(__file__)
    mainXsltNm = os.path.join(modP, MainXsltNm)
    try:
        mainXsltF = open(mainXsltNm)
    except IOError, e:
        raise Exception("Can't open main XSLT file %r: %s" % ( mainXsltNm, e, ))

    # Parse and prepare XSLT
    xsltParser = etree.XMLParser()
    try:
        mainXsltDoc = etree.parse(mainXsltF, xsltParser)
    except Exception, e:
        raise Exception("Error parsing main XSLT file %r: %s"
                        % ( mainXsltNm, e, ))
    mainXsltF.close()

    mainXslt = etree.XSLT(mainXsltDoc)

    # Parse input file
    if not zipfile.is_zipfile(inNm):
        raise Exception("%r is not a valid ZIP input file" % ( inNm, ))
    try:
        inZip = zipfile.ZipFile(inNm)
        inXml = inZip.open(ContentXmlNm)
    except Exception, e:
        raise Exception("Error opening content of input file %r: %s"
                        % ( inNm, e, ))

    inParser = etree.XMLParser()
    try:
        inDoc = etree.parse(inXml, inParser)
    except Exception, e:
        raise Exception("Error parsing content of input file %r: %s"
                        % ( inNm, e, ))
    inXml.close()

    # Process input
    log = None
    try:
        result = mainXslt(inDoc)
        log = str(mainXslt.error_log)
    except Exception, e:
        raise Exception("Error transforming input file %r: %s" % ( inNm, e, ))
    outS = str(result)
    if outNm:
        try:
            outF = open(outNm, "w")
        except IOError, e:
            raise Exception("Can't open output file %r: %s" % ( outNm, e, ))
        outF.write(outS)
        outF.close()
    else:
        sys.stdout.write(outS)

    return log
