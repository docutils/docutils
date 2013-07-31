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
    try:
        result = mainXslt(inDoc)
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
