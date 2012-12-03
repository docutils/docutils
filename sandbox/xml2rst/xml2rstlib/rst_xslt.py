"""
Glue code for XSLT and Python based conversion.
"""

###############################################################################
###############################################################################
# Import

import os.path
import sys

try:
    from lxml import etree
except ImportError:
    raise Exception("""
Python package 'lxml' is not available.
You may try to use an older version of 'xml2rst.xsl' with a standalone
XSLT processor like 'xalan' or 'xsltproc'""")

from xml2rstlib import markup

__docformat__ = 'reStructuredText'

###############################################################################
###############################################################################
# Constants

MainXsltNm = 'xml2rst.xsl'
"""
`MainXsltNm`: ``str``
   Name of the main XSLT source file.
"""

###############################################################################
###############################################################################
# Classes

class XPathExtension():
    """
    Abstract class for XPath extension functions.
    """

    namespace = "http://www.merten-home.de/xml2rst"
    """
    `namespace`: ``str``
      Namespace for these XSLT extension functions.
    """

    def _stringParameter(self, string):
        """
        Return the normalized string parameter from an XPath
        parameter.

        `string`: ``lxml.etree._ElementStringResult`` | ``[ lxml.etree._ElementStringResult, ]`` | ``[ ]``
          Original XPath string parameter.
        """
        if isinstance(string, list):
            if len(string) == 0:
                return ""
            else:
                assert len(string) == 1, "Encountered an XPath string parameter with more than one element"
                return string[0]
        else:
            return string

    def _boolParameter(self, boolean):
        """
        Return the normalized bool parameter from an XPath parameter.

        `boolean`: ``bool``
          Original XPath bool parameter.
        """
        return boolean

###############################################################################

class RstText(XPathExtension):
    """
    XPath extension functions for computing valid reStructuredText
    markup for plain text.
    """

    def plain(self, context, string, indent, literal):
        """
        Output a plain text preventing further interpretation by
        reStructuredText. Text may contain linefeeds.

        `context`: ``lxml.etree._XSLTContext``
          The evaluation context.

          `context.context_node`: ``Element``
            The context node.

          `contect.eval_context`: ``dict``
            A dictionary to store state.

        `string`:
          The (smart) string to turn into output text.

        `indent`:
          The (smart) string to use for indent in case of internal
          linefeeds.

        `literal`:
          The (smart) string to use for indent in case of internal
          linefeeds.
        """
        return markup.Text.plain(self._stringParameter(string),
                                 self._stringParameter(indent),
                                 self._boolParameter(literal))

    # indent
    # directive
    # field_names
    # substitution
    # inline markup
    # token
    # label
    # start_delimiter
    # end_delimiter
    # target_definition

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
    try:
        inF = open(inNm)
    except IOError, e:
        raise Exception("Can't open input file %r: %s" % ( inNm, e, ))

    # Find XSLT
    modP = os.path.dirname(__file__)
    mainXsltNm = os.path.join(modP, MainXsltNm)
    try:
        mainXsltF = open(mainXsltNm)
    except IOError, e:
        raise Exception("Can't open main XSLT file %r: %s" % ( mainXsltNm, e, ))

    # Parse and prepare XSLT and extensions
    xsltParser = etree.XMLParser()
    try:
        mainXsltDoc = etree.parse(mainXsltF, xsltParser)
    except Exception, e:
        raise Exception("Error parsing main XSLT file %r: %s"
                        % ( mainXsltNm, e, ))
    mainXsltF.close()

    rstText = RstText()
    extensions = etree.Extension(rstText, ns=rstText.namespace)
    mainXslt = etree.XSLT(mainXsltDoc, extensions=extensions)

    # Parse input file
    inParser = etree.XMLParser()
    try:
        inDoc = etree.parse(inF, inParser)
    except Exception, e:
        raise Exception("Error parsing input file %r: %s" % ( inNm, e, ))
    inF.close()

    # Process input
    xsltParams = { }
    if settings.fold is not None:
        xsltParams['fold'] = str(settings.fold)
    if settings.adornment is not None:
        xsltParams['adornment'] = "'" + settings.adornment + "'"
    try:
        result = mainXslt(inDoc, **xsltParams)
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
