###############################################################################
###############################################################################
# Import

import os.path

try:
    from lxml import etree
except ImportError:
    raise Exception("""
Python package 'lxml' is not available.
You may try to use 'xml2rst.xsl' with a standalone XSLT processor like 'xalan' or 'xsltproc'""")

###############################################################################
###############################################################################
# Constants

"""
@var MainXsltNm: Name of the main XSLT source file
@type MainXsltNm: str
"""
MainXsltNm = "xml2rst.xsl"

###############################################################################
###############################################################################
# Specialized functions

def convert(inNm, outNm, settings):
    """
    Do the conversion.

    @param inNm: Filename of input file.
    @type inNm: str

    @param outNm: Filename of output file or None.
    @type outNm: str | None
    """
    try:
        inF = open(inNm)
    except IOError, e:
        raise Exception("Can't open input file %r: %s" % ( inNm, e, ))

    modP = os.path.dirname(__file__)
    mainXsltNm = os.path.join(modP, MainXsltNm)
    try:
        mainXsltF = open(mainXsltNm)
    except IOError, e:
        raise Exception("Can't open main XSLT file %r: %s" % ( mainXsltNm, e, ))

    xsltParser = etree.XMLParser()
    mainXsltDoc = etree.parse(mainXsltF, xsltParser)
    mainXsltF.close()
    mainXslt = etree.XSLT(mainXsltDoc)

    inParser = etree.XMLParser()
    try:
        inDoc = etree.parse(inF, inParser)
    except Exception, e:
        raise Exception("Error parsing input file %r: %s" % ( inNm, e, ))
    inF.close()

    xsltParams = { }
    if settings.fold is not None:
        xsltParams['fold'] = str(settings.fold)
    if settings.adornment is not None:
        xsltParams['adornment'] = "'" + settings.adornment + "'"
    try:
        result = mainXslt(inDoc, **xsltParams)
    except Exception, e:
        raise Exception("Error transforming input file %r: %s" % ( inNm, e, ))
    # Chop off trailing linefeed - added somehow
    outS = str(result)[:-1]
    if outNm:
        try:
            outF = open(outNm, "w")
        except IOError, e:
            raise Exception("Can't open output file %r: %s" % ( outNm, e, ))
        outF.write(outS)
        outF.close()
    else:
        print(outS)
