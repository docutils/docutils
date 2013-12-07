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

from docutils_xml.parsers import encodeForXmlParser

import math

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

class XPathExtension(object):
    """
    Abstract base class for XPath extension functions. All public methods
    defined in a derived class are extension functions which can be called from
    XSLT.

    Methods to be used as extension functions need to accept an fixed first
    parameter (`lxml.etree._XSLTContext`). This is the evaluation context.

    Apart from that any number of parameters may be given according to the
    signature of the XPath function.
    """

    namespace = None
    """
    :type: str

    Namespace for these XSLT extension functions defined by this class.

    Override this in derived classes.
    """

    def _stringParameter(self, string):
        """
        :return: The normalized string parameter from an XPath parameter.
        :rtype: unicode

        :Parameters:

          string : `lxml.etree._ElementStringResult` | [ `lxml.etree._ElementStringResult`, ] | [ `lxml.etree._Element`, ] | [ ]
            Original XPath string parameter.

        :Exceptions:

          * `TypeError`: When a type is given which is incompatible with an
            XPath string.
        """
        if isinstance(string, list):
            if len(string) == 0:
                # Happens for forced but non-existing elements like `text()` in
                # an empty element
                return u""
            elif len(string) == 1:
                string = string[0]
            else:
                raise TypeError("Encountered an XPath string parameter with more than one (%d) element" % ( len(string), ))

        if isinstance(string, basestring):
            return unicode(string)
        else:
            raise TypeError("Encountered invalid type for XPath string parameter: %s"
                % type(string) )

    def _boolParameter(self, boolean):
        """
        :return: The normalized bool parameter from an XPath parameter.
        :rtype: bool

        :Parameters:

          boolean : bool
            Original XPath bool parameter.

        :Exceptions:

          * `TypeError`: When a type is given which is incompatible with a
            boolean.
        """
        if isinstance(boolean, bool):
            return boolean
        raise TypeError("Encountered invalid type for XPath boolean parameter: %s"
                        % type(boolean) )

    def _floatParameter(self, number):
        """
        :return: The normalized float parameter from an XPath parameter.
        :rtype: float

        :Parameters:

          number : float
            Original XPath number parameter.

        :Exceptions:

          * `TypeError`: When a type is given which is incompatible with an
            float.
        """
        if isinstance(number, float):
            
            return number
        raise TypeError("Encountered invalid type for XPath float parameter: %s"
                        % type(number) )

    def _intParameter(self, number):
        """
        :return: The normalized int parameter from an XPath parameter.
        :rtype: int

        :Parameters:

          number : int
            Original XPath number parameter.

        :Exceptions:

          * `TypeError`: When a type is given which is incompatible with an
            int.

          * `ValueError`: When the number has a non-trivial fractional part.
        """
        if isinstance(number, float):
            ( fraction, integer ) = math.modf(number)
            if fraction == 0:
                return int(number)
            raise ValueError("XPath number (%f) has non-trivial fractiona part"
                             % ( number, ))
        raise TypeError("Encountered invalid type for XPath int parameter: %s"
                        % type(number) )

###############################################################################

class XsltParser(docutils.parsers.Parser):
    """
    Parses XML input by XSLT and stores the result in the attribute
    `xslt_result` of the document. Works together with `XsltWriter`.
    """

    # TODO Accept additional XSLT sheets to create a transformation pipeline

    def __init__(self, xsltSource, extension=None):
        """
        :Parameters:

           xsltSource : file-like object
             The source containing the XSLT. This is an open file-like object.

             The XSLT program receives settings from Docutils as parameters.
             All settings whose value is a string, integer, float or boolean
             are transferred. This allows settings for the specific XSLT
             program. Please note that a string value may contain either ``'``
             or ``"`` but not both.

           extension : None | XPathExtension
             The extensions to use or None for no extensions.
        """
        self.xslt = None
        """
        :type: Return type of `etree.XSLT`\ ()

        The XSLT to use for parsing.
        """

        try:
            xsltDoc = etree.parse(xsltSource)
        except Exception, e:
            raise Exception("Error parsing XSLT: %s" % ( e, ))
        xsltSource.close()
        if extension is None:
            exts = None
        else:
            exts = etree.Extension(extension, ns=extension.namespace)
        self.xslt = etree.XSLT(xsltDoc, extensions=exts)

    def parse(self, inputstring, document):
        self.setup_parse(inputstring, document)
        settings = document.settings.copy()
        settings._source_name = document.current_source
        xsltParams = { }
        for ( key, val ) in settings.__dict__.items():
            if isinstance(val, bool):
                fmt = '%s'
                if val:
                    val = 'true()'
                else:
                    val = 'false()'
            elif isinstance(val, ( int, long )):
                fmt = '%d'
            elif isinstance(val, float):
                fmt = '%G'
            elif isinstance(val, basestring):
                val = unicode(val)
                if "'" in val:
                    if '"' in val:
                        raise ValueError("Can not use string containing single and double quote as XSLT parameter ('%r')"
                                         % ( val, ))
                    fmt = '"%s"'
                else:
                    fmt = "'%s'"
            else:
                continue
            xsltParams[key] = fmt % ( val, )
        inDoc = etree.fromstring(encodeForXmlParser(inputstring))
        document.xslt_result = self.xslt(inDoc, **xsltParams)
        if self.xslt.error_log:
            document.reporter.error(self.xslt.error_log)
        self.finish_parse()
