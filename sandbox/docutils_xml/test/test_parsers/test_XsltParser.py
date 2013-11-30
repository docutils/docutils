# -*- coding: utf-8 -*-

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
Test XsltParser.
"""

import unittest
import docutils.utils, docutils.frontend
import StringIO
from lxml import etree

from __init__ import DocutilsTestSupport

from docutils_xml.parsers.xslt import XsltParser, XPathExtension
from docutils_xml.writers.xslt import XsltWriter

###############################################################################

class XsltParserTestCase(DocutilsTestSupport.ParserTestCase):
    """
    Output checker for XsltParser.
    """

    xslt = None
    """
    :type: basestring

    XSLT sheet to use for processing.

    Override this in subclasses.
    """

    extension = None
    """
    :type: None | XPathExtension

    XPath extension class to use.

    Override this in subclasses if you need XPath extension functions.
    """

    def __init__(self, *args, **kwargs):
        """
        :Parameters: See super class for arguments.

          input : unicode | ( dict, unicode )
            Either the input string to use or a tuple with a dictionary
            containing settings for this test case and the input string.

          expected : unicode | type
            Either the expected output string or an exception class. If an
            exception class is given the test case is expected to raise this
            exception.
        """
        self.parser = XsltParser(StringIO.StringIO(self.xslt), self.extension)
        """Input parser for this test case."""
        self.option_parser = docutils.frontend.OptionParser(components=(
                self.parser, ))
        DocutilsTestSupport.ParserTestCase.__init__(self, *args, **kwargs)

    def test_parser(self):
        if isinstance(self.input, ( list, tuple )):
            ( case_settings, input ) = self.input
        else:
            ( case_settings, input ) = ( { }, self.input )
        settings = self.settings.copy()
        settings.__dict__.update(self.suite_settings)
        settings.__dict__.update(case_settings)
        document = docutils.utils.new_document('test data', settings)
        if (isinstance(self.expected, type)
            and issubclass(self.expected, Exception)):
            with self.assertRaises(self.expected):
                self.parser.parse(input, document)
        else:
            self.parser.parse(input, document)
            writer = XsltWriter()
            output = writer.write(document, docutils.io.StringOutput())
            self.compare_output(input, output, self.expected)

###############################################################################
###############################################################################

class XsltParserIdentityTestCase(XsltParserTestCase):
    """
    Output checker for XsltParser with identity transformation.
    """

    xslt = u"""\
<?xml version="1.0"?>

<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0">

  <xsl:output
      method="xml"
      encoding="utf-8"/>

  <xsl:template
      match="/">
    <xsl:copy-of
        select="/"/>
  </xsl:template>

</xsl:stylesheet>
"""

###############################################################################

class XsltParserIdentityTestSuite(DocutilsTestSupport.ParserTestSuite):

    test_case_class = XsltParserIdentityTestCase

###############################################################################

identity = { }

identity['simple'] = (
    ( u"""<?xml version="1.0"?>
<rootOnly/>
""",
    """<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""" ),
    ( u"""<?xml version="1.0"?>
""",
    etree.LxmlSyntaxError, ),
    ( u"""<?xml version="1.0" encoding="utf-8"?>
<rootOnly>
""",
    etree.LxmlSyntaxError, ),
    )

identity['encoding'] = (
    ( """<?xml version="1.0"?>
<rootOnly/>
""",
    """<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""" ),
    ( """<?xml version="1.0" encoding="ascii"?>
<rootOnly/>
""",
    """<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""" ),
    ( u"""<?xml version="1.0" encoding="ascii"?>
<rootOnly/>
""",
    """<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""" ),
    ( """<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""",
    """<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""" ),
    ( u"""<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""",
    """<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""" ),
    ( """<?xml version="1.0" encoding="utf-8"?>
<root\xC3\x9Cmlaut/>
""",
    """<?xml version="1.0" encoding="utf-8"?>
<root\xC3\x9Cmlaut/>
""" ),
    ( u"""<?xml version="1.0" encoding="utf-8"?>
<rootÜmlaut/>
""",
    """<?xml version="1.0" encoding="utf-8"?>
<root\xC3\x9Cmlaut/>
""" ),
    ( u"""<?xml version="1.0" encoding="bla"?>
<rootÜmlaut/>
""",
    LookupError ),
    ( u"""<?xml version="1.0" encoding="iso-8859-1"?>
<root€mlaut/>
""",
    UnicodeError ),
    )

###############################################################################
###############################################################################

class XPathExtensionTest(XPathExtension):
    """
    XPath extension functions for test purposes.
    """

    namespace = "http://www.merten-home.de/docutils_xml"

    def parVoid(self, context):
        """
        Accept no parameters and do nothing.
        """
        pass

    def parString(self, context, s):
        """
        Accept a string parameter and return its string representation.

        :Parameters:

          s : Smart string
            The smart string.
        """
        return repr(self._stringParameter(s))

    def parBoolean(self, context, b):
        """
        Accept a boolean parameter and return its string representation.

        :Parameters:

          b : boolean
            The boolean.
        """
        return str(self._boolParameter(b))

    def parFloat(self, context, f):
        """
        Accept a float parameter and return its string representation.

        :Parameters:

          f : float
            The float.
        """
        return str(self._floatParameter(f))

    def parInt(self, context, i):
        """
        Accept an int parameter and return its string representation.

        :Parameters:

          i : int
            The int.
        """
        return str(self._intParameter(i))

###############################################################################

class XsltParserXPathExtensionTestCase(XsltParserTestCase):
    """
    Output checker for XsltParser using XPath extension functions.
    """

    xslt = u"""\
<?xml version="1.0"?>
<xsl:stylesheet
    xmlns:ext="http://www.merten-home.de/docutils_xml"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0">

  <xsl:output
      method="text"
      encoding="utf-8"/>

  <xsl:template
      name="output">
    <xsl:param
	name="value"/>
    <xsl:value-of
	select="$value"/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>

  <xsl:template
      match="void">
    <xsl:call-template
	name="output">
      <xsl:with-param
	  name="value"
	  select="ext:parVoid()"/>
    </xsl:call-template>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Constant values. -->

  <xsl:template
      match="test[@const]">
    <xsl:choose>
      <xsl:when
	  test="@type = 'string'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parString('Constant')"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="@type = 'boolean'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parBoolean(true())"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="@type = 'float'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parFloat(3.14)"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="@type = 'int'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parInt(42)"/>
	</xsl:call-template>
      </xsl:when>
    </xsl:choose>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Missing values. -->
  
  <xsl:template
      match="test[@missing]">
    <xsl:choose>
      <xsl:when
	  test="@type = 'string'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parString()"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="@type = 'boolean'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parBoolean()"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="@type = 'float'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parFloat()"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="@type = 'int'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parInt()"/>
	</xsl:call-template>
      </xsl:when>
    </xsl:choose>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Element as value. -->
  
  <xsl:template
      match="test[@node]">
    <xsl:choose>
      <xsl:when
	  test="@type = 'string'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parString(.)"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="@type = 'boolean'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parBoolean(.)"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="@type = 'float'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parFloat(.)"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="@type = 'int'">
	<xsl:call-template
	    name="output">
	  <xsl:with-param
	      name="value"
	      select="ext:parInt(.)"/>
	</xsl:call-template>
      </xsl:when>
    </xsl:choose>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Test attribute values. -->

  <xsl:template
      match="@string">
    <xsl:call-template
	name="output">
      <xsl:with-param
	  name="value"
	  select="ext:parString(.)"/>
    </xsl:call-template>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template
      match="@boolean">
    <xsl:call-template
	name="output">
      <xsl:with-param
	  name="value"
	  select="ext:parBoolean(boolean(string(.)))"/>
    </xsl:call-template>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template
      match="@float">
    <xsl:call-template
	name="output">
      <xsl:with-param
	  name="value"
	  select="ext:parFloat(number(.))"/>
    </xsl:call-template>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template
      match="@int">
    <xsl:call-template
	name="output">
      <xsl:with-param
	  name="value"
	  select="ext:parInt(number(.))"/>
    </xsl:call-template>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- By default apply also to attributes. -->
  <xsl:template
      match="* | /">
    <xsl:apply-templates
	select="* | @* | text()"/>
  </xsl:template>

  <!-- Suppress normal output. -->
  <xsl:template
      match="text() | @*"/>

</xsl:stylesheet>
"""

    extension = XPathExtensionTest()

###############################################################################

class XsltParserXPathExtensionTestSuite(DocutilsTestSupport.ParserTestSuite):

    test_case_class = XsltParserXPathExtensionTestCase

###############################################################################

xPathExtension = { }

xPathExtension['simple'] = (
    ( u"""<?xml version="1.0"?>
<root/>
""",
    "" ),
    )

xPathExtension['void'] = (
    ( u"""<?xml version="1.0"?>
<void/>
""",
    """
""" ),
    )

xPathExtension['string'] = (
    ( u"""<?xml version="1.0"?>
<test type="string" const=""/>
""",
    """u'Constant'
""" ),
    ( u"""<?xml version="1.0"?>
<root string="Attribute content"/>
""",
    """u'Attribute content'
""" ),
    ( u"""<?xml version="1.0"?>
<test type="string" missing=""/>
""",
    TypeError ),
    ( u"""<?xml version="1.0"?>
<test type="string" node="">Element content</test>
""",
    TypeError ),
    ( u"""<?xml version="1.0"?>
<root string="ümlaut"/>
""",
    """u'\\xfcmlaut'
""" ),
    )

xPathExtension['boolean'] = (
    ( u"""<?xml version="1.0"?>
<test type="boolean" const=""/>
""",
    """True
""" ),
    ( u"""<?xml version="1.0"?>
<root boolean="non-empty"/>
""",
    """True
""" ),
    ( u"""<?xml version="1.0"?>
<root boolean=""/>
""",
    """False
""" ),
    ( u"""<?xml version="1.0"?>
<test type="boolean" missing=""/>
""",
    TypeError ),
    ( u"""<?xml version="1.0"?>
<test type="boolean" node="">non-empty</test>
""",
    TypeError ),
    )

xPathExtension['float'] = (
    ( u"""<?xml version="1.0"?>
<test type="float" const=""/>
""",
    """3.14
""" ),
    ( u"""<?xml version="1.0"?>
<root float="0"/>
""",
    """0.0
""" ),
    ( u"""<?xml version="1.0"?>
<root float=" -3.7 "/>
""",
    """-3.7
""" ),
    ( u"""<?xml version="1.0"?>
<root float="1e-3"/>
""",
    """0.001
""" ),
    ( u"""<?xml version="1.0"?>
<test type="float" missing=""/>
""",
    TypeError ),
    ( u"""<?xml version="1.0"?>
<test type="float" node="">10.7</test>
""",
    TypeError ),
    )

xPathExtension['int'] = (
    ( u"""<?xml version="1.0"?>
<test type="int" const=""/>
""",
    """42
""" ),
    ( u"""<?xml version="1.0"?>
<root int="0"/>
""",
    """0
""" ),
    ( u"""<?xml version="1.0"?>
<root int=" -3 "/>
""",
    """-3
""" ),
    ( u"""<?xml version="1.0"?>
<root int="1e3"/>
""",
    """1000
""" ),
    ( u"""<?xml version="1.0"?>
<test type="int" missing=""/>
""",
    TypeError ),
    ( u"""<?xml version="1.0"?>
<test type="int" node="">17</test>
""",
    TypeError ),
    ( u"""<?xml version="1.0"?>
<root int="3.14"/>
""",
    ValueError ),
    )

###############################################################################
###############################################################################

class XsltParserParameterTestCase(XsltParserTestCase):
    """
    Output checker for XsltParser using parameters.
    """

    xslt = u"""\
<?xml version="1.0"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0">

  <xsl:param
      name="bool"
      select="false()"/>
  <xsl:param
      name="int"
      select="0"/>
  <xsl:param
      name="float"
      select="0.0"/>
  <xsl:param
      name="string"
      select="''"/>
  <xsl:param
      name="mandatory"/>

  <xsl:output
      method="text"
      encoding="utf-8"/>

  <xsl:template match="/">
    <xsl:value-of
	select="$bool"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:value-of
	select="$int"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:value-of
	select="$float"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:value-of
	select="$string"/>
    <xsl:text>&#xA;</xsl:text>
    <xsl:value-of
	select="$mandatory"/>
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>

</xsl:stylesheet>
"""

###############################################################################

class XsltParserParameterTestSuite(DocutilsTestSupport.ParserTestSuite):

    test_case_class = XsltParserParameterTestCase

###############################################################################

parameter = { }

parameter['simple'] = (
    ( u"""<?xml version="1.0"?>
<rootOnly/>""",
    u"""false
0
0


""" ),
    ( ( { 'bool': True,
          'int': 42,
          'float': 3.14,
          'string': 'bla',
          'mandatory': 'given',
          },
        u"""<?xml version="1.0"?>
<rootOnly/>""", ),
    """true
42
3.14
bla
given
""" ),
    ( ( { 'bool': False,
          'int': 2147483647,
          'float': 1e50,
          'string': '',
          'mandatory': ( 1, 2 ),
          },
        u"""<?xml version="1.0"?>
<rootOnly/>""", ),
    """false
2147483647
 1e+50


""" ),
    ( ( { 'string': '"\'',
          },
        u"""<?xml version="1.0"?>
<rootOnly/>""", ),
      ValueError ),
    ( ( { 'bool': True,
          'int': 42,
          'float': 3.14,
          'string': u'ümlaut',
          'mandatory': 'given',
          },
        u"""<?xml version="1.0"?>
<rootOnly/>""", ),
    """true
42
3.14
ümlaut
given
""" ),
    )

###############################################################################
###############################################################################

def suite():
    s = unittest.TestSuite()

    identitySuite = XsltParserIdentityTestSuite()
    identitySuite.generateTests(identity, dictname='identity')
    s.addTest(identitySuite)

    xPathExtensionSuite = XsltParserXPathExtensionTestSuite()
    xPathExtensionSuite.generateTests(xPathExtension, dictname='xPathExtension')
    s.addTest(xPathExtensionSuite)

    parameterSuite = XsltParserParameterTestSuite()
    parameterSuite.generateTests(parameter, dictname='parameter')
    s.addTest(parameterSuite)

    return s

###############################################################################

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
