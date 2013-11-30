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
Test XmlParser.
"""

import unittest
import docutils.frontend
from docutils.nodes import Text

from __init__ import DocutilsTestSupport

from docutils_xml.parsers.xml import Uri2Prefixes, XmlVisitor, XmlParser, SomeChildren

###############################################################################

class XmlVisitorMock(XmlVisitor):
    """
    Mock class recording calls in document.
    """

    depth = 0
    """
    :type: int

    Current indentation depth.
    """

    indent = u"  "
    """
    :type: unicode

    Indentation to use for one step.
    """

    currentPrefix = None
    """
    :type: str

    The prefix of the current call.
    """

    currentTag = None
    """
    :type: str

    The tag of the current call.
    """

    def __recordVisit(self, elem):
        ( pfx, nm ) = self.uri2Prefixes.elem2PrefixName(elem)
        attrs = ""
        for attr in sorted(elem.keys()):
            attrs += " %s=%r" % ( attr, elem.get(attr) )
        self.document += Text("%s{ %s:%s%s\n"
                              % ( self.depth * self.indent,
                                  self.currentPrefix, self.currentTag,
                                  attrs))
        self.depth += 1
        control = elem.get('control', None)
        if control in ( 'SkipNode', 'SkipDeparture', 'SkipSiblings',
                        'SkipChildren', 'StopTraversal' ):
            e = eval("docutils.nodes.%s()" % ( control, ))
            try:
                raise e
            except ( docutils.nodes.SkipNode, docutils.nodes.SkipSiblings ):
                self.depth -= 1
                raise
        elif control == 'SomeChildren':
            tags = [ child.split(':', 1)
                     for child in elem.get('controlSomeChildren', '').split() ]
            raise SomeChildren(tags)
        return None

    def __recordDepart(self, elem):
        ( pfx, nm ) = self.uri2Prefixes.elem2PrefixName(elem)
        self.depth -= 1
        self.document += Text("%s} %s:%s\n"
                              % ( self.depth * self.indent,
                                  self.currentPrefix, self.currentTag ))
        return None

    def __getattr__(self, name):
        ( currentType, self.currentPrefix,
          self.currentTag ) = name.split('_', 2)
        if currentType == 'visit':
            return self.__recordVisit
        else:
            return self.__recordDepart

###############################################################################

class XmlParserMock(XmlParser):
    """
    Mock class recording visited nodes in the output document.
    """

    uri2Prefixes = Uri2Prefixes((
            ( 'urn:example', 'ex', 'alias', 'int' ),
            # ( 'urn:empty', u'' ), # Empty tag is not accepted by lxml
            ( 'urn:other', 'ot' ),
            ))

    visitorClass = XmlVisitorMock

###############################################################################

class XmlParserTestCase(DocutilsTestSupport.ParserTestCase):
    """
    Output checker for XmlParser.

    Supports additional settings on input and exceptions on output as
    `XsltParserTestCase` does.
    """

    parser = XmlParserMock()
    """Parser shared by all XmlParserTestCases."""

    option_parser = docutils.frontend.OptionParser(components=(
            XmlParserMock, ))

    def test_parser(self):
        if self.run_in_debugger:
            pdb.set_trace()
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
            output = document.pformat()
            self.compare_output(input, output, self.expected)

###############################################################################

class XmlParserTestSuite(DocutilsTestSupport.ParserTestSuite):

    test_case_class = XmlParserTestCase

###############################################################################

totest = {}

totest['simple'] = (
    ( u"""<?xml version="1.0"?>
<rootOnly/>
""",
  """<document source="test data">
    { :rootOnly
    } :rootOnly
""" ),
    ( u"""<?xml version="1.0"?>
<root>
  <embedded/>
</root>
""",
      """<document source="test data">
    { :root
      { :embedded
      } :embedded
    } :root
""" ),
    ( u"""<?xml version="1.0"?>
<root>
  <one/>
  <two/>
</root>
""",
      """<document source="test data">
    { :root
      { :one
      } :one
      { :two
      } :two
    } :root
""" ),
    ( u"""<?xml version="1.0"?>
<rootOnly otherAttr='moreContent' attribute="content"/>
""",
  """<document source="test data">
    { :rootOnly attribute='content' otherAttr='moreContent'
    } :rootOnly
""" ),
    )

totest['nonAscii'] = (
    ( u"""<?xml version="1.0"?>
<rootÜmlaut/>
""",
  """<document source="test data">
    { :rootmlaut
    } :rootmlaut
""" ),
    )

totest['encoding'] = (
    ( """<?xml version="1.0"?>
<rootOnly/>
""",
  """<document source="test data">
    { :rootOnly
    } :rootOnly
""" ),
    ( """<?xml version="1.0" encoding="ascii"?>
<rootOnly/>
""",
  """<document source="test data">
    { :rootOnly
    } :rootOnly
""" ),
    ( u"""<?xml version="1.0" encoding="ascii"?>
<rootOnly/>
""",
  """<document source="test data">
    { :rootOnly
    } :rootOnly
""" ),
    ( """<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""",
  """<document source="test data">
    { :rootOnly
    } :rootOnly
""" ),
    ( u"""<?xml version="1.0" encoding="utf-8"?>
<rootOnly/>
""",
  """<document source="test data">
    { :rootOnly
    } :rootOnly
""" ),
    ( """<?xml version="1.0" encoding="utf-8"?>
<root\xC3\x9Cmlaut/>
""",
  u"""<document source="test data">
    { :rootmlaut
    } :rootmlaut
""" ),
    ( u"""<?xml version="1.0" encoding="utf-8"?>
<rootÜmlaut/>
""",
  u"""<document source="test data">
    { :rootmlaut
    } :rootmlaut
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

totest['namespace'] = (
    ( u"""<?xml version="1.0"?>
<root
    xmlns:int="urn:example"
    xmlns:alias="urn:example"
    xmlns:ot="urn:other">
  <int:one/>
  <alias:two/>
  <ot:three/>
</root>
""",
      """<document source="test data">
    { :root
      { ex:one
      } ex:one
      { ex:two
      } ex:two
      { ot:three
      } ot:three
    } :root
""" ),
    )

totest['SkipNode'] = (
    ( u"""<?xml version="1.0"?>
<root control='SkipNode'>
  <one/>
  <two/>
</root>
""",
      """<document source="test data">
    { :root control='SkipNode'
""" ),
    )

totest['SkipDeparture'] = (
    ( u"""<?xml version="1.0"?>
<root control='SkipDeparture'>
  <one/>
  <two/>
</root>
""",
      """<document source="test data">
    { :root control='SkipDeparture'
      { :one
      } :one
      { :two
      } :two
""" ),
    )

totest['SkipSiblings'] = (
    ( u"""<?xml version="1.0"?>
<root>
  <one control='SkipSiblings'/>
  <two/>
</root>
""",
      """<document source="test data">
    { :root
      { :one control='SkipSiblings'
    } :root
""" ),
    )

totest['SkipChildren'] = (
    ( u"""<?xml version="1.0"?>
<root control='SkipChildren'>
  <one/>
  <two/>
</root>
""",
      """<document source="test data">
    { :root control='SkipChildren'
    } :root
""" ),
    )

totest['StopTraversal'] = (
    ( u"""<?xml version="1.0"?>
<root>
  <one control='StopTraversal'/>
  <two/>
</root>
""",
      """<document source="test data">
    { :root
      { :one control='StopTraversal'
      } :one
    } :root
""" ),
    )

totest['SomeChildren'] = (
    # Take care to use namespaced children
    ( u"""<?xml version="1.0"?>
<root xmlns:int="urn:example"
      control='SomeChildren'>
  <int:one/>
  <int:two/>
</root>
""",
      """<document source="test data">
    { :root control='SomeChildren'
    } :root
""" ),
    ( u"""<?xml version="1.0"?>
<root xmlns:int="urn:example"
      control='SomeChildren' controlSomeChildren='ex:one'>
  <int:one/>
  <int:two/>
</root>
""",
      """<document source="test data">
    { :root control='SomeChildren' controlSomeChildren='ex:one'
      { ex:one
      } ex:one
    } :root
""" ),
    ( u"""<?xml version="1.0"?>
<root xmlns:int="urn:example"
      control='SomeChildren' controlSomeChildren='ex:one ex:two ex:three'>
  <int:one/>
  <int:two/>
</root>
""",
      """<document source="test data">
    { :root control='SomeChildren' controlSomeChildren='ex:one ex:two ex:three'
      { ex:one
      } ex:one
      { ex:two
      } ex:two
    } :root
""" ),
    )

###############################################################################

def suite():
    s = XmlParserTestSuite()
    s.generateTests(totest)
    return s

###############################################################################

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
