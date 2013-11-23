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
Test XmlVisitor.
"""

import unittest
from lxml import etree
import docutils.nodes, docutils.utils

from docutils_xml.parsers.xml import XmlVisitor, Uri2Prefixes

###############################################################################

class XmlVisitorMock(XmlVisitor):
    """
    Mock class recording calls.
    """

    def __init__(self, uri2Prefixes, document):

        self.calls = [ ]
        """
        :type: [ ( str, ( ... ), { str: ..., ... } ) ]

        Sequence of calls seen. Each entry is a tuple consisting of the name of
        the method calles, the array of positional arguments given and a dict
        with the keyword arguments.
        """

        self.currentCall = None
        """
        :type: str

        The name of the current call.
        """

        XmlVisitor.__init__(self, uri2Prefixes, document)

    def __record(self, *args, **kwargs):
        self.calls.append(( self.currentCall, args, kwargs ))
        return None

    def __getattr__(self, name):
        if 'default' in name:
            # Pass through attributes containing "default" so the default
            # method is chosen
            raise AttributeError("Should use *Default method")
        self.currentCall = name
        return self.__record

    def visitDefault(self, elem):
        self.currentCall = 'visitDefault'
        self.__record(elem)

    def departDefault(self, elem):
        self.currentCall = 'departDefault'
        self.__record(elem)

###############################################################################

class XmlVisitorTests(unittest.TestCase):

    def setUp(self):
        self.visitor = XmlVisitorMock(Uri2Prefixes(( )),
                                      docutils.utils.new_document(None))

    def test__init__(self):
        self.assertIsInstance(XmlVisitor(None, None), XmlVisitor)
        self.assertIsInstance(self.visitor, XmlVisitor)
        self.assertEqual(len(self.visitor.stack), 1)

    def test_dummyMethod(self):
        with self.assertRaises(NotImplementedError):
            self.visitor.event_prefix_tag(None)

    def test_visit(self):
        rootElem = etree.Element('root')
        dashedElem = etree.SubElement(rootElem, 'da-sh-ed')
        defaultElem = etree.SubElement(rootElem, 'default')
        self.visitor.visit(rootElem)
        self.visitor.visit(dashedElem)
        self.visitor.visit(defaultElem)
        self.assertEqual(self.visitor.calls, [
                ( 'visit__root', ( rootElem, ), { } ),
                ( 'visit__dashed', ( dashedElem, ), { } ),
                ( 'visitDefault', ( defaultElem, ), { } ),
                ])
        # TODO Tests with namespaces

    def test_depart(self):
        rootElem = etree.Element('root')
        defaultElem = etree.SubElement(rootElem, 'default')
        self.visitor.depart(rootElem)
        self.visitor.depart(defaultElem)
        self.assertEqual(self.visitor.calls, [
                ( 'depart__root', ( rootElem, ), { } ),
                ( 'departDefault', ( defaultElem, ), { } ),
                ])

    def test_stack(self):
        self.assertIsInstance(self.visitor.push(docutils.nodes.Text('bla')),
                              docutils.nodes.document)
        with self.assertRaises(AssertionError):
            self.visitor.push(None)
        self.assertIsInstance(self.visitor.pop(), docutils.nodes.Text)
        self.assertIsInstance(self.visitor.pop(), docutils.nodes.document)
        with self.assertRaises(IndexError):
            self.visitor.pop()

###############################################################################

if __name__ == '__main__':
    unittest.main()
