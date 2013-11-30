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
Do conversion by using Python.
"""

__docformat__ = 'reStructuredText' # Formatted to be rendered by epydoc

###############################################################################
###############################################################################
# Import

import docutils.nodes

from docutils_xml.parsers.xml import XmlParser, XmlVisitor, SomeChildren, Uri2Prefixes

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

class OdfVisitor(XmlVisitor):
    """
    Visitor class for visiting an ODF element tree.
    """

    ###########################################################################
    # Presentation

    def visit_draw_page(self, elem):
        self.push(docutils.nodes.section())

    def depart_draw_page(self, elem):
        self.pop()

    ###########################################################################
    # Lists

    def visit_text_list(self, elem):
        self.push(docutils.nodes.bullet_list(bullet="*"))
        raise SomeChildren(( ( "text", "list-item" ), ))

    def depart_text_list(self, elem):
        self.pop()

    def visit_text_listitem(self, elem):
        self.push(docutils.nodes.list_item())
        raise SomeChildren(( ( "text", "list" ), ( "text", "p" ), ))

    def depart_text_listitem(self, elem):
        self.pop()

    ###########################################################################
    # Text

    def visit_text_p(self, elem):
        para = docutils.nodes.paragraph()
        if elem.text is not None:
            para.append(docutils.nodes.Text(elem.text))
        self.push(para)
        # TODO tails of embedded inline elements must be considered

    def depart_text_p(self, elem):
        self.pop()

    # TODO Implement

###############################################################################

class Parser(XmlParser):
    """
    Parse the input file and translate it to the output file by Python.
    """

    uri2Prefixes = Uri2Prefixes((
            ( "http://www.w3.org/2001/xml-events", 'dom' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0", 'draw' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0", 'fo' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:presentation:1.0", 'presentation' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:script:1.0", 'script' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0", 'smil' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:style:1.0", 'style' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0", 'svg' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:table:1.0", 'table' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:text:1.0", 'text' ),
            ( "http://www.w3.org/1999/xlink", 'xlink' ),
            ( "urn:oasis:names:tc:opendocument:xmlns:office:1.0", 'office' ),
            ))

    visitorClass = OdfVisitor
