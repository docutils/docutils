#!/usr/bin/env python

# Copyright (C) 2010 Stefan Merten

# rstdiff.py is free software; you can redistribute it and/or modify
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
Translates a reStructuredText document to a plain text outline. This
can then be transformed_ to PowerPoint.

.. _transformed: http://www.pptfaq.com/FAQ00246.htm
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

from docutils import frontend, writers, nodes
from docutils.core import publish_cmdline, default_description

description = ('Generates a plain text outline from a standalone '
               'reStructuredText source.  ' + default_description)

class Writer(writers.Writer):

    supported = ('outline',)
    """Formats this writer supports."""

    settings_spec = (
        'Outline Writer Options',
        None,
        ( )
        )

    settings_defaults = { }

    config_section = 'outline writer'
    config_section_dependencies = ( 'writers', )

    output = None
    """Final translated form of `document`."""

    def translate(self):
        settings = self.document.settings
        visitor = OutlineTranslator(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.doc

class OutlineTranslator(nodes.SparseNodeVisitor):

    doc = ""
    """The resulting document."""

    inTitle = False
    inBullet = 0
    inItem = [ False, ]
    inPara = [ ]

    def __init__(self, document):
        nodes.SparseNodeVisitor.__init__(self, document)

    def visit_title(self, node):
        self.addBol(0)
        self.inTitle = True

    def depart_title(self, node):
        self.addEol()
        self.inTitle = False

    def visit_bullet_list(self, node):
        self.inBullet += 1

    def depart_bullet_list(self, node):
        self.inBullet -= 1

    # TODO Other list types need to be included as well

    def visit_list_item(self, node):
        self.inItem = True

    def depart_list_item(self, node):
        self.inItem = False

    def visit_paragraph(self, node):
        if self.inItem:
            self.inPara = True
            self.inItem = False
            self.addBol(self.inBullet)

    def depart_paragraph(self, node):
        if self.inPara:
            self.addEol()
        self.inPara = False

    def visit_Text(self, node):
        if self.inTitle or self.inPara:
            self.addText(node.astext())

    def addBol(self, level):
        self.doc += "\t" * level

    def addText(self, text):
        self.doc += text.replace("\n", " ")

    def addEol(self):
        self.doc += "\r\n"

# TODO References to images should be included somehow

publish_cmdline(writer=Writer(), description=description)
