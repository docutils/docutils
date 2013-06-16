#!/usr/bin/env python

# Copyright (C) 2010 Stefan Merten

# rst2gxl.py is free software; you can redistribute it and/or modify
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
Translates a reStructuredText document to GXL_. This can then be
transformed to graphs for instance by dot_.

.. _GXL: http://www.gupro.de/GXL
.. _dot: http://graphviz.org/
"""

__docformat__ = 'reStructuredText'

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

import docutils
from docutils import frontend, writers, nodes
from docutils.core import publish_cmdline, default_description

from xml.dom import minidom

import re

description = ('Generates GXL from standalone reStructuredText sources.  '
               + default_description)

# `gxl2gv` supports V1.0 only
GxlNamespace = "http://www.gupro.de/GXL/gxl-1.0.dtd"
GxlTagRoot = "gxl"
GxlTagGraph = "graph"
GxlTagNode = "node"
GxlTagEdge = "edge"
GxlAttrId = "id"
GxlTagAttr = "attr"
GxlAttrName = "name"
GxlTagAttrTagName = "name"
GxlTagAttrTagNameTag = "string"
GxlAttrFrom = "from"
GxlAttrTo = "to"
GxlAttrEdgemode = "edgemode"
GxlValEdgemode = "directed"

DuAttrSource = "source"
DuAttrIds = "ids"
DuAttrNames = "names"
DuAttrRefid = "refid"
DuAttrRefuri = "refuri"
DuAttrClasses = "classes"
DuAttrClassesValToc = "contents"

def string2XMLName(s):
    """Return an XML Name similar to the string given."""
    s = re.sub(r"(?u)[^-.:\w]", "_", s)
    return re.sub("^(?u)[^:\w]", "_", s)

class Writer(writers.Writer):

    supported = ('gxl',)
    """Formats this writer supports."""

    settings_spec = (
        'GXL Writer Options',
        None,
        (('Generate XML with indents and newlines. Use this for human '
          'reading only.',
          ['--indents'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ('Create a reverse dependency graph. Default is a forward dependency '
          'graph.',
          ['--reverse'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ('Create multiple edges between same node if they exist in the '
          'original document. Default is to unify all edges between two nodes.',
          ['--multiedge'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ('Select a certain table and ignore the rest of the document. The '
          'argument must be the name of the table as given in the document or '
          ' the number of the table counting from 1. '
          'Default is to consider the whole document. May be given more than '
          'once.',
          ['--select-table'],
          {'action': 'append'}),
         # TODO The encoding must be specified somehow
         )
        )

    settings_defaults = {'output_encoding_error_handler': 'xmlcharrefreplace',
                         'reverse': False, 'multiedge': False,
                         'select_table': [ ]}

    config_section = 'gxl writer'
    config_section_dependencies = ('writers',)

    output = None
    """Final translated form of `document`."""

    def translate(self):
        settings = self.document.settings
        indent = newline = ''
        if settings.indents:
            indent = '  '
            newline = '\n'
        visitor = GXLTranslator(self.document)
        self.document.walkabout(visitor)
        doc = self.nodes2Glx(self.document, visitor.anchors,
                             visitor.references, settings.reverse,
                             not settings.multiedge)
        self.output = doc.toprettyxml(indent, newline)
        doc.unlink()

    def nodes2Glx(self, document, anchors, references, doReverse, doUnify):
        """Translate nodes and edges to a GXL DOM"""

        impl = minidom.getDOMImplementation()
        doctype = impl.createDocumentType(GxlTagRoot, None, GxlNamespace)
        doc = impl.createDocument(None, GxlTagRoot, doctype)
        graph = doc.createElement(GxlTagGraph)
        graph.setAttribute(GxlAttrId, string2XMLName(document[DuAttrSource]))
        graph.setAttribute(GxlAttrEdgemode, GxlValEdgemode)
        doc.documentElement.appendChild(graph)

        for anchor in anchors:
            anchor.renderGlx(doc, graph)

        for reference in references:
            reference.resolve(anchors)

        valids = [ ]
        for reference in references:
            reference.gatherValids(valids, doUnify)
        references = valids

        for reference in references:
            reference.renderGlx(doc, graph, doReverse)

        return doc

class GXLTranslator(nodes.GenericNodeVisitor):

    """The list of anchors found by traversing"""
    anchors = [ ]

    """The last anchor found"""
    lastAnchor = None

    """The list of ``GEdge``\s found by traversing"""
    references = [ ]

    """Stack for being currently in a selected part"""
    inSelected = [ ]

    """Counter for selecting a table by number"""
    tablesSeen = 0

    def __init__(self, document):
        nodes.GenericNodeVisitor.__init__(self, document)
        if document.settings.select_table:
            self.inSelected.append(False)
        else:
            self.inSelected.append(True)

    def default_visit(self, node):
        if self.isSelected(node, True):
            self.inSelected.append(True)
        elif self.unSelected(node):
            self.inSelected.append(False)
        if self.inSelected[-1]:
            self.processNode(node)

    def default_departure(self, node):
        if self.isSelected(node, False) or self.unSelected(node):
            self.inSelected.pop()

    def isSelected(self, node, entering):
        if (self.document.settings.select_table
            and isinstance(node, nodes.table)):
            if entering:
                self.tablesSeen += 1
            visitor = FirstTitleGatherer(self.document)
            node.walkabout(visitor)
            title = visitor.text
            for wantedTable in self.document.settings.select_table:
                try:
                    if int(wantedTable) == self.tablesSeen:
                        return True
                except:
                    if wantedTable == title:
                        return True
        return False

    def unSelected(self, node):
        # TOCs are never selected
        return (isinstance(node, nodes.topic)
                and DuAttrClassesValToc in node.get(DuAttrClasses, ( )))

    def processNode(self, node):
        if Anchor.isAnchor(node):
            self.lastAnchor = anchor = Anchor(node, self.document)
            self.anchors.append(anchor)
        if Reference.isReference(node):
            reference = Reference(node, self.lastAnchor)
            self.references.append(reference)

class Anchor(object):
    """An anchor in the source"""

    """The source node"""
    node = None

    """The name of the node"""
    _name = None

    """The id of the node"""
    _id = None

    def __init__(self, node, document):
        self.node = node
        self.document = document

    def renderGlx(self, doc, graph):
        eNode = doc.createElement(GxlTagNode)
        graph.appendChild(eNode)
        eNode.setAttribute(GxlAttrId, self.id())

        eAttr = doc.createElement(GxlTagAttr)
        eNode.appendChild(eAttr)
        eAttr.setAttribute(GxlAttrName, GxlTagAttrTagName)

        eContent = doc.createElement(GxlTagAttrTagNameTag)
        eAttr.appendChild(eContent)
        eContent.appendChild(doc.createTextNode(self.name()))

    def name(self):
        if self._name is None:
            if isinstance(self.node, nodes.Structural):
                visitor = FirstTitleGatherer(self.document)
            else:
                visitor = TextGatherer(self.document)
            self.node.walkabout(visitor)
            self._name = visitor.text
        return self._name

    def id(self):
        if self._id is None:
            self._id = self.node[DuAttrIds][0]
        return self._id

    def ids(self):
        return self.node[DuAttrIds]

    @staticmethod
    def isAnchor(node):
        """``True`` if the node can be an ``Anchor``"""
        # TODO What is considered an anchor needs to be subject to an option
        return bool((isinstance(node, nodes.target)
                     or isinstance(node, nodes.Structural))
                    and node[DuAttrIds]
                    and not node.get(DuAttrRefuri, None))

class Reference(object):
    """A reference in the source"""

    """The source node"""
    node = None

    """The last anchor seen before this reference"""
    fromAnchor = None

    """The anchor this points to"""
    toAnchor = None

    def __init__(self, node, fromAnchor):
        self.node = node
        self.fromAnchor = fromAnchor

    def renderGlx(self, doc, graph, doReverse):
        if self.fromAnchor is None:
            # No anchor to start edge from
            # TODO Should result in a warning
            return

        eEdge = doc.createElement(GxlTagEdge)
        graph.appendChild(eEdge)
        fromAttr = GxlAttrFrom
        toAttr = GxlAttrTo
        if doReverse:
            ( fromAttr, toAttr ) = ( toAttr, fromAttr )
        eEdge.setAttribute(toAttr, self.toAnchor.id())
        # TODO There should be several ways to identify the "from" node
        eEdge.setAttribute(fromAttr, self.fromAnchor.id())

    def resolve(self, anchors):
        """Resolve this reference against the anchors given."""

        for anchor in anchors:
            if self.node[DuAttrRefid] in anchor.ids():
                self.toAnchor = anchor
                break

    def gatherValids(self, valids, doUnify):
        """Checks whether the current reference appears in the list given. If
        If not adds the current reference and returns ``True``"""

        if not self.fromAnchor or not self.toAnchor:
            return
        if doUnify:
            for unique in valids:
                if (self.fromAnchor == unique.fromAnchor and
                    self.toAnchor == unique.toAnchor):
                    return
        valids.append(self)

    @staticmethod
    def isReference(node):
        """``True`` if the node can be a ``Reference``"""
        return bool(isinstance(node, nodes.Referential)
                    and node.get(DuAttrRefid, None))

class TextGatherer(nodes.SparseNodeVisitor):
    """A visitor gathering text."""

    """Gathered text"""
    text = ""

    gather = True

    def visit_generated(self, node):
        self.gather = False

    def depart_generated(self, node):
        self.gather = True

    def visit_Text(self, node):
        if self.gather:
            self.text += node.astext()

class FirstTitleGatherer(nodes.SparseNodeVisitor):
    """A visitor gathering text in first title."""

    """Gathered text"""
    text = ""

    gather = False
    found = False
    skip = False

    def visit_title(self, node):
        self.gather = not self.found

    def depart_title(self, node):
        self.gather = False
        self.found = True

    def visit_generated(self, node):
        self.skip = True

    def depart_generated(self, node):
        self.skip = False

    def visit_Text(self, node):
        if self.gather and not self.skip:
            self.text += node.astext()

publish_cmdline(writer=Writer(), description=description)
