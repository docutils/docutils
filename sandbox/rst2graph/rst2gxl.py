#!/usr/bin/env python

# Copyright (C) 2010, 2013 Stefan Merten

# rst2graph.py is free software; you can redistribute it and/or modify
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
Translates the links in a reStructuredText document to a graph. The graph can
be output in various formats such as GXL_ or dot_. This graph can then be
converted to a graphical representation of the link structure of the document.

.. _GXL: http://www.gupro.de/GXL
.. _dot: http://www.graphviz.org/content/dot-language
"""

__docformat__ = 'reStructuredText'

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

import docutils
from docutils import frontend, writers, nodes, utils
from docutils.core import publish_cmdline, default_description

from xml.dom import minidom

import re
import sys
import os.path

try:
    from pygraphviz import AGraph
    pygraphvizAvail = True
except:
    pygraphvizAvail = False

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

CmdRst2Gxl = "rst2gxl"
CmdRst2Dot = "rst2dot"
CmdRst2Gv = "rst2gv"

##############################################################################
##############################################################################
# Functions

def string2XMLName(s):
    """Return an XML Name similar to the string given."""
    s = re.sub(r"(?u)[^-.:\w]", "_", s)
    return re.sub("^(?u)[^:\w]", "_", s)

##############################################################################
##############################################################################

class Writer(writers.Writer):

    """Formats this writer supports."""
    supported = ('gxl', 'gv', 'dot')

    settings_spec = (
        'Graph Writer Format Options',
        'The following options determine the output format produced.',
        (
         ('Produce output in the dot language suitable for Graphviz. '
          'Default when called as rst2gv or rst2dot.',
          ['--dot', '--gv'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ('Produce GXL output. '
          'Default when called as rst2gxl.',
          ['--gxl'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ),
        'Graph Writer Global Options',
        'The following options affect the way the graph is produced.',
        (
         ('Put only nodes with connections to the graph. '
          'Default is to put all nodes to the graph.',
          ['--connected-only'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ('Create multiple edges between same node if they exist in the '
          'original document. Default is to unify all edges between two nodes.',
          ['--multiedge'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ('Create a reverse dependency graph. Default is a forward dependency '
          'graph.',
          ['--reverse'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ('Select a certain table and ignore the rest of the document. The '
          'argument must be the name of the table as given in the document or '
          'the number of the table counting from 1. '
          'Default is to consider the whole document. May be given more than '
          'once.',
          ['--select-table'],
          {'action': 'append'}),
         ('Use the whole path of section titles up to a section as label for '
          'a node. Default is to use only the section title.',
          ['--title-path'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         ),
        'Graph Writer GXL Options',
        'The following options are valid only for GXL format output.',
        (
         ('Generate XML with indents and newlines.',
          ['--indents'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),
         )
        )

    settings_defaults = {'output_encoding_error_handler': 'xmlcharrefreplace',
                         'reverse': False,
                         'multiedge': False,
                         'select_table': [ ],
                         'indents': False,
                         'gxl': False,
                         'dot': False,
                         'connected_only': False,
                         'title_path': False,
                         }

    config_section = 'graph writer'
    config_section_dependencies = ('writers',)

    """Final translated form of `document`."""
    output = None

    def translate(self):
        settings = self.document.settings
        if settings.gxl and settings.dot:
            self.document.reporter.severe("Options --gxl and --dot/--gv are mutual exclusive")
        elif not settings.gxl and not settings.dot:
            if len(sys.argv):
                cmd = os.path.basename(sys.argv[0]).lower()
                if cmd.startswith(CmdRst2Gxl):
                    settings.gxl = True
                elif cmd.startswith(CmdRst2Dot) or cmd.startswith(CmdRst2Gv):
                    settings.dot = True
        if not settings.gxl and not settings.dot:
            self.document.reporter.severe("One of --gxl and --dot/--gv must be given or implied")
        if not settings.gxl and settings.indents:
            self.document.reporter.severe("--indents may be given only with --gxl")

        renderer = GraphRenderer.getRenderer(settings, self.document.reporter)
        if not renderer:
            self.document.reporter.severe("Internal error: No `GraphRenderer` found")
        visitor = GraphTranslator(self.document, settings)
        self.document.walkabout(visitor)
        visitor.resolve()
        self.output = renderer.render(visitor)

##############################################################################
##############################################################################

class GraphTranslator(nodes.GenericNodeVisitor):

    """Name of the source file."""
    sourceName = None

    """The list of anchors found by traversing."""
    anchors = [ ]

    """The last anchor found."""
    _lastAnchor = None

    """The list of ``GEdge``\s found by traversing."""
    references = [ ]

    """Stack for being currently in a selected part."""
    _inSelected = [ ]

    """Counter for selecting a table by number."""
    _tablesSeen = 0

    """Selected tables."""
    _selectedTables = None

    """Use title path for labels."""
    _titlePath = None

    def __init__(self, document, settings):
        nodes.GenericNodeVisitor.__init__(self, document)
        self.sourceName = document[DuAttrSource]
        self._selectedTables = settings.select_table
        if self._selectedTables:
            self._inSelected.append(False)
        else:
            self._inSelected.append(True)
        self._titlePath = settings.title_path

    def default_visit(self, node):
        if self.isSelected(node, True):
            self._inSelected.append(True)
        elif self.unSelected(node):
            self._inSelected.append(False)
        if self._inSelected[-1]:
            self.processNode(node)

    def default_departure(self, node):
        if self.isSelected(node, False) or self.unSelected(node):
            self._inSelected.pop()

    def isSelected(self, node, entering):
        if (self._selectedTables
            and isinstance(node, nodes.table)):
            if entering:
                self._tablesSeen += 1
            visitor = FirstTitleGatherer(self.document)
            node.walkabout(visitor)
            title = visitor.text
            for wantedTable in self._selectedTables:
                try:
                    if int(wantedTable) == self._tablesSeen:
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
            self._lastAnchor = anchor = Anchor(node, self.document,
                                               self._titlePath)
            self.anchors.append(anchor)
        if Reference.isReference(node):
            reference = Reference(node, self._lastAnchor)
            self.references.append(reference)

    def resolve(self):
        """Resolve anything necessary after visiting the document."""
        for reference in self.references:
            reference.resolve(self.anchors)

##############################################################################

class Anchor(object):
    """An anchor in the source."""

    """The source node."""
    _node = None

    """The document where the node is in."""
    _document = None

    """The name of the node."""
    _name = None

    """The id of the node."""
    _id = None

    """Use title path for the name."""
    _titlePath = None

    def __init__(self, node, document, titlePath):
        self._node = node
        self._document = document
        self._titlePath = titlePath

    def name(self):
        """Determine and return the user readable name of the anchor."""
        if self._name is None:
            root = self._node
            if isinstance(self._node, nodes.Structural):
                if self._titlePath:
                    visitor = TitlePathGatherer(self._document, self._node)
                    root = self._document
                else:
                    visitor = FirstTitleGatherer(self._document)
            else:
                visitor = TextGatherer(self._document)
            root.walkabout(visitor)
            self._name = visitor.text.replace("\n", "\\n")
        return self._name

    def id(self):
        """Determine and return the canoncical id of the anchor."""
        if self._id is None:
            self._id = self._node[DuAttrIds][0]
        return self._id

    def ids(self):
        """Return all ids of the anchor."""

        return self._node[DuAttrIds]

    @staticmethod
    def isAnchor(node):
        """``True`` if the node can be an ``Anchor``."""
        # TODO What is considered an anchor needs to be subject to an option
        return bool((isinstance(node, nodes.target)
                     or isinstance(node, nodes.Structural))
                    and node[DuAttrIds]
                    and not node.get(DuAttrRefuri, None))

##############################################################################

class Reference(object):
    """A reference to an anchor in the source."""

    """The source node."""
    _node = None

    """The last anchor seen before this reference."""
    fromAnchor = None

    """The anchor this reference points to."""
    toAnchor = None

    def __init__(self, node, fromAnchor):
        self._node = node
        self.fromAnchor = fromAnchor

    def resolve(self, anchors):
        """Resolve this reference against the anchors given."""
        for anchor in anchors:
            if self._node[DuAttrRefid] in anchor.ids():
                self.toAnchor = anchor
                break

    @staticmethod
    def isReference(node):
        """``True`` if the node can be a ``Reference``."""
        return bool(isinstance(node, nodes.Referential)
                    and node.get(DuAttrRefid, None))

##############################################################################
##############################################################################

class TextGatherer(nodes.SparseNodeVisitor):
    """A visitor gathering text. This should be subclassed for more specialized
    gatherers."""

    """Gathered text. May contain line feeds for several lines."""
    text = ""

    def visit_generated(self, node):
        raise nodes.SkipNode()

    def visit_Text(self, node):
        self.text += node.astext()

##############################################################################

class FirstTitleGatherer(TextGatherer):
    """A visitor gathering text in first title."""

    _gather = False

    def visit_title(self, node):
        self._gather = True

    def depart_title(self, node):
        raise nodes.StopTraversal()

    def visit_Text(self, node):
        if self._gather:
            self.text += node.astext()

##############################################################################

class TitlePathGatherer(TextGatherer):
    """A visitor gathering title text for all sections leading up to a certain
    one."""

    """Current title path."""
    _titles = None

    """The structural node we are looking for."""
    _structuralNode = None

    def __init__(self, document, structuralNode):
        nodes.SparseNodeVisitor.__init__(self, document)
        self._titles = [ ]
        if not isinstance(structuralNode, nodes.Structural):
            raise TypeError("Node looked for in `TitlePathGatherer` must be a structural node")
        self._structuralNode = structuralNode

    def visit_section(self, node):
        visitor = FirstTitleGatherer(self.document)
        node.walkabout(visitor)
        self._titles.append(visitor.text)

    def depart_section(self, node):
        if node == self._structuralNode:
            self.text = "\n".join(self._titles)
            raise nodes.StopTraversal()
        else:
            self._titles.pop()

##############################################################################
##############################################################################

class GraphRenderer(object):
    """Abstract base class for graph renderers."""

    """Command line settings.""" 
    _settings = None

    """Reverse the direction of the edges."""
    _doReverse = None

    """Unify multiple edges to a single one."""
    _doUnify = None

    """Render only connected nodes."""
    _connectedOnly = None

    """The GraphTranslator currently rendered."""
    _visitor = None

    def __init__(self, settings, reporter):
        self._settings = settings
        self.reporter = reporter
        self._doReverse = self._settings.reverse
        self._doUnify = not self._settings.multiedge
        self._connectedOnly = self._settings.connected_only

    @staticmethod
    def getRenderer(settings, reporter):
        """Factory method: Return the correct renderer according to the
        settings."""
        if settings.gxl:
            return GxlRenderer(settings, reporter)
        elif settings.dot:
            return DotRenderer(settings, reporter)
        return None

    def render(self, visitor):
        """Translate nodes and edges to a GXL DOM and return it as a XML
        string."""
        self._visitor = visitor
        self.prepare()

        references = self.validReferences(self._visitor.references)
        for anchor in self.validAnchors(self._visitor.anchors, references):
            self.renderAnchor(anchor)
        for reference in references:
            self.renderReference(reference)

        r = self.finish()
        self._visitor = None
        return r

    def prepare(self):
        """Prepare the rendering."""
        pass

    def finish(self):
        """Finish the rendering and return the resulting string."""
        raise NotImplementedError("Each subclass of `GraphRenderer` must implement `finish()`")

    def renderAnchor(self, anchor):
        """Render an anchor."""
        raise NotImplementedError("Each subclass of `GraphRenderer` must implement `renderAnchor()`")

    def renderReference(self, reference):
        """Render a reference."""
        raise NotImplementedError("Each subclass of `GraphRenderer` must implement `renderReference()`")

    def validReferences(self, references):
        """Checks references for valid ones and returns these."""
        valids = [ ]
        for reference in references:
           add = reference.fromAnchor and reference.toAnchor
           if add and self._doUnify:
               for unique in valids:
                   if (reference.fromAnchor == unique.fromAnchor and
                       reference.toAnchor == unique.toAnchor):
                       add = False
                       break
           if add:
               valids.append(reference)
        return valids

    def validAnchors(self, anchors, references):
        """Checks anchors for valid ones and returns these."""
        if not self._connectedOnly:
            return anchors
        usedAnchors = ([ reference.fromAnchor
                         for reference in references ]
                       + [ reference.toAnchor
                           for reference in references ])
        return [ anchor
                 for anchor in anchors
                 if anchor in usedAnchors ]

##############################################################################

class GxlRenderer(GraphRenderer):
    """Renderer fox GXL."""

    """Indentation to use for output."""
    _indent = ''

    """Newline to use for output."""
    _newline = ''

    """The DOM document where the result is build."""
    _doc = None

    """The top node in the DOM."""
    _graph = None

    def __init__(self, settings, reporter):
        GraphRenderer.__init__(self, settings, reporter)
        if self._settings.indents:
            self._indent = '  '
            self._newline = '\n'

    def prepare(self):
        impl = minidom.getDOMImplementation()
        doctype = impl.createDocumentType(GxlTagRoot, None, GxlNamespace)
        self._doc = impl.createDocument(None, GxlTagRoot, doctype)
        self._graph = self._doc.createElement(GxlTagGraph)
        self._doc.documentElement.appendChild(self._graph)
        self._graph.setAttribute(GxlAttrId, string2XMLName(self._visitor.sourceName))
        self._graph.setAttribute(GxlAttrEdgemode, GxlValEdgemode)

    def finish(self):
        r = self._doc.toprettyxml(self._indent, self._newline)
        self._graph = None
        self._doc.unlink()
        self._doc = None
        return r

    def renderAnchor(self, anchor):
        eNode = self._doc.createElement(GxlTagNode)
        self._graph.appendChild(eNode)
        eNode.setAttribute(GxlAttrId, anchor.id())

        eAttr = self._doc.createElement(GxlTagAttr)
        eNode.appendChild(eAttr)
        eAttr.setAttribute(GxlAttrName, GxlTagAttrTagName)

        eContent = self._doc.createElement(GxlTagAttrTagNameTag)
        eAttr.appendChild(eContent)
        eContent.appendChild(self._doc.createTextNode(anchor.name()))

    def renderReference(self, reference):
        if reference.fromAnchor is None:
            # No anchor to start edge from
            # TODO Should result in a warning
            return

        eEdge = self._doc.createElement(GxlTagEdge)
        self._graph.appendChild(eEdge)
        fromAttr = GxlAttrFrom
        toAttr = GxlAttrTo
        if self._doReverse:
            (fromAttr, toAttr) = (toAttr, fromAttr)
        eEdge.setAttribute(toAttr, reference.toAnchor.id())
        # TODO There should be several ways to identify the "from" node
        eEdge.setAttribute(fromAttr, reference.fromAnchor.id())

##############################################################################

class DotRenderer(GraphRenderer):
    """Renderer fox dot."""

    """The AGraph used for rendering."""
    _graph = None

    def __init__(self, settings, reporter):
        GraphRenderer.__init__(self, settings, reporter)

        if not pygraphvizAvail:
            self.reporter.severe("Can not render dot format because module `pygraphviz` cannot be loaded")

    def prepare(self):
        self._graph = AGraph(strict=False, directed=True,
                             name=self._visitor.sourceName)
            
    def finish(self):
        # TODO The ordering of nodes seems to be rather random in the output;
        #      however, the node ordering is used for the layout of the graph
        #      at least by `dot` and by `neato`; there should be a way to
        #      determine the ordering
        r = self._graph.string()
        self._graph = None
        return r

    def renderAnchor(self, anchor):
        self._graph.add_node(anchor.id(), label=anchor.name())

    def renderReference(self, reference):
        if reference.fromAnchor is None:
            # No anchor to start edge from
            # TODO Should result in a warning
            return

        fromId = reference.fromAnchor.id()
        toId = reference.toAnchor.id()
        if self._doReverse:
            (fromId, toId) = (toId, fromId)
        self._graph.add_edge(fromId, toId)

##############################################################################
##############################################################################
# Main

publish_cmdline(writer=Writer(), description=description)
