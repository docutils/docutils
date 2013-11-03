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

import os.path

import docutils.parsers, docutils.writers, docutils.nodes

from lxml import etree

###############################################################################
###############################################################################
# Constants

###############################################################################
###############################################################################
# Variables

###############################################################################
###############################################################################
# Functions

def elem2PrefixName(elem):
    """
    :Parameters:

      elem : etree._Element
        The root element of the part of the input document to be traversed.

    :rtype: ( unicode | None, unicode )
    :return: Return namespace prefix and localname of `elem`. Namespace
             prefix may be ``None`` if no or unknown namespace.
    """
    qName = etree.QName(elem)
    prefix = None
    # elem.prefix would also work for lxml but using the namespace is saver
    if qName.namespace:
        prefix = Parser.ns2Prefix.get(qName.namespace, None)
    return ( prefix, qName.localname )

###############################################################################
###############################################################################
# Classes

class Parser(docutils.parsers.Parser):
    """
    Parse the input file and translate it to the output file by Python.
    """

    ns2Prefix = {
        "http://www.w3.org/2001/xml-events": u'dom',
        "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0": u'draw',
        "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0": u'fo',
        "urn:oasis:names:tc:opendocument:xmlns:presentation:1.0": u'presentation',
        "urn:oasis:names:tc:opendocument:xmlns:script:1.0": u'script',
        "urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0": u'smil',
        "urn:oasis:names:tc:opendocument:xmlns:style:1.0": u'style',
        "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0": u'svg',
        "urn:oasis:names:tc:opendocument:xmlns:table:1.0": u'table',
        "urn:oasis:names:tc:opendocument:xmlns:text:1.0": u'text',
        "http://www.w3.org/1999/xlink": u'xlink',
        "urn:oasis:names:tc:opendocument:xmlns:office:1.0": u'office',
        }
    """Map namespace tag name to namespace URI."""

    def parse(self, inputstring, document):
        [ etree.register_namespace(prefix, uri)
          for ( uri, prefix ) in self.ns2Prefix.items() ]
        inDoc = etree.fromstring(inputstring)
        self.walk(inDoc, OdfVisitor(document))

    def walk(self, elem, visitor):
        """
        Traverse the element tree and create the result.

        :Parameters:

          elem : etree._Element
            The root element of the part of the input document to be traversed.

          visitor : OdfVisitor
            The visitor to use.

        :return: ``True`` if the traversal should be stopped.
        :rtype: bool
        """
        stop = False
        skipDeparture = False
        someChildren = None
        try:
            try:
                visitor.visit(elem)
            except docutils.nodes.SkipNode:
                return stop
            except docutils.nodes.SkipDeparture:
                skipDeparture = True
            except SomeChildren, e:
                someChildren = [ ( unicode(prefix), unicode(tag) )
                                 for ( prefix, tag ) in e.tags ]
            try:
                for child in elem:
                    if (someChildren is not None
                        and elem2PrefixName(child) not in someChildren):
                        continue
                    if self.walk(child, visitor):
                        stop = True
                        break
            except docutils.nodes.SkipSiblings:
                pass
        except docutils.nodes.SkipChildren:
            pass
        except docutils.nodes.StopTraversal:
            stop = True
        if not skipDeparture:
            visitor.depart(elem)
        return stop

###############################################################################

class SomeChildren(Exception):
    """
    Thrown to process only some children of the given node.
    """

    def __init__(self, tags):
        self.tags = tags
        """
        :type: ( ( str, str ), ... )

        List of children tags to process. A tag is given as a pair of namespace
        prefix and local name.
        """

###############################################################################

class OdfVisitor(object):
    """
    Visitor class for visiting an ODF element tree.

    See `event_prefix_tag` for a description of the methods in this visitor.
    """

    def event_prefix_tag(self, elem):
        """
        Dummy method for documentation.

        Vistor methods are named *event*\_\ *prefix*\_\ *tag* where *event* is
        either ``visit`` or ``depart``, *prefix* is the namespace prefix and
        *tag* is the local name of the tag with hyphens removed.

        :Parameters:

          elem : etree._Element
            The element to process.

        :except docutils.nodes.TreePruningException:
          Thrown to control the behavior of the caller.

        :except SomeChildren:
          Thrown to limit the children processed.

        :rtype: void

        """
        raise NotImplementedError("'event_prefix_tag' is just for documentation")

    def __init__(self, document):
        self.document = document
        """
        :type: docutils.nodes.document

        The target document.
        """
        self.stack = [ document ]
        """
        :type: [ docutils.nodes.Node, ... ]

        The stack of current nodes. The last one is the most recent current
        node.
        """

    def applyMethod(self, elem, event):
        """
        Find and apply method.

        :Parameters:

          elem : lxml.etree._Element
            The element to apply the method to.

          event : str
            The event to apply.
        """
        ( prefix, name ) = elem2PrefixName(elem)
        name = name.replace("-", "")
        methodName = "_".join(( event, prefix, name ))
        method = getattr(self, methodName, self.visitDefault)
        return method(elem)

    def visit(self, elem):
        """
        Visit an element.

        :Parameters:

          elem : lxml.etree._Element
            The element to visit.
        """
        self.applyMethod(elem, 'visit')

    def depart(self, elem):
        """
        Depart an element.

        :Parameters:

          elem : lxml.etree._Element
            The element to depart.
        """
        self.applyMethod(elem, 'depart')

    def visitDefault(self, elem):
        """
        Used for elements without specific visit method.
        """
        pass

    def departDefault(self, elem):
        """
        Used for elements without specific depart method.
        """
        pass

    def push(self, node):
        """
        Add a node to the current node and make the node the current node.

        :Parameters:

          node : docutils.nodes.Node
            The new node to be made current.

        :return: The former current node which is now the parent.
        :rtype: docutils.nodes.Node
        """
        parent = self.stack[-1]
        assert isinstance(parent, docutils.nodes.Element), \
            "Can not push to '%s'" % ( type(parent), )
        parent.append(node)
        self.stack.append(node)
        return parent

    def pop(self):
        """
        Remove current node and make its parent the new current node.

        :return: The former current node which is now a child of the current
                 node.
        :rtype: docutils.nodes.Node
        """
        return self.stack.pop()

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
