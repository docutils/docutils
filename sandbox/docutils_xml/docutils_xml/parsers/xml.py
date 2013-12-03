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
Base classes for parsing XML and transform it to a Docutils document.
"""

__docformat__ = 'reStructuredText' # Formatted to be rendered by epydoc

###############################################################################
###############################################################################
# Import

import re

import docutils.parsers, docutils.nodes

from lxml import etree

from docutils_xml.parsers import encodeForXmlParser

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

class SomeChildren(docutils.nodes.TreePruningException):
    """
    Thrown to process only some children of the given node.

    Resembles ::

      <xsl:apply-templates
	  select="..."/>
    """

    def __init__(self, tags):
        """
        :Parameters:

          tags : ( ( str | unicode, str | unicode ), ... )
            See `tags`.

        """
        self.tags = [ ( unicode(prefix), unicode(tag) )
                      for ( prefix, tag ) in tags ]
        """
        :type: ( ( unicode, unicode ), ... )

        List of children tags to process. A tag is given as a pair of namespace
        prefix and local name.
        """

###############################################################################

class Uri2Prefixes(object):
    """
    Maps namespace URI to prefixes.
    """

    uri2Prefixes = { }
    """
    :type: { unicode: [ unicode, ... ] , ... }

    Map namespace URI to one or more namespace prefixes.
    """

    def __init__(self, uriPrefixes):
        """
        :Parameters:

          uriPrefixes : ( ( basestring, basestring, ... ), ... )
            For each tuple in the list use the first entry as an URI and the
            remaining entries as prefixes defined for this URI. An URI may be
            given more than once

            The strings must be convertable to unicode.
        """
        for uriPfxes in uriPrefixes:
            pfxList = [ unicode(s)
                        for s in uriPfxes ]
            uri = pfxList.pop(0)
            if uri not in self.uri2Prefixes:
                self.uri2Prefixes[uri] = [ ]
            self.uri2Prefixes[uri].extend(pfxList)

    def etreeRegister(self):
        """
        Register all namespace URIs in etree.

        :note: In etree this is a global setting which is problematic because
               it is shared. Take care to override the namespaces every time
               the parser needs them.
        """
        for ( uri, prefixes ) in self.uri2Prefixes.items():
            for prefix in prefixes:
                etree.register_namespace(prefix, uri)

    def elem2PrefixName(self, elem):
        """
        :Parameters:

          elem : etree._Element
            The element to work for.

        :rtype: ( unicode | None, unicode )
        :return: Namespace prefix and localname of `elem`. Namespace prefix may
                 be ``None`` if no or unknown namespace. If the namespace has
                 more than one prefixes the first one is used.
        """
        qName = etree.QName(elem)
        prefix = None
        # elem.prefix would also work for lxml but using the namespace is saver
        if qName.namespace:
            prefixes = self.uri2Prefixes.get(qName.namespace, None)
            if prefixes is not None:
                prefix = prefixes[0]
        return ( prefix, qName.localname )

###############################################################################

class XmlVisitor(object):
    """
    Base visitor class for visiting an XML tree.

    See `event_prefix_tag` for a description of the methods in this visitor.
    """

    def event_prefix_tag(self, elem):
        """
        Dummy method for documentation.

        Vistor methods are named *event*\_\ *prefix*\_\ *tag* where *event* is
        either ``visit`` or ``depart``, *prefix* is the namespace prefix and
        *tag* is the local name of the tag with hyphens and other non-word and
        non-ASCII characters removed.

        :Parameters:

          elem : etree._Element
            The element to process.

        :except docutils.nodes.TreePruningException:
          Thrown to control the behavior of the caller.

          Note that are more subclasses defined here.

        :rtype: void

        """
        raise NotImplementedError("'event_prefix_tag' is just for documentation")

    def __init__(self, uri2Prefixes, document):
        """
        See instance attributes for a description of the parameters.
        """
        self.uri2Prefixes = uri2Prefixes
        """
        :type: Uri2Prefixes

        The namespace prefixes to use. These must match those of the parser
        used.
        """
        self.document = document
        """
        :type: docutils.nodes.document

        The target document.
        """
        self.stack = [ document ]
        """
        :type: [ docutils.nodes.Node, ... ]

        The stack of current nodes. The last one is the most recent current
        node and the first one is the document itself.
        """

    def _applyMethod(self, elem, event):
        """
        Find and apply method.

        :Parameters:

          elem : lxml.etree._Element
            The element to apply the method to.

          event : str
            The event to apply.
        """
        ( prefix, name ) = self.uri2Prefixes.elem2PrefixName(elem)
        if prefix is None:
            prefix = ""
        prefix = prefix.replace("-", "")
        name = re.sub(r"\W", "", name)
        methodName = "_".join(( event, prefix, name ))
        try:
            method = getattr(self, methodName)
        except AttributeError:
            method = getattr(self, "".join(( event, "Default" )))
        return method(elem)

    def visit(self, elem):
        """
        Visit an element.

        :Parameters:

          elem : lxml.etree._Element
            The element to visit.
        """
        self._applyMethod(elem, 'visit')

    def depart(self, elem):
        """
        Depart an element.

        :Parameters:

          elem : lxml.etree._Element
            The element to depart.
        """
        self._applyMethod(elem, 'depart')

    def visitDefault(self, elem):
        """
        Used for elements without specific visit method. Does nothing.

        May be overridden in subclasses.
        """
        pass

    def departDefault(self, elem):
        """
        Used for elements without specific depart method. Does nothing.

        May be overridden in subclasses.
        """
        pass

    def push(self, node):
        """
        Add a node to the current node and make it the current node.

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

###############################################################################

class XmlParser(docutils.parsers.Parser):
    """
    A generic XML parser for parsing XML input populating the Docutils doctree.
    """

    uri2Prefixes = Uri2Prefixes(( ))
    """
    :type: Uri2Prefixes

    Map namespace URI to prefixes for this parser.

    Usually overridden in subclasses.
    """

    visitorClass = XmlVisitor
    """
    :type: class

    The visitor class to use. Must be a subclass of `XmlVisitor`.

    This is the essential thing to override in subclasses. All the logic on how
    to parse and transform an XML document is in the visitor.
    """

    def parse(self, inputstring, document):
        self.setup_parse(inputstring, document)
        self.uri2Prefixes.etreeRegister()
        inDoc = etree.fromstring(encodeForXmlParser(inputstring))
        self.walk(inDoc, self.visitorClass(self.uri2Prefixes, document))
        self.finish_parse()

    def walk(self, elem, visitor):
        """
        Traverse the element tree and create the result.

        :Parameters:

          elem : etree._Element
            The root element of the part of the input document to be traversed.

          visitor : XmlVisitor
            The visitor to use.

        :return: ``True`` if the traversal should be stopped.
        :rtype: bool
        """
        stop = False
        skipDeparture = False
        someChildren = None
        beep = False
        try:
            try:
                visitor.visit(elem)
            except docutils.nodes.SkipNode:
                return stop
            except docutils.nodes.SkipDeparture:
                skipDeparture = True
            except SomeChildren, e:
                someChildren = e.tags
            try:
                for child in elem:
                    if (someChildren is not None
                        and self.uri2Prefixes.elem2PrefixName(child)
                        not in someChildren):
                        continue
                    if self.walk(child, visitor):
                        stop = True
                        break
            except docutils.nodes.SkipSiblings:
                # Implies SkipDeparture for the child because it is caught only
                # by the parent
                pass
        except docutils.nodes.SkipChildren:
            pass
        except docutils.nodes.StopTraversal:
            stop = True
        if not skipDeparture:
            visitor.depart(elem)
        return stop
