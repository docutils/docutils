#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Docutils document tree element class library.

Classes in CamelCase are abstract base classes or auxiliary classes. The one
exception is `Text`, for a text node; uppercase is used to differentiate from
element classes.

Classes in lower_case_with_underscores are element classes, matching the XML
element generic identifiers in the DTD_.

.. _DTD: http://docutils.sourceforge.net/spec/docutils.dtd
"""

import sys, os
import xml.dom.minidom
from types import IntType, SliceType, StringType, TupleType, ListType
from UserString import MutableString
import utils
import docutils


# ==============================
#  Functional Node Base Classes
# ==============================

class Node:

    """Abstract base class of nodes in a document tree."""

    parent = None
    """Back-reference to the `Node` containing this `Node`."""

    def __nonzero__(self):
        """Node instances are always true."""
        return 1

    def asdom(self, dom=xml.dom.minidom):
        """Return a DOM representation of this Node."""
        return self._dom_node(dom)

    def pformat(self, indent='    ', level=0):
        """Return an indented pseudo-XML representation, for test purposes."""
        raise NotImplementedError

    def copy(self):
        """Return a copy of self."""
        raise NotImplementedError

    def walk(self, visitor):
        """
        Traverse a tree of `Node` objects, calling ``visit_...`` methods of
        `visitor` when entering each node. If there is no
        ``visit_particular_node`` method for a node of type
        ``particular_node``, the ``unknown_visit`` method is called.  (The
        `walkabout()` method is similar, except it also calls ``depart_...``
        methods before exiting each node.)

        This tree traversal doesn't handle arbitrary in-place tree
        modifications.  Replacing one element with one element is OK.

        Within ``visit_...`` methods (and ``depart_...`` methods for
        `walkabout()`), `TreePruningException` subclasses may be raised
        (`SkipChildren`, `SkipSiblings`, `SkipNode`, `SkipDeparture`).

        Parameter `visitor`: A `NodeVisitor` object, containing a
        ``visit_...`` method for each `Node` subclass encountered.
        """
        name = 'visit_' + self.__class__.__name__
        method = getattr(visitor, name, visitor.unknown_visit)
        visitor.document.reporter.debug(name, category='nodes.Node.walk')
        try:
            method(self)
        except (SkipChildren, SkipNode):
            return
        except SkipDeparture:           # not applicable; ignore
            pass
        children = self.getchildren()
        try:
            for i in range(len(children)):
                children[i].walk(visitor)
        except SkipSiblings:
            pass

    def walkabout(self, visitor):
        """
        Perform a tree traversal similarly to `Node.walk()` (which see),
        except also call ``depart_...`` methods before exiting each node. If
        there is no ``depart_particular_node`` method for a node of type
        ``particular_node``, the ``unknown_departure`` method is called.

        Parameter `visitor`: A `NodeVisitor` object, containing ``visit_...``
        and ``depart_...`` methods for each `Node` subclass encountered.
        """
        call_depart = 1
        name = 'visit_' + self.__class__.__name__
        method = getattr(visitor, name, visitor.unknown_visit)
        visitor.document.reporter.debug(name, category='nodes.Node.walkabout')
        try:
            try:
                method(self)
            except SkipNode:
                return
            except SkipDeparture:
                call_depart = 0
            children = self.getchildren()
            try:
                for i in range(len(children)):
                    children[i].walkabout(visitor)
            except SkipSiblings:
                pass
        except SkipChildren:
            pass
        if call_depart:
            name = 'depart_' + self.__class__.__name__
            method = getattr(visitor, name, visitor.unknown_departure)
            visitor.document.reporter.debug(
                  name, category='nodes.Node.walkabout')
            method(self)


class Text(Node, MutableString):

    tagname = '#text'

    def __repr__(self):
        data = repr(self.data)
        if len(data) > 70:
            data = repr(self.data[:64] + ' ...')
        return '<%s: %s>' % (self.tagname, data)

    def shortrepr(self):
        data = repr(self.data)
        if len(data) > 20:
            data = repr(self.data[:16] + ' ...')
        return '<%s: %s>' % (self.tagname, data)

    def _dom_node(self, dom):
        return dom.Text(self.data)

    def _rooted_dom_node(self, domroot):
        return domroot.createTextNode(self.data)

    def astext(self):
        return self.data

    def copy(self):
        return self.__class__(self.data)

    def pformat(self, indent='    ', level=0):
        result = []
        indent = indent * level
        for line in self.data.splitlines():
            result.append(indent + line + '\n')
        return ''.join(result)

    def getchildren(self):
        """Text nodes have no children. Return []."""
        return []


class Element(Node):

    """
    `Element` is the superclass to all specific elements.

    Elements contain attributes and child nodes. Elements emulate dictionaries
    for attributes, indexing by attribute name (a string). To set the
    attribute 'att' to 'value', do::

        element['att'] = 'value'

    Elements also emulate lists for child nodes (element nodes and/or text
    nodes), indexing by integer. To get the first child node, use::

        element[0]

    Elements may be constructed using the ``+=`` operator. To add one new
    child node to element, do::

        element += node

    To add a list of multiple child nodes at once, use the same ``+=``
    operator::

        element += [node1, node2]
    """

    tagname = None
    """The element generic identifier. If None, it is set as an instance
    attribute to the name of the class."""

    child_text_separator = '\n\n'
    """Separator for child nodes, used by `astext()` method."""

    def __init__(self, rawsource='', *children, **attributes):
        self.rawsource = rawsource
        """The raw text from which this element was constructed."""

        self.children = []
        """List of child nodes (elements and/or `Text`)."""

        self.extend(children)           # maintain parent info

        self.attributes = {}
        """Dictionary of attribute {name: value}."""

        for att, value in attributes.items():
            self.attributes[att.lower()] = value

        if self.tagname is None:
            self.tagname = self.__class__.__name__

    def _dom_node(self, dom):
        element = dom.Element(self.tagname)
        for attribute, value in self.attributes.items():
            element.setAttribute(attribute, str(value))
        for child in self.children:
            element.appendChild(child._dom_node(dom))
        return element

    def _rooted_dom_node(self, domroot):
        element = domroot.createElement(self.tagname)
        for attribute, value in self.attributes.items():
            element.setAttribute(attribute, str(value))
        for child in self.children:
            element.appendChild(child._rooted_dom_node(domroot))
        return element

    def __repr__(self):
        data = ''
        for c in self.children:
            data += c.shortrepr()
            if len(data) > 60:
                data = data[:56] + ' ...'
                break
        if self.hasattr('name'):
            return '<%s "%s": %s>' % (self.__class__.__name__,
                                      self.attributes['name'], data)
        else:
            return '<%s: %s>' % (self.__class__.__name__, data)

    def shortrepr(self):
        if self.hasattr('name'):
            return '<%s "%s"...>' % (self.__class__.__name__,
                                      self.attributes['name'])
        else:
            return '<%s...>' % self.tagname

    def __str__(self):
        if self.children:
            return '%s%s%s' % (self.starttag(),
                                ''.join([str(c) for c in self.children]),
                                self.endtag())
        else:
            return self.emptytag()

    def starttag(self):
        parts = [self.tagname]
        for name, value in self.attlist():
            if value is None:           # boolean attribute
                parts.append(name)
            elif isinstance(value, ListType):
                values = [str(v) for v in value]
                parts.append('%s="%s"' % (name, ' '.join(values)))
            else:
                parts.append('%s="%s"' % (name, str(value)))
        return '<%s>' % ' '.join(parts)

    def endtag(self):
        return '</%s>' % self.tagname

    def emptytag(self):
        return '<%s/>' % ' '.join([self.tagname] +
                                  ['%s="%s"' % (n, v)
                                   for n, v in self.attlist()])

    def __len__(self):
        return len(self.children)

    def __getitem__(self, key):
        if isinstance(key, StringType):
            return self.attributes[key]
        elif isinstance(key, IntType):
            return self.children[key]
        elif isinstance(key, SliceType):
            assert key.step is None, 'cannot handle slice with stride'
            return self.children[key.start:key.stop]
        else:
            raise TypeError, ('element index must be an integer, a slice, or '
                              'an attribute name string')

    def __setitem__(self, key, item):
        if isinstance(key, StringType):
            self.attributes[key] = item
        elif isinstance(key, IntType):
            item.parent = self
            self.children[key] = item
        elif isinstance(key, SliceType):
            assert key.step is None, 'cannot handle slice with stride'
            for node in item:
                node.parent = self
            self.children[key.start:key.stop] = item
        else:
            raise TypeError, ('element index must be an integer, a slice, or '
                              'an attribute name string')

    def __delitem__(self, key):
        if isinstance(key, StringType):
            del self.attributes[key]
        elif isinstance(key, IntType):
            del self.children[key]
        elif isinstance(key, SliceType):
            assert key.step is None, 'cannot handle slice with stride'
            del self.children[key.start:key.stop]
        else:
            raise TypeError, ('element index must be an integer, a simple '
                              'slice, or an attribute name string')

    def __add__(self, other):
        return self.children + other

    def __radd__(self, other):
        return other + self.children

    def __iadd__(self, other):
        """Append a node or a list of nodes to `self.children`."""
        if isinstance(other, Node):
            other.parent = self
            self.children.append(other)
        elif other is not None:
            for node in other:
                node.parent = self
            self.children.extend(other)
        return self

    def astext(self):
        return self.child_text_separator.join(
              [child.astext() for child in self.children])

    def attlist(self):
        attlist = self.attributes.items()
        attlist.sort()
        return attlist

    def get(self, key, failobj=None):
        return self.attributes.get(key, failobj)

    def hasattr(self, attr):
        return self.attributes.has_key(attr)

    def delattr(self, attr):
        if self.attributes.has_key(attr):
            del self.attributes[attr]

    def setdefault(self, key, failobj=None):
        return self.attributes.setdefault(key, failobj)

    has_key = hasattr

    def append(self, item):
        item.parent = self
        self.children.append(item)

    def extend(self, item):
        for node in item:
            node.parent = self
        self.children.extend(item)

    def insert(self, i, item):
        assert isinstance(item, Node)
        item.parent = self
        self.children.insert(i, item)

    def pop(self, i=-1):
        return self.children.pop(i)

    def remove(self, item):
        self.children.remove(item)

    def index(self, item):
        return self.children.index(item)

    def replace(self, old, new):
        """Replace one child `Node` with another child or children."""
        index = self.index(old)
        if isinstance(new, Node):
            self[index] = new
        elif new is not None:
            self[index:index+1] = new

    def findclass(self, childclass, start=0, end=sys.maxint):
        """
        Return the index of the first child whose class exactly matches.

        Parameters:

        - `childclass`: A `Node` subclass to search for, or a tuple of `Node`
          classes. If a tuple, any of the classes may match.
        - `start`: Initial index to check.
        - `end`: Initial index to *not* check.
        """
        if not isinstance(childclass, TupleType):
            childclass = (childclass,)
        for index in range(start, min(len(self), end)):
            for c in childclass:
                if isinstance(self[index], c):
                    return index
        return None

    def findnonclass(self, childclass, start=0, end=sys.maxint):
        """
        Return the index of the first child whose class does *not* match.

        Parameters:

        - `childclass`: A `Node` subclass to skip, or a tuple of `Node`
          classes. If a tuple, none of the classes may match.
        - `start`: Initial index to check.
        - `end`: Initial index to *not* check.
        """
        if not isinstance(childclass, TupleType):
            childclass = (childclass,)
        for index in range(start, min(len(self), end)):
            match = 0
            for c in childclass:
                if isinstance(self.children[index], c):
                    match = 1
            if not match:
                return index
        return None

    def pformat(self, indent='    ', level=0):
        return ''.join(['%s%s\n' % (indent * level, self.starttag())] +
                       [child.pformat(indent, level+1)
                        for child in self.children])

    def getchildren(self):
        """Return this element's children."""
        return self.children

    def copy(self):
        return self.__class__(**self.attributes)


class TextElement(Element):

    """
    An element which directly contains text.

    Its children are all Text or TextElement nodes.
    """

    child_text_separator = ''
    """Separator for child nodes, used by `astext()` method."""

    def __init__(self, rawsource='', text='', *children, **attributes):
        if text != '':
            textnode = Text(text)
            Element.__init__(self, rawsource, textnode, *children,
                              **attributes)
        else:
            Element.__init__(self, rawsource, *children, **attributes)


# ========
#  Mixins
# ========

class Resolvable:

    resolved = 0


class BackLinkable:

    def add_backref(self, refid):
        self.setdefault('backrefs', []).append(refid)


# ====================
#  Element Categories
# ====================

class Root: pass

class Titular: pass

class Bibliographic: pass


class PreBibliographic:
    """Category of Node which may occur before Bibliographic Nodes."""
    pass


class Structural: pass

class Body: pass

class General(Body): pass

class Sequential(Body): pass

class Admonition(Body): pass


class Special(Body):
    """Special internal body elements."""
    pass


class Part: pass

class Inline: pass

class Referential(Resolvable): pass
    #refnode = None
    #"""Resolved reference to a node."""


class Targetable(Resolvable):

    referenced = 0


# ==============
#  Root Element
# ==============

class document(Root, Structural, Element):

    def __init__(self, reporter, language_code, *args, **kwargs):
        Element.__init__(self, *args, **kwargs)

        self.reporter = reporter
        """System message generator."""

        self.language_code = language_code
        """ISO 639 2-letter language identifier."""

        self.explicit_targets = {}
        """Mapping of target names to explicit target nodes."""

        self.implicit_targets = {}
        """Mapping of target names to implicit (internal) target
        nodes."""

        self.external_targets = []
        """List of external target nodes."""

        self.internal_targets = []
        """List of internal target nodes."""

        self.indirect_targets = []
        """List of indirect target nodes."""

        self.substitution_defs = {}
        """Mapping of substitution names to substitution_definition nodes."""

        self.refnames = {}
        """Mapping of names to lists of referencing nodes."""

        self.refids = {}
        """Mapping of ids to lists of referencing nodes."""

        self.nameids = {}
        """Mapping of names to unique id's."""

        self.ids = {}
        """Mapping of ids to nodes."""

        self.substitution_refs = {}
        """Mapping of substitution names to lists of substitution_reference
        nodes."""

        self.footnote_refs = {}
        """Mapping of footnote labels to lists of footnote_reference nodes."""

        self.citation_refs = {}
        """Mapping of citation labels to lists of citation_reference nodes."""

        self.anonymous_targets = []
        """List of anonymous target nodes."""

        self.anonymous_refs = []
        """List of anonymous reference nodes."""

        self.autofootnotes = []
        """List of auto-numbered footnote nodes."""

        self.autofootnote_refs = []
        """List of auto-numbered footnote_reference nodes."""

        self.symbol_footnotes = []
        """List of symbol footnote nodes."""

        self.symbol_footnote_refs = []
        """List of symbol footnote_reference nodes."""

        self.footnotes = []
        """List of manually-numbered footnote nodes."""

        self.citations = []
        """List of citation nodes."""

        self.pending = []
        """List of pending elements @@@."""

        self.autofootnote_start = 1
        """Initial auto-numbered footnote number."""

        self.symbol_footnote_start = 0
        """Initial symbol footnote symbol index."""

        self.id_start = 1
        """Initial ID number."""

        self.messages = Element()
        """System messages generated after parsing."""

    def asdom(self, dom=xml.dom.minidom):
        domroot = dom.Document()
        domroot.appendChild(Element._rooted_dom_node(self, domroot))
        return domroot

    def set_id(self, node, msgnode=None):
        if msgnode == None:
            msgnode = self.messages
        if node.has_key('id'):
            id = node['id']
            if self.ids.has_key(id) and self.ids[id] is not node:
                msg = self.reporter.severe('Duplicate ID: "%s".' % id)
                msgnode += msg
        else:
            if node.has_key('name'):
                id = utils.id(node['name'])
            else:
                id = ''
            while not id or self.ids.has_key(id):
                id = 'id%s' % self.id_start
                self.id_start += 1
            node['id'] = id
        self.ids[id] = node
        if node.has_key('name'):
            self.nameids[node['name']] = id
        return id

    def note_implicit_target(self, target, msgnode=None):
        if msgnode == None:
            msgnode = self.messages
        id = self.set_id(target, msgnode)
        name = target['name']
        if self.explicit_targets.has_key(name) \
              or self.implicit_targets.has_key(name):
            msg = self.reporter.info(
                  'Duplicate implicit target name: "%s".' % name, backrefs=[id])
            msgnode += msg
            self.clear_target_names(name, self.implicit_targets)
            del target['name']
            target['dupname'] = name
            self.implicit_targets[name] = None
        else:
            self.implicit_targets[name] = target

    def note_explicit_target(self, target, msgnode=None):
        if msgnode == None:
            msgnode = self.messages
        id = self.set_id(target, msgnode)
        name = target['name']
        if self.explicit_targets.has_key(name):
            level = 2
            if target.has_key('refuri'): # external target, dups OK
                refuri = target['refuri']
                t = self.explicit_targets[name]
                if t.has_key('name') and t.has_key('refuri') \
                      and t['refuri'] == refuri:
                    level = 1           # just inform if refuri's identical
            msg = self.reporter.system_message(
                  level, 'Duplicate explicit target name: "%s".' % name,
                  backrefs=[id])
            msgnode += msg
            self.clear_target_names(name, self.explicit_targets,
                                    self.implicit_targets)
            if level > 1:
                del target['name']
                target['dupname'] = name
        elif self.implicit_targets.has_key(name):
            msg = self.reporter.info(
                  'Duplicate implicit target name: "%s".' % name, backrefs=[id])
            msgnode += msg
            self.clear_target_names(name, self.implicit_targets)
        self.explicit_targets[name] = target

    def clear_target_names(self, name, *targetdicts):
        for targetdict in targetdicts:
            if not targetdict.has_key(name):
                continue
            node = targetdict[name]
            if node.has_key('name'):
                node['dupname'] = node['name']
                del node['name']

    def note_refname(self, node):
        self.refnames.setdefault(node['refname'], []).append(node)

    def note_refid(self, node):
        self.refids.setdefault(node['refid'], []).append(node)

    def note_external_target(self, target):
        self.external_targets.append(target)

    def note_internal_target(self, target):
        self.internal_targets.append(target)

    def note_indirect_target(self, target):
        self.indirect_targets.append(target)
        if target.has_key('name'):
            self.note_refname(target)

    def note_anonymous_target(self, target):
        self.set_id(target)
        self.anonymous_targets.append(target)

    def note_anonymous_ref(self, ref):
        self.anonymous_refs.append(ref)

    def note_autofootnote(self, footnote):
        self.set_id(footnote)
        self.autofootnotes.append(footnote)

    def note_autofootnote_ref(self, ref):
        self.set_id(ref)
        self.autofootnote_refs.append(ref)

    def note_symbol_footnote(self, footnote):
        self.set_id(footnote)
        self.symbol_footnotes.append(footnote)

    def note_symbol_footnote_ref(self, ref):
        self.set_id(ref)
        self.symbol_footnote_refs.append(ref)

    def note_footnote(self, footnote):
        self.set_id(footnote)
        self.footnotes.append(footnote)

    def note_footnote_ref(self, ref):
        self.set_id(ref)
        self.footnote_refs.setdefault(ref['refname'], []).append(ref)
        self.note_refname(ref)

    def note_citation(self, citation):
        self.set_id(citation)
        self.citations.append(citation)

    def note_citation_ref(self, ref):
        self.set_id(ref)
        self.citation_refs.setdefault(ref['refname'], []).append(ref)
        self.note_refname(ref)

    def note_substitution_def(self, subdef, msgnode=None):
        name = subdef['name']
        if self.substitution_defs.has_key(name):
            msg = self.reporter.error(
                  'Duplicate substitution definition name: "%s".' % name)
            if msgnode == None:
                msgnode = self.messages
            msgnode += msg
            oldnode = self.substitution_defs[name]
            oldnode['dupname'] = oldnode['name']
            del oldnode['name']
        # keep only the last definition
        self.substitution_defs[name] = subdef

    def note_substitution_ref(self, subref):
        self.substitution_refs.setdefault(
              subref['refname'], []).append(subref)

    def note_pending(self, pending):
        self.pending.append(pending)

    def copy(self):
        return self.__class__(self.reporter, self.language_code,
                              **self.attributes)


# ================
#  Title Elements
# ================

class title(Titular, PreBibliographic, TextElement): pass
class subtitle(Titular, PreBibliographic, TextElement): pass


# ========================
#  Bibliographic Elements
# ========================

class docinfo(Bibliographic, Element): pass
class author(Bibliographic, TextElement): pass
class authors(Bibliographic, Element): pass
class organization(Bibliographic, TextElement): pass
class contact(Bibliographic, TextElement): pass
class version(Bibliographic, TextElement): pass
class revision(Bibliographic, TextElement): pass
class status(Bibliographic, TextElement): pass
class date(Bibliographic, TextElement): pass
class copyright(Bibliographic, TextElement): pass


# =====================
#  Structural Elements
# =====================

class section(Structural, Element): pass

class topic(Structural, Element):

    """
    Topics are terminal, "leaf" mini-sections, like block quotes with titles,
    or textual figures. A topic is just like a section, except that it has no
    subsections, and it doesn't have to conform to section placement rules.

    Topics are allowed wherever body elements (list, table, etc.) are allowed,
    but only at the top level of a section or document. Topics cannot nest
    inside topics or body elements; you can't have a topic inside a table,
    list, block quote, etc.
    """

    pass


class transition(Structural, Element): pass


# ===============
#  Body Elements
# ===============

class paragraph(General, TextElement): pass
class bullet_list(Sequential, Element): pass
class enumerated_list(Sequential, Element): pass
class list_item(Part, Element): pass
class definition_list(Sequential, Element): pass
class definition_list_item(Part, Element): pass
class term(Part, TextElement): pass
class classifier(Part, TextElement): pass
class definition(Part, Element): pass
class field_list(Sequential, Element): pass
class field(Part, Element): pass
class field_name(Part, TextElement): pass
class field_argument(Part, TextElement): pass
class field_body(Part, Element): pass


class option(Part, Element):

    child_text_separator = ''


class option_argument(Part, TextElement):

    def astext(self):
        return self.get('delimiter', ' ') + TextElement.astext(self)


class option_group(Part, Element):

    child_text_separator = ', '


class option_list(Sequential, Element): pass


class option_list_item(Part, Element):

    child_text_separator = '  '


class option_string(Part, TextElement): pass
class description(Part, Element): pass
class literal_block(General, TextElement): pass
class block_quote(General, Element): pass
class doctest_block(General, TextElement): pass
class attention(Admonition, Element): pass
class caution(Admonition, Element): pass
class danger(Admonition, Element): pass
class error(Admonition, Element): pass
class important(Admonition, Element): pass
class note(Admonition, Element): pass
class tip(Admonition, Element): pass
class hint(Admonition, Element): pass
class warning(Admonition, Element): pass
class comment(Special, PreBibliographic, TextElement): pass
class substitution_definition(Special, TextElement): pass
class target(Special, Inline, TextElement, Targetable): pass
class footnote(General, Element, BackLinkable): pass
class citation(General, Element, BackLinkable): pass
class label(Part, TextElement): pass
class figure(General, Element): pass
class caption(Part, TextElement): pass
class legend(Part, Element): pass
class table(General, Element): pass
class tgroup(Part, Element): pass
class colspec(Part, Element): pass
class thead(Part, Element): pass
class tbody(Part, Element): pass
class row(Part, Element): pass
class entry(Part, Element): pass


class system_message(Special, PreBibliographic, Element, BackLinkable):

    def __init__(self, comment=None, *children, **attributes):
        if comment:
            p = paragraph('', comment)
            children = (p,) + children
        Element.__init__(self, '', *children, **attributes)

    def astext(self):
        return '%s (%s) %s' % (self['type'], self['level'],
                               Element.astext(self))


class pending(Special, PreBibliographic, Element):

    """
    The "pending" element is used to encapsulate a pending operation: the
    operation, the point at which to apply it, and any data it requires.  Only
    the pending operation's location within the document is stored in the
    public document tree; the operation itself and its data are stored in
    internal instance attributes.

    For example, say you want a table of contents in your reStructuredText
    document.  The easiest way to specify where to put it is from within the
    document, with a directive::

        .. contents::

    But the "contents" directive can't do its work until the entire document
    has been parsed (and possibly transformed to some extent).  So the
    directive code leaves a placeholder behind that will trigger the second
    phase of the its processing, something like this::

        <pending ...public attributes...> + internal attributes

    The "pending" node is also appended to `document.pending`, so that a later
    stage of processing can easily run all pending transforms.
    """

    def __init__(self, transform, stage, details,
                 rawsource='', *children, **attributes):
        Element.__init__(self, rawsource, *children, **attributes)

        self.transform = transform
        """The `docutils.transforms.Transform` class implementing the pending
        operation."""

        self.stage = stage
        """The stage of processing when the function will be called."""

        self.details = details
        """Detail data (dictionary) required by the pending operation."""

    def pformat(self, indent='    ', level=0):
        internals = [
              '.. internal attributes:',
              '     .transform: %s.%s' % (self.transform.__module__,
                                                 self.transform.__name__),
              '     .stage: %r' % self.stage,
              '     .details:']
        details = self.details.items()
        details.sort()
        for key, value in details:
            if isinstance(value, Node):
                internals.append('%7s%s:' % ('', key))
                internals.extend(['%9s%s' % ('', line)
                                  for line in value.pformat().splitlines()])
            elif value and type(value) == ListType \
                  and isinstance(value[0], Node):
                internals.append('%7s%s:' % ('', key))
                for v in value:
                    internals.extend(['%9s%s' % ('', line)
                                      for line in v.pformat().splitlines()])
            else:
                internals.append('%7s%s: %r' % ('', key, value))
        return (Element.pformat(self, indent, level)
                + ''.join([('    %s%s\n' % (indent * level, line))
                           for line in internals]))

    def copy(self):
        return self.__class__(self.transform, self.stage, self.details,
                              **self.attributes)


class raw(Special, Inline, PreBibliographic, TextElement):

    """
    Raw data that is to be passed untouched to the Writer.
    """

    pass


# =================
#  Inline Elements
# =================

class emphasis(Inline, TextElement): pass
class strong(Inline, TextElement): pass
class interpreted(Inline, Referential, TextElement): pass
class literal(Inline, TextElement): pass
class reference(Inline, Referential, TextElement): pass
class footnote_reference(Inline, Referential, TextElement): pass
class citation_reference(Inline, Referential, TextElement): pass
class substitution_reference(Inline, TextElement): pass


class image(General, Inline, TextElement):

    def astext(self):
        return self.get('alt', '')


class problematic(Inline, TextElement): pass


# ========================================
#  Auxiliary Classes, Functions, and Data
# ========================================

node_class_names = """
    Text
    attention author authors
    block_quote bullet_list
    caption caution citation citation_reference classifier colspec
        comment contact copyright
    danger date definition definition_list definition_list_item
        description docinfo doctest_block document
    emphasis entry enumerated_list error
    field field_argument field_body field_list field_name figure
        footnote footnote_reference
    hint
    image important interpreted
    label legend list_item literal literal_block
    note
    option option_argument option_group option_list option_list_item
        option_string organization
    paragraph pending problematic
    raw reference revision row
    section status strong substitution_definition
        substitution_reference subtitle system_message
    table target tbody term tgroup thead tip title topic transition
    version
    warning""".split()
"""A list of names of all concrete Node subclasses."""


class NodeVisitor:

    """
    "Visitor" pattern [GoF95]_ abstract superclass implementation for document
    tree traversals.

    Each node class has corresponding methods, doing nothing by default;
    override individual methods for specific and useful behaviour. The
    "``visit_`` + node class name" method is called by `Node.walk()` upon
    entering a node. `Node.walkabout()` also calls the "``depart_`` + node
    class name" method before exiting a node.

    .. [GoF95] Gamma, Helm, Johnson, Vlissides. *Design Patterns: Elements of
       Reusable Object-Oriented Software*. Addison-Wesley, Reading, MA, USA,
       1995.
    """

    def __init__(self, document):
        self.document = document

    def unknown_visit(self, node):
        """
        Called when entering unknown `Node` types.

        Raise an exception unless overridden.
        """
        raise NotImplementedError('visiting unknown node type: %s'
                                  % node.__class__.__name__)

    def unknown_departure(self, node):
        """
        Called before exiting unknown `Node` types.

        Raise exception unless overridden.
        """
        raise NotImplementedError('departing unknown node type: %s'
                                  % node.__class__.__name__)

    # Save typing with dynamic definitions.
    for name in node_class_names:
        exec """def visit_%s(self, node): pass\n""" % name
        exec """def depart_%s(self, node): pass\n""" % name
    del name


class GenericNodeVisitor(NodeVisitor):

    """
    Generic "Visitor" abstract superclass, for simple traversals.

    Unless overridden, each ``visit_...`` method calls `default_visit()`, and
    each ``depart_...`` method (when using `Node.walkabout()`) calls
    `default_departure()`. `default_visit()` (and `default_departure()`) must
    be overridden in subclasses.

    Define fully generic visitors by overriding `default_visit()` (and
    `default_departure()`) only. Define semi-generic visitors by overriding
    individual ``visit_...()`` (and ``depart_...()``) methods also.

    `NodeVisitor.unknown_visit()` (`NodeVisitor.unknown_departure()`) should
    be overridden for default behavior.
    """

    def default_visit(self, node):
        """Override for generic, uniform traversals."""
        raise NotImplementedError

    def default_departure(self, node):
        """Override for generic, uniform traversals."""
        raise NotImplementedError

    # Save typing with dynamic definitions.
    for name in node_class_names:
        exec """def visit_%s(self, node):
                    self.default_visit(node)\n""" % name
        exec """def depart_%s(self, node):
                    self.default_departure(node)\n""" % name
    del name


class TreeCopyVisitor(GenericNodeVisitor):

    """
    Make a complete copy of a tree or branch, including element attributes.
    """

    def __init__(self, document):
        GenericNodeVisitor.__init__(self, document)
        self.parent_stack = [[]]

    def get_tree_copy(self):
        return self.parent_stack[0][0]

    def default_visit(self, node):
        """Copy the current node, and make it the new acting parent."""
        newnode = node.copy()
        self.parent_stack[-1].append(newnode)
        self.parent_stack.append(newnode)

    def default_departure(self, node):
        """Restore the previous acting parent."""
        self.parent_stack.pop()


class TreePruningException(Exception):

    """
    Base class for `NodeVisitor`-related tree pruning exceptions.

    Raise subclasses from within ``visit_...`` or ``depart_...`` methods
    called from `Node.walk()` and `Node.walkabout()` tree traversals to prune
    the tree traversed.
    """

    pass


class SkipChildren(TreePruningException):

    """
    Do not visit any children of the current node.  The current node's
    siblings and ``depart_...`` method are not affected.
    """

    pass


class SkipSiblings(TreePruningException):

    """
    Do not visit any more siblings (to the right) of the current node.  The
    current node's children and its ``depart_...`` method are not affected.
    """

    pass


class SkipNode(TreePruningException):

    """
    Do not visit the current node's children, and do not call the current
    node's ``depart_...`` method.
    """

    pass


class SkipDeparture(TreePruningException):

    """
    Do not call the current node's ``depart_...`` method.  The current node's
    children and siblings are not affected.
    """

    pass
