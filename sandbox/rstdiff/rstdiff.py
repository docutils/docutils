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
Generates a structural diff from two reStructuredText input documents
and produces an annotated result.
"""

__docformat__ = 'reStructuredText'

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

import os, re, sys

import docutils
from docutils import frontend, writers, nodes, SettingsSpec
from docutils.core import Publisher
from docutils.utils import SystemMessage, Reporter, new_reporter, new_document
from docutils.frontend import OptionParser, make_paths_absolute
from docutils.transforms import Transform

from treediff import TreeMatcher, HashableNodeImpl

###############################################################################
###############################################################################
# Command line specification

description = ("""Generates a structural diff from two reStructuredText input
documents and produces an annotated result.  """)

writerOption = 'writer'
writerDefault = 'xml'
writerArgRE1 = '^--' + writerOption + '=' + '(.*)$'

settings_spec = (
    'rstdiff options',
    None,
    (('Select writer to write output with (default "xml").',
      ['--' + writerOption],
      {}),
     )
    )

settings_defaults = {'output_encoding_error_handler': 'xmlcharrefreplace',
                     writerOption: writerDefault}

config_section = 'rstdiff'

usage = '%prog [options]... <old> [<new> [<output>]]'

###############################################################################
# Classes for three argument command lines

class Publisher3Args(Publisher):

    def setup_option_parser(self, usage=None, description=None,
                            settings_spec=None, config_section=None,
                            **defaults):
        if config_section:
            if not settings_spec:
                settings_spec = SettingsSpec()
            settings_spec.config_section = config_section
            parts = config_section.split()
            if len(parts) > 1 and parts[-1] == 'application':
                settings_spec.config_section_dependencies = ['applications']
        #@@@ Add self.source & self.destination to components in future?
        option_parser = OptionParser3Args(
            components=(self.parser, self.reader, self.writer, settings_spec),
            defaults=defaults, read_config_files=1,
            usage=usage, description=description)
        return option_parser

class OptionParser3Args(OptionParser):

    def check_values(self, values, args):
        """Store positional arguments as runtime settings."""
        values._old_source, values._new_source, values._destination = self.check_args(args)
        make_paths_absolute(values.__dict__, self.relative_path_settings,
                            os.getcwd())
        values._config_files = self.config_files
        return values

    def check_args(self, args):
        old_source = new_source = destination = None
        if not args:
            self.error('At least 1 argument required.')
        else:
            old_source = args.pop(0)
            if old_source == '-':           # means stdin
                old_source = None
        if args:
            new_source = args.pop(0)
            if new_source == '-':           # means stdin
                new_source = None
        if args:
            destination = args.pop(0)
            if destination == '-':      # means stdout
                destination = None
        if args:
            self.error('Maximum 3 arguments allowed.')
        if old_source is None and new_source is None:
            self.error('Old and new source may not both use stdin.')
        if (old_source and old_source == destination
            or new_source and new_source == destination):
            self.error('Do not specify the same file for both source and '
                       'destination.  It will clobber the source file.')
        return old_source, new_source, destination

###############################################################################
###############################################################################
# Helpers

class Opcode(object):
    """Encapsulates opcodes as returned by `TreeMatcher.get_opcodes()`"""

    Replace = 'replace'
    Delete = 'delete'
    Insert = 'insert'
    Equal = 'equal'
    Descend = 'descend'

    _tuple = None

    def __init__(self, opcodeTuple):
        """Initialize from a tuple returned by `TreeMatcher.get_opcodes()`"""
        self._tuple = list(opcodeTuple)

    def getCommand(self):
        """Return the command."""
        return self._tuple[0]

    def getOldRange(self):
        """Returns the range pertaining to an old list."""
        return ( self._tuple[1], self._tuple[2], )

    def getNewRange(self):
        """Returns the range pertaining to a new list."""
        return ( self._tuple[3], self._tuple[4], )

    def getSubOpcodes(self):
        """Return the sub-opcodes in case of `command` == 'descend' or
        `None`."""
        if self._tuple[0] != self.Descend:
            return None
        return self._tuple[5]

    def resolveOpcode(self, oldList, newList):
        """Resolves opcode pertaining to `oldList` and `newList`. Returns tuple
        consisting of

        command
          Same as self.getCommand().

        oldRange
          The range of elements in `oldList` affected by the opcode.

        newRange
          The range of elements in `newList` affected by the opcode.

        subOpcodes
          Same as self.getSubOpcodes().
        """
        oldRange = self.getOldRange()
        newRange = self.getNewRange()
        return ( self.getCommand(), oldList[oldRange[0]:oldRange[1]],
                 newList[newRange[0]:newRange[1]], self.getSubOpcodes())

    def setSubOpcodes(self, opcodes):
        """Set the sub-opcodes to a new list."""
        if self._tuple[0] != self.Descend:
            raise TypeError("Can not set subopcodes of a %r opcode"
                            % ( self._tuple[0], ))
        self._tuple[5] = opcodes

    def setCommand(self, command):
        """Set a new command adapting subopcodes."""
        if self._tuple[0] == command:
            return
        self._tuple[0] = command
        if command == self.Descend:
            self._tuple[5] = [ ]
        else:
            self._tuple = self._tuple[0:5]

    def setOldRange(self, range):
        """Sets the range pertaining to an old list."""
        ( self._tuple[1], self._tuple[2], ) = range

    def setNewRange(self, range):
        """Sets the range pertaining to a new list."""
        ( self._tuple[3], self._tuple[4], ) = range

    def asTuple(self):
        """Return the opcode as a tuple."""
        return tuple(self._tuple)

###############################################################################
###############################################################################
# Additional docutils stuff

###############################################################################
# Node types

class White(nodes.Text):
    """A piece of text containing only whitespace."""

    tagname = '#white'

    """A regular expression matching strings for this class and returning
    them as the first match."""
    # TODO Could be subject to an option
    re = '(\\s+)'

class Word(nodes.Text):
    """A piece of text containing exactly one word."""

    tagname = '#word'

    @staticmethod
    def splitText(text):
        """Splits text and returns a sequence of `Word` and `White`
        objects. Returns an empty sequence for an empty `text`."""

        subs = re.split(White.re, text.astext())
        result = [ ]
        if not subs:
            return result
        elif re.match(White.re, subs[0]):
            ( current, next, ) = ( White, Word, )
        else:
            ( current, next, ) = ( Word, White, )
        for sub in subs:
            result.append(current(sub))
            ( current, next, ) = ( next, current, )
        return result

###############################################################################
# Transformers

class Text2Words(Transform):
    """Transforms a `Text` node into a sequence of `Word`/`White`."""

    def apply(self):
        self.document.walk(Text2WordsVisitor(self.document))

class Text2WordsVisitor(nodes.SparseNodeVisitor):

    def visit_Text(self, text):
        words = Word.splitText(text)
        if not words:
            # An empty text
            words = [ White(''), ]
        text.parent.replace(text, words)

class Words2Text(Transform):
    """Transforms a sequence of `Word`/`White` into a `Text` node."""

    def apply(self):
        self.document.walk(Words2TextVisitor(self.document))

class Words2TextVisitor(nodes.SparseNodeVisitor):

    def visit_Text(self, text):
        parent = text.parent
        # Find this node and the first node of the sequence it belongs to
        first = None
        for i in range(len(parent)):
            if not isinstance(parent[i], nodes.Text):
                first = None
            elif first is None:
                first = i
            # ``parent.index(text)`` uses value equality - can not be
            # used here to find `text`
            if id(parent[i]) == id(text):
                end = i + 1
                break
        else:
            raise IndexError("Can not find %r in its parent" % ( text, ))

        if (len(parent) > end
            and isinstance(parent[end], nodes.Text)):
            # The visitor processes following children even if they are
            # deleted - so work for last node of a sequence
            return

        texts = nodes.Text(reduce(lambda s, node: s + node.astext(),
                                 parent[first:end], ""))
        parent[first:end] = ( texts, )

    visit_White = visit_Text

    visit_Word = visit_Text

###############################################################################
###############################################################################
# Hashable

class DocutilsDispatcher(HashableNodeImpl): 
    """Implements hashable for a docutils `Node` and supports construction."""

    debug = False

    def __init__(self):
        super(self.__class__, self).__init__(nodes.Node)

    def dispatchClass(self, function, node, *args):
        """Dispatch a call of type `function` for the class of `node` using
        arguments `node` and `args`. Default is to dispatch for imaginary class
        "UNKNOWN"."""
        pat = "%s_%%s" % ( function, )
        try:
            name = pat % ( node.__class__.__name__, )
            method = getattr(self, name)
        except AttributeError:
            name = pat % ( 'UNKNOWN', )
            method = getattr(self, name)
        if self.debug:
            print("*** %s(%s)"
                  % ( name, ", ".join([ arg.__class__.__name__
                                        for arg in ( node, ) + args ]), ))
            for arg in ( node, ) + args:
                print("    > %s" % ( arg, ))
        result = method(node, *args)
        if self.debug:
            print("    < %s" % ( result, ))
        return result

    ###########################################################################
    ###########################################################################
    # Implementation of abstract methods for `HashableNodeImpl`

    def rootHash(self, node):
        """Return a hash for the root only. Subclasses must override
        this."""
        return self.dispatchClass('rootHash', node)

    def rootHash_UNKNOWN(self, node):
        return hash(node.__class__)

    def rootEq(self, node, other):
        """Returns root equality of `node` and an `other` node. ``True`` if
        the two nodes as roots are equal without considering their
        children. This should be true if one node can be replaced by
        the other and all changes can be represented without changing
        the node itself. Subclasses must override this."""
        # Only nodes of the same class can be equal - this assumption
        # is used in many places
        if node.__class__ != other.__class__:
            return False
        return self.dispatchClass('rootEq', node, other)

    def rootEq_UNKNOWN(self, node, other):
        # Unless we know better two roots of the same type are considered equal
        return True

    def childHash(self, node):
        """Return a hash for the node as a child. Subclasses must override
        this."""
        return self.dispatchClass('childHash', node)

    def childHash_UNKNOWN(self, node):
        # By default compare as a child by comparing children
        return self.childrenHash(node)

    def childEq(self, node, other):
        """Returns equality of `node` and an `other` node as children.
        ``True`` if the child features of the two nodes are equal
        without considering the root. Subclasses must override
        this."""
        # Only nodes of the same class can be equal - this assumption
        # is used in many places
        if node.__class__ != other.__class__:
            return False
        return self.dispatchClass('childEq', node, other)

    def childEq_UNKNOWN(self, node, other):
        # By default compare as a child by comparing children
        return self.childrenEq(node, other)

    def getChildren(self, node):
        """Return the children of `node` as a list. Subclasses must override
        this."""
        return self.dispatchClass('getChildren', node)

    def getChildren_UNKNOWN(self, node):
        return node.children

    ###########################################################################
    ###########################################################################
    # Merging

    # TODO The resulting class names should be configurable
    NewDelete = 'removed'
    NewInsert = 'added'
    NewReplaced = 'replaced'
    NewReplacement = 'replacement'

    def copyRoot(self, node):
        """Copy `node` as root and return it."""
        return self.dispatchClass('copyRoot', node)

    def copyRoot_UNKNOWN(self, node):
        return node.copy()

    def addChild(self, root, child):
        """Add `child` to `root`."""
        return self.dispatchClass('addChild', root, child)

    def addChild_UNKNOWN(self, root, child):
        root.append(child)

    def copyChild(self, node, newType):
        """Copy `node` as child and return it. `newType` is ``None`` for an
        unchanged child or the change type."""
        return self.dispatchClass('copyChild', node, newType)

    def copyChild_UNKNOWN(self, node, newType):
        return self.setNewType(node.deepcopy(), newType)

    def copyChildren(self, head, tail, root, newType):
        """Return a range of new nodes copied from [ `head` ] + `tail` under
        `root`. `tail` are all the same class as `head`. Nodes are
        created approproate to type `newType`."""
        return self.dispatchClass('copyChildren', head, tail, root, newType)

    def copyChildren_UNKNOWN(self, head, tail, root, newType):
        return [ self.copyChild(child, newType)
                 for child in [ head, ] + tail ]

    def copyRange(self, root, children, newType):
        """Return a range of new nodes copied from `children` under `root`.
        Nodes are created appropriate to type `newType`."""
        result = [ ]
        begin = 0
        while begin < len(children):
            first = children[begin]
            end = begin + 1
            while end < len(children):
                last = children[end]
                if not(first.__class__ == last.__class__
                    or (isinstance(first, nodes.Text)
                        and isinstance(last, nodes.Text))):
                    break
                end += 1
            result.extend(self.copyChildren(first, children[begin + 1:end],
                                            root, newType))
            begin = end
        return result

    def mergeChildren(self, diffRoot, oldRoot, newRoot,
                      command, oldRange, newRange):
        """Add children to `diffRoot` merging children `oldRange` / `newRange`
        of `oldRoot` / `newRoot` by `command`."""
        if command == Opcode.Equal:
            for old in oldRange:
                self.addChild(diffRoot, self.copyChild(old, None))
        elif command == Opcode.Insert or command == Opcode.Delete:
            if command == Opcode.Insert:
                srcRoot = newRoot
                srcRange = newRange
                newType = self.NewInsert
            else:
                srcRoot = oldRoot
                srcRange = oldRange
                newType = self.NewDelete
            for newChild in self.copyRange(srcRoot, srcRange, newType):
                self.addChild(diffRoot, newChild)
        elif command == Opcode.Replace:
            # TODO Replacement doubles elements. This needs to be
            # reflected properly in the @ids. If the @ids don't change
            # there need to be unique @ids for replaced elements. This
            # needs also to be reflected in referring @refid and
            # @backrefs.
            for newChild in self.copyRange(oldRoot, oldRange,
                                           self.NewReplaced):
                self.addChild(diffRoot, newChild)
            for newChild in self.copyRange(newRoot, newRange,
                                           self.NewReplacement):
                self.addChild(diffRoot, newChild)
        else:
            raise TypeError("Unhandled command %r" % ( command, ))

    ###########################################################################
    ###########################################################################
    # Helpers

    def setNewType(self, node, newType):
        """Set a class on `node` for `newType` if set. Returns `node`."""
        if newType:
            node['classes'].append("change-%s" % ( newType, ))
        return node

    ###########################################################################
    ###########################################################################
    # Real comparison and merging

    # The idea is like this: Each node has attributes which need to be
    # compared as root and it has attributes which need to be compared
    # as child. This is different for every node type.
    #
    # Similarly each node type may need special methods for cloning
    # and merging.

    ###########################################################################
    # Text / Word / White

    def rootHash_Text(self, node):
        return hash(node.astext())

    rootHash_Word = rootHash_Text

    def rootHash_White(self, node):
        # Whitespace compares all equal
        return hash('')

    def rootEq_Text(self, node, other):
        return node.astext() == other.astext()

    rootEq_Word = rootEq_Text

    def rootEq_White(self, node, other):
        # TODO Must behave different for places where whitespace
        # differences are relevant
        return True

    # Text behaves the same as root or child

    childHash_Text = rootHash_Text
    childHash_Word = rootHash_Word
    childHash_White = rootHash_White

    childEq_Text = rootEq_Text
    childEq_Word = rootEq_Word
    childEq_White = rootEq_White

    def copyChildren_Text(self, head, tail, root, newType):
        if not tail and isinstance(head, nodes.Text) and not head.astext():
            # Do not create empty inlines
            return [ ]
        inline = nodes.inline()
        self.setNewType(inline, newType)
        inline.extend([ head, ] + tail)
        return [ inline, ]

    # Sequences of Text are treated together
    copyChildren_Word = copyChildren_Text
    copyChildren_White = copyChildren_Text

    ###########################################################################
    # For some elements their attributes need to be considered to
    # detect changes.

    def attributeEq(self, node, other, attribute):
        if (attribute in node) != (attribute in other):
            return False
        if not attribute in node:
            return True
        return node[attribute] == other[attribute]

    ###########################################################################
    # reference

    def rootEq_reference(self, node, other):
        return self.attributeEq(node, other, 'refuri')

    ###########################################################################
    # target

    def rootEq_target(self, node, other):
        return self.attributeEq(node, other, 'refuri')

    ###########################################################################
    # bullet_list

    # TODO This is typically a minor change and should be requested by
    # a special option

    def attributeEq_bullet_list(self, node, other):
        return self.attributeEq(node, other, 'bullet')

    def rootEq_bullet_list(self, node, other):
        return self.attributeEq_bullet_list(node, other)

    def childEq_bullet_list(self, node, other):
        return (self.attributeEq_bullet_list(node, other)
                and self.childrenEq(node, other))

    ###########################################################################
    # enumerated_list

    # TODO This is typically a minor change and should be requested by
    # a special option

    def attributeEq_enumerated_list(self, node, other):
        return (self.attributeEq(node, other, 'enumtype')
                and self.attributeEq(node, other, 'prefix')
                and self.attributeEq(node, other, 'suffix')
                and self.attributeEq(node, other, 'start'))

    def rootEq_enumerated_list(self, node, other):
        return self.attributeEq_enumerated_list(node, other)

    def childEq_enumerated_list(self, node, other):
        return (self.attributeEq_enumerated_list(node, other)
                and self.childrenEq(node, other))

    ###########################################################################
    # image

    def rootEq_image(self, node, other):
        if node.__class__ != other.__class__:
            return False
        return self.attributeEq(node, other, 'uri')

    ###########################################################################
    # Some elements may contain only #PCDATA. They need to propagate
    # changes in their children up to the element itself.

    def rootEqWithChildren(self, node, other):
        if node.__class__ != other.__class__:
            return False
        return self.childrenEq(node, other)

    ###########################################################################
    # comment

    rootEq_comment = rootEqWithChildren

    ###########################################################################
    # literal

    rootEq_literal = rootEqWithChildren

    ###########################################################################
    # option_string

    rootEq_option_string = rootEqWithChildren

    ###########################################################################
    # label

    # TODO This is typically a minor change and should be requested by
    # a special option
   
    rootEq_label = rootEqWithChildren

    ###########################################################################
    # footnote_reference

    # TODO This is typically a minor change and should be requested by
    # a special option
   
    rootEq_footnote_reference = rootEqWithChildren

    ###########################################################################
    # citation_reference

    # TODO This is typically a minor change and should be requested by
    # a special option
   
    rootEq_citation_reference = rootEqWithChildren

    ###########################################################################
    # For some elements their attributes need to be considered to
    # detect changes *and* they may contain only #PCDATA.

    ###########################################################################
    # option_argument

    # TODO This is typically a minor change and should be requested by
    # a special option
   
    def attributeEq_option_argument(self, node, other):
        return self.attributeEq(node, other, 'delimiter')

    def rootEq_option_argument(self, node, other):
        return (self.attributeEq_option_argument(node, other)
                and self.rootEqWithChildren(node, other))

    def childEq_option_argument(self, node, other):
        return (self.attributeEq_option_argument(node, other)
                and self.childrenEq(node, other))

    ###########################################################################
    # A change in certain elements must propagate the change up since
    # they may occur only once. Must be done by parents.

    # Checks whether `node` and `other` have both a node of type
    # `childClass` and whether the first of thosee are equal.
    def rootEqWithChild(self, node, other, childClass):
        if node.__class__ != other.__class__:
            return False

        nodeFound = None
        for nodeChild in self.getChildren(node):
            if isinstance(nodeChild, childClass):
                nodeFound = nodeChild
                break

        otherFound = None
        for otherChild in self.getChildren(other):
            if isinstance(otherChild, childClass):
                otherFound = otherChild
                break

        if nodeFound is None or otherFound is None:
            return True

        return self.childEq(nodeFound, otherFound)

    ###########################################################################
    # footnote

    def rootEq_footnote(self, node, other):
        return self.rootEqWithChild(node, other, nodes.label)

    ###########################################################################
    # citation

    def rootEq_citation(self, node, other):
        return self.rootEqWithChild(node, other, nodes.label)

    ###########################################################################
    # option

    def rootEq_option(self, node, other):
        return self.rootEqWithChild(node, other, nodes.option_string)

###############################################################################
###############################################################################
# Main

def processCommandLine():
    """Process command line and return a `Publisher`."""
    # Determine writer here so options can be given normally
    preWriter = writerDefault
    for arg in sys.argv:
        match = re.search(writerArgRE1, arg)
        if match:
            preWriter = match.group(1)

    pub = Publisher3Args()
    pub.set_reader('standalone', None, 'restructuredtext')
    pub.set_writer(preWriter)

    settingsSpec = SettingsSpec()
    settingsSpec.settings_spec = settings_spec
    settingsSpec.settings_defaults = settings_defaults
    pub.process_command_line(usage=usage, description=description,
                             settings_spec=settingsSpec,
                             config_section=config_section)
    if pub.settings.writer != preWriter:
        new_reporter('<cmdline>',
                     pub.settings).severe("Internal error: Mismatch of pre-parsed (%r) and real (%r) writer"
                                          % ( preWriter, pub.settings.writer, ))
    pub.set_destination()
    return pub

def readTree(pub, sourceName):
    """Read and return a tree from `sourceName`."""
    # Reset reader - just in case it keeps state from a previous invocation
    pub.set_reader('standalone', None, 'restructuredtext')
    pub.set_source(None, sourceName)
    pub.document = None
    pub.document = pub.reader.read(pub.source, pub.parser, pub.settings)
    pub.apply_transforms()
    return pub.document

def doDiff(hashableNodeImpl, oldTree, newTree):
    """Create a difference from `oldTree` to `newTree` using
    `hashableNodeImpl`. Returns the opcodes necessary to transform
    `oldTree` to `newTree`."""
    matcher = TreeMatcher(hashableNodeImpl, oldTree, newTree,
                          lambda node: isinstance(node, White))
    return matcher.get_opcodes()

def buildDocument(oldTree, newTree, opcodes, settings):
    """Returns a new document for the result of converting `oldTree` to
    `newTree` using `opcodes` as description of changes."""
    if (not isinstance(oldTree, docutils.nodes.document)
        or not isinstance(newTree, docutils.nodes.document)):
        raise TypeError("Roots of trees must be documents")
    return new_document(u"%s => %s"
                        % ( settings._old_source, settings._new_source, ),
                        settings)

def buildTree(dispatcher, diffRoot, opcodes, oldRoot, newRoot):
    """Adds a new sub-tree under `diffRoot` converting children of
    `oldRoot` to `newRoot` using `opcodes`."""
    oldChildren = dispatcher.getChildren(oldRoot)
    newChildren = dispatcher.getChildren(newRoot)
    for opcode in opcodes:
        ( command, oldRange, newRange,
          subOpcodes, ) = Opcode(opcode).resolveOpcode(oldChildren, newChildren)
        if command == Opcode.Descend:
            child = dispatcher.copyRoot(oldRange[0])
            dispatcher.addChild(diffRoot, child)
            buildTree(dispatcher, child,
                      subOpcodes, oldRange[0], newRange[0])
        else:
            dispatcher.mergeChildren(diffRoot, oldRoot, newRoot,
                                     command, oldRange, newRange)

# A replacement in certain elements must not be propagated up since
# they may occur only once and replacement would double them
replaceNotUp = ( nodes.title, nodes.subtitle, nodes.term, nodes.field_name,
                 nodes.attribution, nodes.caption, # (%text.model)
                 nodes.header, nodes.footer, nodes.definition,
                 nodes.field_body, nodes.description, nodes.legend,
                 nodes.entry, # (%body.elements;+) or (%body.elements;*)
                 nodes.decoration, nodes.docinfo, nodes.transition,
                 nodes.option_group, nodes.thead,
                 nodes.tbody, # different content model
                 )

# A replacement in certain elements normally not subject to up
# propagation and contained in certain elements may propagate up if
# all their siblings are also replacements and would propagate up
replaceUpSiblings = (
    ( nodes.title, nodes.section, ),
    ( nodes.subtitle, nodes.section, ),
    ( nodes.term, nodes.definition_list_item, ),
    ( nodes.field_name, nodes.field, ),
    ( nodes.attribution, nodes.block_quote, ),
    ( nodes.caption, nodes.figure, ),
    ( nodes.definition, nodes.definition_list_item, ),
    ( nodes.field_body, nodes.field, ),
    ( nodes.description, nodes.option_list_item, ),
    ( nodes.legend, nodes.figure, ),
    ( nodes.option_group, nodes.option_list_item, ),
    )

# TODO If much text is replaced in a text element the whole element
# should be replaced. This makes more sense to people than two large
# replaced/replacement blocks where the only equality is in words like
# "the". The exact meaning of "much" should be an option.
def cleanOpcodes(opcodes, dispatcher, oldList, newList):
    """Replace some nasty results in `opcodes` by cleaner versions. Opcodes
    create `newList` from `oldList`."""
    mightReplaceUpSiblings = [ ]
    for i in range(len(opcodes)):
        opcode = Opcode(opcodes[i])
        ( command, oldRange, newRange, subOpcodes,
          ) = opcode.resolveOpcode(oldList, newList)
        if not subOpcodes:
            # Nothing to clean for flat or empty opcodes
            continue

        oldNode = oldRange[0]
        newNode = newRange[0]
        cleanOpcodes(subOpcodes, dispatcher, dispatcher.getChildren(oldNode),
                     dispatcher.getChildren(newNode))
        j = 1
        while j < len(subOpcodes):
            prev = Opcode(subOpcodes[j - 1])
            this = Opcode(subOpcodes[j])
            if (this.getCommand() != Opcode.Descend
                and prev.getCommand() == this.getCommand()):
                # Merge adjacing opcodes of same type
                prevOld = prev.getOldRange()
                prevNew = prev.getNewRange()
                thisOld = this.getOldRange()
                thisNew = this.getNewRange()
                prev.setOldRange(( prevOld[0], thisOld[1], ))
                prev.setNewRange(( prevNew[0], thisNew[1], ))
                subOpcodes[j - 1:j + 1] = [ prev.asTuple(), ]
            else:
                j += 1
        opcode.setSubOpcodes(subOpcodes)
        if len(subOpcodes) == 1:
            subOpcode = Opcode(subOpcodes[0])
            if subOpcode.getCommand() == Opcode.Descend:
                propagateUp = False
            elif subOpcode.getCommand() == Opcode.Replace:
                if any([ isinstance(oldNode, cls)
                         for cls in replaceNotUp ]):
                    propagateUp = False
                    if any([ isinstance(oldNode, cls)
                             and isinstance(oldNode.parent, parentCls)
                             for ( cls, parentCls, ) in replaceUpSiblings ]):
                        # If for instance a section/title would
                        # propagate a replacement up the propagation
                        # needs to be done if all siblings would
                        # also propagate a replacement up
                        mightReplaceUpSiblings.append(i)
                else:
                    propagateUp = True
            else:
                propagateUp = True
            if propagateUp:
                # Propagate 1-element sequences up
                opcode.setCommand(subOpcode.getCommand())
        opcodes[i] = opcode.asTuple()

    if mightReplaceUpSiblings:
        # There are entries which might propagate a replace up if all
        # siblings could do as well
        if all([ i in mightReplaceUpSiblings
                 or Opcode(opcodes[i]).getCommand() == Opcode.Replace
                 for i in range(len(opcodes)) ]):
            # All entries are replacements which may propagate up -
            # actually propagate elements which may propagate
            for i in mightReplaceUpSiblings:
                opcode = Opcode(opcodes[i])
                opcode.setCommand(Opcode.Replace)
                opcodes[i] = opcode.asTuple()

def createDiff(oldTree, newTree, debug=False):
    """Create and return a diff document from `oldTree` to `newTree`."""
    dispatcher = DocutilsDispatcher()
    dispatcher.debug = debug
    opcodes = doDiff(dispatcher, oldTree, newTree)
    if debug:
        from pprint import pprint
        print(oldTree.asdom().toprettyxml())
        print(newTree.asdom().toprettyxml())
        pprint(opcodes, sys.stdout, 2, 40, None)
        print("^^^ Before cleaning vvv After cleaning")
    cleanOpcodes(opcodes, dispatcher, [ oldTree ], [ newTree ])
    if debug:
        from pprint import pprint
        pprint(opcodes, sys.stdout, 2, 40, None)
    if len(opcodes) != 1:
        raise TypeError("Don't know how to merge documents which are not rootEq")
    opcode = Opcode(opcodes[0])
    if opcode.getCommand() not in ( Opcode.Descend, Opcode.Equal, ):
        # TODO There should be a sense making message for this case
        # because this may happen due to up propagation of replacements
        raise TypeError("Don't know how to merge top level opcode of type %r"
                        % ( opcode.getCommand(), ))

    diffDoc = buildDocument(oldTree, newTree, opcodes, pub.settings)
    if opcode.getCommand() == Opcode.Equal:
        # TODO Equality should be reported somehow
        diffDoc.extend([ child.deepcopy()
                         for child in newTree.children ])
    else:
        buildTree(dispatcher, diffDoc, opcode.getSubOpcodes(), oldTree, newTree)
    return diffDoc

if __name__ == '__main__':
    pub = processCommandLine()

    oldTree = readTree(pub, pub.settings._old_source)
    newTree = readTree(pub, pub.settings._new_source)

    Text2Words(oldTree).apply()
    Text2Words(newTree).apply()

    diffDoc = createDiff(oldTree, newTree)
    Words2Text(diffDoc).apply()

    pub.writer.write(diffDoc, pub.destination)
    pub.writer.assemble_parts()

# TODO The CSS classes need to be set in a CSS stylesheet
