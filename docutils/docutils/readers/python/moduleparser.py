# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Parser for Python modules.

Ideas:

* Tokenize the module in parallel to extract initial values, comments, etc.

* Merge the compiler & tokenize output such that the raw text hangs off of
  nodes?  Especially assignment expressions (RHS).

What I'd like to do is to take a module, read in the text, run it through the
module parser (using compiler.py and tokenize.py) and produce a high-level AST
full of nodes that are interesting from an auto-documentation standpoint.  For
example, given this module (x.py)::

    # comment

    '''Docstring'''

    '''Additional docstring'''

    __docformat__ = 'reStructuredText'

    a = 1
    '''Attribute docstring'''

    class C(Super):

        '''C's docstring'''

        class_attribute = 1
        '''class_attribute's docstring'''

        def __init__(self, text=None):
            '''__init__'s docstring'''

            self.instance_attribute = (text * 7
                                       + ' whaddyaknow')
            '''instance_attribute's docstring'''


    def f(x,                            # parameter x
          y=a*5,                        # parameter y
          *args):                       # parameter args
        '''f's docstring'''
        return [x + item for item in args]

    f.function_attribute = 1
    '''f.function_attribute's docstring'''

The module parser should produce a high-level AST, something like this::

    <Module filename="x.py">
        <Comment lineno=1>
            comment
        <Docstring lineno=3>
            Docstring
        <Docstring lineno=...>           (I'll leave out the lineno's)
            Additional docstring
        <Attribute name="__docformat__">
            <Expression>
                'reStructuredText'
        <Attribute name="a">
            <Expression>
                1
            <Docstring>
                Attribute docstring
        <Class name="C" inheritance="Super">
            <Docstring>
                C's docstring
            <Attribute name="class_attribute">
                <Expression>
                    1
                <Docstring>
                    class_attribute's docstring
            <Method name="__init__" argnames=['self', ('text', 'None')]>
                <Docstring>
                    __init__'s docstring
                <Attribute name="instance_attribute" instance=True>
                    <Expression>
                        (text * 7
                         + ' whaddyaknow')
                    <Docstring>
                        class_attribute's docstring
        <Function name="f">
            <Parameters>
                <Parameter name="x">
                    <Comment>
                        # parameter x
                <Parameter name="y">
                    <Expression>
                        a*5
                    <Comment>
                        # parameter y
                <Parameter name="args" varargs=True>
                    <Comment>
                        # parameter args
            <Docstring>
                f's docstring
            <Attribute name="function_attribute">
                <Expression>
                    1
                <Docstring>
                    f.function_attribute's docstring

compiler.parse() provides most of what's needed for this AST.  I think that
"tokenize" can be used to get the rest, and all that's left is to hunker down
and figure out how.  We can determine the line number from the
compiler.parse() AST, and a get_rhs(lineno) method would provide the rest.

The Docutils Python reader component will transform this AST into a
Python-specific doctree, and then a `stylist transform`_ would further
transform it into a generic doctree.  Namespaces will have to be compiled for
each of the scopes, but I'm not certain at what stage of processing.

It's very important to keep all docstring processing out of this, so that it's
a completely generic and not tool-specific.

> Why perform all of those transformations?  Why not go from the AST to a
> generic doctree?  Or, even from the AST to the final output?

I want the docutils.readers.python.moduleparser.parse_module() function to
produce a standard documentation-oriented AST that can be used by any tool.
We can develop it together without having to compromise on the rest of our
design (i.e., HappyDoc doesn't have to be made to work like Docutils, and
vice-versa).  It would be a higher-level version of what compiler.py provides.

The Python reader component transforms this generic AST into a Python-specific
doctree (it knows about modules, classes, functions, etc.), but this is
specific to Docutils and cannot be used by HappyDoc or others.  The stylist
transform does the final layout, converting Python-specific structures
("class" sections, etc.) into a generic doctree using primitives (tables,
sections, lists, etc.).  This generic doctree does *not* know about Python
structures any more.  The advantage is that this doctree can be handed off to
any of the output writers to create any output format we like.

The latter two transforms are separate because I want to be able to have
multiple independent layout styles (multiple runtime-selectable "stylist
transforms").  Each of the existing tools (HappyDoc, pydoc, epydoc, Crystal,
etc.) has its own fixed format.  I personally don't like the tables-based
format produced by these tools, and I'd like to be able to customize the
format easily.  That's the goal of stylist transforms, which are independent
from the Reader component itself.  One stylist transform could produce
HappyDoc-like output, another could produce output similar to module docs in
the Python library reference manual, and so on.

It's for exactly this reason:

>> It's very important to keep all docstring processing out of this, so that
>> it's a completely generic and not tool-specific.

... but it goes past docstring processing.  It's also important to keep style
decisions and tool-specific data transforms out of this module parser.

"""

__docformat__ = 'reStructuredText'

import sys
import compiler
import compiler.ast
import tokenize
import token
from compiler.consts import OP_ASSIGN
from compiler.visitor import ASTVisitor
from types import StringType, UnicodeType


def parse_module(module_text, filename):
    ast = compiler.parse(module_text)
    visitor = ModuleVisitor(filename)
    compiler.walk(ast, visitor, walker=visitor)
    return visitor.module


class ModuleVisitor(ASTVisitor):

    def __init__(self, filename):
        ASTVisitor.__init__(self)
        self.filename = filename
        self.module = None
        self.context = []
        self.documentable = None

    def default(self, node, *args):
        self.documentable = None
        #print 'in default (%s)' % node.__class__.__name__
        #ASTVisitor.default(self, node, *args)

    def default_ignore(self, node, *args):
        #print 'in default_ignore (%s)' % node.__class__.__name__
        ASTVisitor.default(self, node, *args)

    def visitModule(self, node):
        #print dir(node)
        self.module = module = Module(node, self.filename)
        if node.doc is not None:
            module.append(Docstring(node, node.doc))
        self.context.append(module)
        self.documentable = module
        self.visit(node.node)
        self.context.pop()

    def visitStmt(self, node):
        self.default_ignore(node)

    def visitDiscard(self, node):
        if self.documentable:
            self.visit(node.expr)

    def visitConst(self, node):
        if self.documentable:
            if type(node.value) in (StringType, UnicodeType):
                self.documentable.append(Docstring(node, node.value))
            else:
                self.documentable = None

    def visitImport(self, node):
        self.context[-1].append(Import(node, node.names))
        self.documentable = None

    def visitFrom(self, node):
        self.context[-1].append(
            Import(node, node.names, from_name=node.modname))
        self.documentable = None

    def visitAssign(self, node):
        visitor = AssignmentVisitor()
        compiler.walk(node, visitor, walker=visitor)
        if visitor.attributes:
            self.context[-1].extend(visitor.attributes)
        if len(visitor.attributes) == 1:
            self.documentable = visitor.attributes[0]
        else:
            self.documentable = None


class AssignmentVisitor(ASTVisitor):

    """
    Tried reconstructing expressions (the RHS of assignments) by
    visiting the compiler.parse() tree, but a lot of information is
    missing, like parenthesis-grouping of expressions.

    Gotta do it by parsing tokens.
    """

    def __init__(self):
        ASTVisitor.__init__(self)
        self.attributes = []
        self.parts = []

    def default(self, node, *args):
        print >>sys.stderr, '%s not visited!' % node.__class__.__name__
        ASTVisitor.default(self, node)

    def visitAssign(self, node):
        ASTVisitor.default(self, node)
        self.attributes[-1].append(Expression(node, ''.join(self.parts)))

    def visitAssName(self, node):
        self.attributes.append(Attribute(node, node.name))

    def visitAdd(self, node):
        ASTVisitor.default(self, node)
        self.parts[-2:] = ' + '.join(self.parts[-2:])

    def visitAnd(self, node):
        ASTVisitor.default(self, node)
        self.parts.insert(len(self.parts) - 1, ' and ')

    def visitBackquote(self, node):
        self.parts.append('`')
        ASTVisitor.default(self, node)
        self.parts.append('`')

    def visitBitand(self, node):
        ASTVisitor.default(self, node)
        self.parts.insert(len(self.parts) - 1, ' & ')

    def visitBitor(self, node):
        ASTVisitor.default(self, node)
        self.parts.insert(len(self.parts) - 1, ' | ')

    def visitBitxor(self, node):
        ASTVisitor.default(self, node)
        self.parts.insert(len(self.parts) - 1, ' ^ ')

    def visitConst(self, node):
        self.parts.append(repr(node.value))

    def visitConst(self, node):
        self.parts.append(repr(node.value))

    def visitInvert(self, node):
        self.parts.append('~ ')
        ASTVisitor.default(self, node)


class Node:

    def __init__(self, node):
        self.children = []
        """List of child nodes."""

        self.lineno = node.lineno
        """Line number of this node (or ``None``)."""

    def __str__(self, indent='    ', level=0):
        return ''.join(['%s%s\n' % (indent * level, repr(self))] +
                       [child.__str__(indent, level+1)
                        for child in self.children])

    def __repr__(self):
        parts = [self.__class__.__name__]
        for name, value in self.attlist():
            parts.append('%s="%s"' % (name, value))
        return '<%s>' % ' '.join(parts)

    def attlist(self, **atts):
        if self.lineno is not None:
            atts['lineno'] = self.lineno
        attlist = atts.items()
        attlist.sort()
        return attlist

    def append(self, node):
        self.children.append(node)

    def extend(self, node_list):
        self.children.extend(node_list)


class Module(Node):

    def __init__(self, node, filename):
        Node.__init__(self, node)
        self.filename = filename

    def attlist(self):
        return Node.attlist(self, filename=self.filename)


class Docstring(Node):

    def __init__(self, node, text):
        Node.__init__(self, node)
        self.text = trim_docstring(text)

    def __str__(self, indent='    ', level=0):
        prefix = indent * (level + 1)
        text = '\n'.join([prefix + line for line in self.text.splitlines()])
        return Node.__str__(self, indent, level) + text + '\n'


class Import(Node):

    def __init__(self, node, names, from_name=None):
        Node.__init__(self, node)
        self.names = names
        self.from_name = from_name

    def __str__(self, indent='    ', level=0):
        prefix = indent * (level + 1)
        lines = []
        for name, as in self.names:
            if as:
                lines.append('%s%s as %s' % (prefix, name, as))
            else:
                lines.append('%s%s' % (prefix, name))
        text = '\n'.join(lines)
        return Node.__str__(self, indent, level) + text + '\n'

    def attlist(self):
        if self.from_name:
            atts = {'from': self.from_name}
        else:
            atts = {}
        return Node.attlist(self, **atts)


class Attribute(Node):

    def __init__(self, node, name):
        Node.__init__(self, node)
        self.name = name

    def attlist(self):
        return Node.attlist(self, name=self.name)


class Expression(Node):

    def __init__(self, node, text):
        Node.__init__(self, node)
        self.text = text

    def __str__(self, indent='    ', level=0):
        prefix = indent * (level + 1)
        return '%s%s%s\n' % (Node.__str__(self, indent, level),
                             prefix, self.text)


class TokenReader:

    def __init__(self, text):
        self.text = text
        self.lines = text.splitlines(1)
        self.generator = tokenize.generate_tokens(iter(self.lines).next)

    def __iter__(self):
        return self

    def next(self):
        token = self.generator.next()
        self.type, self.string, self.start, self.end, self.line = token
        return token

    def goto_line(self, lineno):
        for token in self:
            if self.start[0] >= lineno:
                return token
        else:
            raise IndexError

    def rhs(self, name, lineno):
        self.goto_line(lineno)
        while self.start[0] == lineno:
            if self.type == token.OP and self.string == '=':
                break
            self.next()
        else:
            raise IndexError
        

def trim_docstring(text):
    """
    Trim indentation and blank lines from docstring text & return it.

    See PEP 257.
    """
    if not text:
        return ''
    # Convert tabs to spaces (following the normal Python rules)
    # and split into a list of lines:
    lines = text.expandtabs().splitlines()
    # Determine minimum indentation (first line doesn't count):
    indent = sys.maxint
    for line in lines[1:]:
        stripped = line.lstrip()
        if stripped:
            indent = min(indent, len(line) - len(stripped))
    # Remove indentation (first line is special):
    trimmed = [lines[0].strip()]
    if indent < sys.maxint:
        for line in lines[1:]:
            trimmed.append(line[indent:].rstrip())
    # Strip off trailing and leading blank lines:
    while trimmed and not trimmed[-1]:
        trimmed.pop()
    while trimmed and not trimmed[0]:
        trimmed.pop(0)
    # Return a single string:
    return '\n'.join(trimmed)
