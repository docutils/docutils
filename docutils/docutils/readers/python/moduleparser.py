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
  nodes.  Useful for assignment expressions (RHS).

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
            <Method name="__init__">
                <Parameters>
                    <Parameter name="self">
                    <Parameter name="text">
                        <Expression>
                            None
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

compiler.parse() provides most of what's needed for this AST, and "tokenize"
can be used to get the rest.  We can determine the line number from the
compiler.parse() AST, and the TokenParser.rhs(lineno) method provides the
rest.

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
    token_parser = TokenParser(module_text)
    visitor = ModuleVisitor(filename, token_parser)
    compiler.walk(ast, visitor, walker=visitor)
    return visitor.module


class BaseVisitor(ASTVisitor):

    def __init__(self, token_parser):
        ASTVisitor.__init__(self)
        self.token_parser = token_parser
        self.context = []
        self.documentable = None

    def default(self, node, *args):
        self.documentable = None
        #print 'in default (%s)' % node.__class__.__name__
        #ASTVisitor.default(self, node, *args)

    def default_visit(self, node, *args):
        #print 'in default_visit (%s)' % node.__class__.__name__
        ASTVisitor.default(self, node, *args)


class DocstringVisitor(BaseVisitor):

    def visitDiscard(self, node):
        if self.documentable:
            self.visit(node.expr)

    def visitConst(self, node):
        if self.documentable:
            if type(node.value) in (StringType, UnicodeType):
                self.documentable.append(Docstring(node, node.value))
            else:
                self.documentable = None

    def visitStmt(self, node):
        self.default_visit(node)


class AssignmentVisitor(DocstringVisitor):

    def visitAssign(self, node):
        visitor = AttributeVisitor(self.token_parser)
        compiler.walk(node, visitor, walker=visitor)
        if visitor.attributes:
            self.context[-1].extend(visitor.attributes)
        if len(visitor.attributes) == 1:
            self.documentable = visitor.attributes[0]
        else:
            self.documentable = None


class ModuleVisitor(AssignmentVisitor):

    def __init__(self, filename, token_parser):
        AssignmentVisitor.__init__(self, token_parser)
        self.filename = filename
        self.module = None

    def visitModule(self, node):
        self.module = module = Module(node, self.filename)
        if node.doc is not None:
            module.append(Docstring(node, node.doc))
        self.context.append(module)
        self.documentable = module
        self.visit(node.node)
        self.context.pop()

    def visitImport(self, node):
        self.context[-1].append(Import(node, node.names))
        self.documentable = None

    def visitFrom(self, node):
        self.context[-1].append(
            Import(node, node.names, from_name=node.modname))
        self.documentable = None

    def visitFunction(self, node):
        visitor = FunctionVisitor(self.token_parser)
        compiler.walk(node, visitor, walker=visitor)
        self.context[-1].append(visitor.function)


class AttributeVisitor(BaseVisitor):

    def __init__(self, token_parser):
        BaseVisitor.__init__(self, token_parser)
        self.attributes = []

    def visitAssign(self, node):
        # Don't visit the expression itself, just the attribute nodes:
        for child in node.nodes:
            self.dispatch(child)
        expression_text = self.token_parser.rhs(node.lineno)
        expression = Expression(node, expression_text)
        for attribute in self.attributes:
            attribute.append(expression)

    def visitAssName(self, node):
        self.attributes.append(Attribute(node, node.name))

    def visitAssTuple(self, node):
        attributes = self.attributes
        self.attributes = []
        self.default_visit(node)
        names = [attribute.name for attribute in self.attributes]
        att_tuple = AttributeTuple(node, names)
        att_tuple.lineno = self.attributes[0].lineno
        self.attributes = attributes
        self.attributes.append(att_tuple)

    def visitAssAttr(self, node):
        self.default_visit(node, node.attrname)

    def visitGetattr(self, node, suffix):
        self.default_visit(node, node.attrname + '.' + suffix)

    def visitName(self, node, suffix):
        self.attributes.append(Attribute(node, node.name + '.' + suffix))


class FunctionVisitor(DocstringVisitor):

    def visitFunction(self, node):
        self.function = function = Function(node, node.name)
        if node.doc is not None:
            function.append(Docstring(node, node.doc))
        self.context.append(function)
        self.documentable = function
        self.parse_parameter_list(node)
        self.visit(node.code)
        self.context.pop()

    def parse_parameter_list(self, node):
        parameters = []
        special = []
        argnames = list(node.argnames)
        if node.kwargs:
            special.append(ExcessKeywordArguments(node, argnames[-1]))
            argnames.pop()
        if node.varargs:
            special.append(ExcessPositionalArguments(node, argnames[-1]))
            argnames.pop()
        defaults = list(node.defaults)
        defaults = [None] * (len(argnames) - len(defaults)) + defaults
        for argname, default in zip(argnames, defaults):
            parameter = Parameter(node, argname)
            if default:
                default_text = self.token_parser.default(node.lineno)
                parameter.append(Default(node, default_text))
            parameters.append(parameter)
        if parameters or special:
            special.reverse()
            parameters.extend(special)
            parameter_list = ParameterList(node)
            parameter_list.extend(parameters)
            self.function.append(parameter_list)


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


class AttributeTuple(Node):

    def __init__(self, node, names):
        Node.__init__(self, node)
        self.names = names

    def attlist(self):
        return Node.attlist(self, names=' '.join(self.names))


class Expression(Node):

    def __init__(self, node, text):
        Node.__init__(self, node)
        self.text = text

    def __str__(self, indent='    ', level=0):
        prefix = indent * (level + 1)
        return '%s%s%s\n' % (Node.__str__(self, indent, level),
                             prefix, self.text.encode('unicode-escape'))


class Function(Attribute): pass


class ParameterList(Node): pass


class Parameter(Attribute): pass


class ExcessPositionalArguments(Parameter): pass


class ExcessKeywordArguments(Parameter): pass


class Default(Expression): pass


class TokenParser:

    def __init__(self, text):
        self.text = text + '\n\n'
        self.lines = self.text.splitlines(1)
        self.generator = tokenize.generate_tokens(iter(self.lines).next)
        self.next()

    def __iter__(self):
        return self

    def next(self):
        self.token = self.generator.next()
        self.type, self.string, self.start, self.end, self.line = self.token
        return self.token

    def goto_line(self, lineno):
        while self.start[0] < lineno:
            self.next()
        return token

    def rhs(self, lineno):
        """
        Return a whitespace-normalized expression string from the right-hand
        side of an assignment at line `lineno`.
        """
        self.goto_line(lineno)
        while self.string != '=':
            self.next()
        while self.type != token.NEWLINE and self.string != ';':
            append = 1
            append_ws = 1
            del_ws = 0
            if self.string == '=':
                start_row, start_col = self.end
                tokens = []
                last_type = None
                last_string = None
                backquote = 0
                append = 0
            elif self.string == '.':
                del_ws = 1
                append_ws = 0
            elif self.string in ('(', '[', '{'):
                append_ws = 0
                if self.string in '([' and (last_type == token.NAME or
                                            last_string in (')', ']', '}')):
                    del_ws = 1
            elif self.string in (')', ']', '}', ':', ','):
                    del_ws = 1
            elif self.string == '`':
                if backquote:
                    del_ws = 1
                else:
                    append_ws = 0
                backquote = not backquote
            elif self.type == tokenize.NL:
                append = 0
            if append:
                if del_ws and tokens and tokens[-1] == ' ':
                    del tokens[-1]
                tokens.append(self.string)
                last_type = self.type
                last_string = self.string
                if append_ws:
                    tokens.append(' ')
            self.next()
        self.next()
        text = ''.join(tokens)
        return text.strip()


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
