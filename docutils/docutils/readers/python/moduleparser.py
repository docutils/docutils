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

"""

__docformat__ = 'reStructuredText'

import sys
import compiler
import compiler.ast
import compiler.visitor
from compiler.consts import OP_ASSIGN
from types import StringType, UnicodeType


def parse_module(module_text, filename):
    ast = compiler.parse(module_text)
    visitor = ModuleVisitor(filename)
    compiler.walk(ast, visitor, walker=visitor)
    return visitor.module


class ModuleVisitor(compiler.visitor.ASTVisitor):

    def __init__(self, filename):
        compiler.visitor.ASTVisitor.__init__(self)
        self.filename = filename
        self.module = None
        self.context = []
        self.documentable = None

    def default(self, node, *args):
        self.documentable = None
        #print 'in default (%s)' % node.__class__.__name__
        #compiler.visitor.ASTVisitor.default(self, node, *args)

    def default_ignore(self, node, *args):
        #print 'in default_ignore (%s)' % node.__class__.__name__
        compiler.visitor.ASTVisitor.default(self, node, *args)

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


class AssignmentVisitor(compiler.visitor.ASTVisitor):

    def __init__(self):
        compiler.visitor.ASTVisitor.__init__(self)
        self.attributes = []

    def default(self, node, *args):
        pass

    def visitAssign(self, node):
        compiler.visitor.ASTVisitor.default(self, node)

    def visitAssName(self, node):
        self.attributes.append(Attribute(node, node.name))

    def get_rhs(self, node):
        return "'TBD'"


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
