#!/usr/bin/env python
"""
This module extracts the documentation from a module, and converts
it into a single reST document.

Usage:
    python extractor.py some_module.py > some_module.txt

The document is based on the module's docstring -- no other
documentation is implicitly included.

Other documentation can be explicitly included by using the directives
``.. include: function_or_class`` or ``.. include-all:``.

The first directive includes the docstring from that function or
class.  When the directive is encountered inside a class, it can refer
either to the global or local namespace, as in ``..include:
Document.add_child`` or ``.. include: add_child``.

The second directive will include all children of the module or class,
except those which start with a ``"_"`` (i.e., private), those that
have ``:ignore:`` anywhere in their docstring, or those that have
already been included.

You can also force a docstring to be ignored by using
``.. ignore: function_or_class``.  This is useful for properties,
whose documentation will not be extracted, or other times when
you want to document the function or class separately from its
docstring.

TODO
----

* Allow docstrings to override the normal function argument
  list (e.g., to hide non-public optional arguments).
* Some sort of table of contents support.

"""


import os, re, sys
from docutils.readers.python import moduleparser

class Document:

    def __init__(self, node):
        self.node = node
        self.documents = []
        self.children = {}
        self.children_order = []

    def add_child(self, child):
        self.children[child.name] = child
        self.children_order.append(child.name)

    def process(self):
        for child in self.node.children:
            self.process_node(child)

    def process_node(self, node):
        if isinstance(node, moduleparser.Docstring):
            self.documents.append(node.text)
        elif isinstance(node, moduleparser.Class):
            self.add_child(Class(node))
        elif isinstance(node, moduleparser.Function):
            self.add_child(Function(node))

    def documentation(self):
        return '\n'.join(self.documents)

    def childStr(self):
        children = []
        for child_name in self.children_order:
            children.append(self.children[child_name])
        return ''.join([c.documentation() for c in children])
            

class Module(Document):

    def __init__(self, filename, text=None):
        self.filename = filename
        if text is None:
            text = open(filename).read()
        self.module_text = text
        self.name = os.path.splitext(os.path.basename(filename))[0]
        node = moduleparser.parse_module(text, filename)
        Document.__init__(self, node)
        self.imports = []
        self.process()

    def process_node(self, node):
        if isinstance(node, moduleparser.Import):
            self.imports.append((node.names, node.from_name))
        else:
            Document.process_node(self, node)

    def documentation(self):
        doc = '\n\n'.join(self.documents)
        subber = InlineSubstitution(self)
        doc = subber.substitute(doc)
        return "%s\n%s\n\n%s" % \
               (self.name,
                "=" * len(self.name),
                doc)
            
    def importText(self, im):
        if im[1]:
            return 'from %s import %s' % (im[1], im[0])
        else:
            return 'import %s' % im[0]

    def _subString(self, s):
        return self._inlineRE.sub(self._inlineSubber, s)

    def _inlineSubber(self, match):
        level = len(match.group(1))
        name = match.group(2).strip().split('.')
        return indent(self._subString(self._getChild(name).documentation()), level)

    def _getChild(self, nameList):
        obj = self
        while 1:
            if not nameList:
                return obj
            obj = obj.children[nameList[0]]
            nameList = nameList[1:]

class InlineSubstitution:

    def __init__(self, rootNode):
        self.rootNode = rootNode
        self._seen = {}

    _inlineRE = re.compile(r'( *).. +inline: *(.*)')
    _inlineAllRE = re.compile(r'( *).. +inline-all: *')
    _ignoreRE = re.compile(r'( *).. ignore: *(.*)\n?')

    def substitute(self, s, currentNode=None):
        if currentNode is None:
            currentNode = self.rootNode
        s = self._ignoreRE.sub(
            lambda m, currentNode=currentNode: self._ignoreSubber(m, currentNode),
            s)
        s = self._inlineRE.sub(
            lambda m, currentNode=currentNode: self._inlineSubber(m, currentNode),
            s)
        s = self._inlineAllRE.sub(
            lambda m, currentNode=currentNode: self._inlineAllSubber(m, currentNode),
            s)
        return s

    def _inlineSubber(self, match, currentNode):
        level = len(match.group(1))
        name = match.group(2).strip().split('.')
        child = self._getChild(name, currentNode)
        self._seen[child] = 1
        return indent(self.substitute(child.documentation(), child), level)

    def _ignoreSubber(self, match, currentNode):
        name = match.group(2).strip().split('.')
        child = self._getChild(name, currentNode)
        self._seen[child] = 1
        return ''

    def _getChild(self, name, currentNode):
        nameList = name
        obj = currentNode
        while 1:
            if not nameList:
                return obj
            if not obj.children.has_key(nameList[0]):
                if currentNode is self.rootNode:
                    raise NameError, '%s not found' % '.'.join(name)
                else:
                    return self._getChild(name, self.rootNode)
            obj = obj.children[nameList[0]]
            nameList = nameList[1:]

    def _inlineAllSubber(self, match, currentNode):
        level = len(match.group(1))
        children = currentNode.children.keys()
        children.sort()
        children = [currentNode.children[name] for name in children]
        allDocs = []
        for child in children:
            if child.name.startswith('_'):
                continue
            if self._seen.has_key(child):
                continue
            doc = child.documentation()
            if doc.lower().find(':ignore:') != -1:
                continue
            allDocs.append(self.substitute(doc, child))
            self._seen[child] = 1
        return indent('\n'.join(allDocs), level)



class Function(Document):

    def __init__(self, node):
        Document.__init__(self, node)
        self.name = node.name
        self.parameters = []
        self.process()

    def process_node(self, node):
        if isinstance(node, moduleparser.ParameterList):
            for parameter in node.children:
                self.process_parameter(parameter)
        else:
            Document.process_node(self, node)

    def process_parameter(self, param):
        ## @@: handle defaults, *args, etc.
        if param.name == 'self':
            return
        if param.children:
            val = ('default', (param.name, param.children[0].text))
        elif isinstance(param, moduleparser.ExcessPositionalArguments):
            val = ('*', param.name)
        elif isinstance(param, moduleparser.ExcessKeywordArguments):
            val = ('**', param.name)
        else:
            val = ('normal', param.name)
        self.parameters.append(val)

    def documentation(self):
        d = "`%s(%s)`:\n" % (self.name,
                             ', '.join([self.parameterText(p)
                                        for p in self.parameters]))
        doc = Document.documentation(self)
        if not doc:
            doc = "Not documented."
        return d + indent(doc)

    def parameterText(self, param):
        t, name = param
        if t == 'normal':
            return name
        elif t == 'default':
            return '%s=%s' % (name[0], name[1])
        elif t == '*':
            return '*%s' % name
        elif t == '**':
            return '**%s' % name
        else:
            assert 0

class Class(Document):

    def __init__(self, node):
        Document.__init__(self, node)
        self.bases = []
        self.name = node.name
        for attr, value in node.attlist():
            if attr == 'bases':
                self.bases = value
        self.process()

    def documentation(self):
        if self.bases:
            base = 'class `%s(%s)`:' % (self.name, self.bases)
        else:
            base = 'class `%s`:' % self.name
        return base + "\n" + indent(Document.documentation(self))


def indent(text, amount=4):
    return '\n'.join([(' '*amount) + line for line in text.split('\n')])

def create_documentation(filename, output):
    if type(output) is type(""):
        output = open(output, 'w')
    mod = Module(filename)
    doc = mod.documentation()
    if doc.lower().find(':ignore:') == -1:
        output.write(mod.documentation())

########################################
## Command-line interface
########################################

def main(options, args):
    for arg in args:
        if os.path.isdir(arg) and options.recurse:
            main(options, [os.path.join(arg, f) for f in os.listdir(arg)])
            continue
        if options.recurse and not arg.endswith('.py'):
            continue
        filename = os.path.splitext(arg)[0] + ".txt"
        filename = os.path.join(options.output, filename)
        filename = os.path.normpath(filename)
        if not options.quiet:
            sys.stdout.write('%s -> %s ...' % (os.path.normpath(arg),
                                               filename))
            sys.stdout.flush()
        create_documentation(arg, filename)
        if not options.quiet:
            sys.stdout.write('done.\n')
            sys.stdout.flush()

if __name__ == '__main__':
    try:
        from optparse import OptionParser
    except ImportError:
        from optik import OptionParser
    parser = OptionParser()
    parser.add_option('-r', '--recurse',
                      action="store_true",
                      dest="recurse",
                      default=0,
                      help="recurse into subdirectories")
    parser.add_option('-o', '--output',
                      dest="output",
                      help="write documentation to FILE (or directory)",
                      metavar="FILE")
    parser.add_option('-q', '--quiet',
                      dest="quiet",
                      default=0,
                      action="store_true",
                      help="be quiet")
    (options, args) = parser.parse_args()
    if len(args) == 1 and options.output \
           and not os.path.isdir(options.output):
        if options.output == '-':
            options.output = sys.stdout
        create_documentation(args[0], options.output)
    else:
        if not options.output:
            options.output = '.'
        main(options, args)

            
        
