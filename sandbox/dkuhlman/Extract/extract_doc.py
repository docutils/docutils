#!/usr/bin/env python

import sys
import string
import time
import getopt
import textwrap
import traceback
import inspect
import __builtin__

from pydoc import *


TARGET_rest = 1
TARGET_latex = 2
UseOverTitleAdornment = 0
TitleAdornmentChars_noover = '=-.,^+~:;_"\''
TitleAdornmentChars_over = '==--..,,^^++~~'
TitleAdornmentChars = TitleAdornmentChars_noover


## class Doc:
##     def document(self, thing, name=None, *args):
##         """Generate documentation for an object."""
##         args = (thing, name) + args
##         # 'try' clause is to attempt to handle the possibility that inspect
##         # identifies something in a way that pydoc itself has issues handling;
##         # think 'super' and how it is a descriptor (which raises the exception
##         # by lacking a __name__ attribute) and an instance.
## ##         try:
##         if inspect.ismodule(thing): 
##             result = self.docmodule(*args)
##             return result
##         if inspect.isclass(thing):
##             result = self.docclass(*args)
##             return result
##         if inspect.isroutine(thing):
##             result = self.docroutine(*args)
##             return result
## ##         except AttributeError:
## ##             traceback.print_last()
## ##             pass
##         result = self.docother(*args)
##         return result
## 
##     def fail(self, thing, name=None, *args):
##         """Raise an exception for unimplemented types."""
##         message = "don't know how to document thing%s of type %s" % (
##             name and ' ' + repr(name), type(thing).__name__)
##         raise TypeError, message
## 
##     docmodule = docclass = docroutine = docother = fail


class ReSTDoc(Doc):
    """Formatter class for reStructuredText documentation."""
    
    def __init__(self):
        self.lines = []
        self.titleLevel = 0

    _repr_instance = TextRepr()
    repr = _repr_instance.repr

    def push(self, text=''):
        """Push one line of output onto the collected output."""
        self.lines.append(text)

    def get_lines(self):
        """Get the accumulated lines of output."""
        return self.lines

    def emphasize(self, text):
        """Add emphasis to a piece of text."""
        text1 = '*%s*' % text
        return text1

    def formatvalue(self, thing):
        """Format an argument default value as text."""
        return '=' + self.repr(thing)

    def escape(self, text):
        """Escape special reStructuredText characters."""
        text1 = text.replace('*', '\\*')
        return text1

    def genTitle(self, text):
        """Generate a title according to the current title level."""
        adornment = TitleAdornmentChars[self.titleLevel]
        # If it's an even numbered title level, then generate
        # adornment above the title.
        if UseOverTitleAdornment:
            if ((self.titleLevel / 2) * 2) == self.titleLevel:
                self.push(adornment * len(text))
        self.push(text)
        self.push(adornment * len(text))
        self.push()
        
    def incTitleLevel(self):
        self.titleLevel += 1

    def decTitleLevel(self):
        self.titleLevel -= 1

    def docmodule(self, thing, name=None, mod=None):
        """Produce text documentation for a given module object."""
        # Ignore the passed-in name.
        name = thing.__name__
        synop, desc = splitdoc(getdoc(thing))
        self.genTitle('SYNOPSIS')
        if synop:
            line = '%s -- %s' % (name, synop)
            line = textwrap.fill(line, 68)
        else:
            line = name
        self.push(line)
        self.push()
        self.genTitle('DESCRIPTION')
        if desc:
            self.push(desc)
        else:
            self.push('No description for this module')
        self.push()
        classes = []
        for key, value in inspect.getmembers(thing, inspect.isclass):
            if (inspect.getmodule(value) or thing) is thing:
                if visiblename(key):
                    classes.append((key, value))
        funcs = []
        for key, value in inspect.getmembers(thing, inspect.isroutine):
            if inspect.isbuiltin(value) or inspect.getmodule(value) is thing:
                if visiblename(key):
                    funcs.append((key, value))
        data = []
        for key, value in inspect.getmembers(thing, isdata):
            if visiblename(key):
                data.append((key, value))
        self.genTitle('CLASSES')
        self.incTitleLevel()
        if classes:
            for key, value in classes:
                self.document(value, key, name)
        else:
            self.push('No classes in this module')
            self.push()
        self.decTitleLevel()
        self.genTitle('FUNCTIONS')
        self.incTitleLevel()
        if funcs:
            for key, value in funcs:
                self.document(value, key, name)
        else:
            self.push('No global functions in this module')
            self.push()
        self.decTitleLevel()
        self.genTitle('DATA')
        self.incTitleLevel()
        if data:
            for key, value in data:
                self.document(value, key, name)
        else:
            self.push('No global data in this module')
            self.push()
        self.decTitleLevel()
        if hasattr(thing, '__version__'):
            version = str(thing.__version__)
            if version[:11] == '$' + 'Revision: ' and version[-1:] == '$':
                version = strip(version[11:-1])
            self.genTitle('VERSION')
            self.push(version)
            self.push()
        if hasattr(thing, '__date__'):
            self.genTitle('DATE')
            self.push(thing.__date__)
            self.push()
        if hasattr(thing, '__author__'):
            self.genTitle('AUTHOR')
            self.push(thing.__author__)
            self.push()
        if hasattr(thing, '__credits__'):
            self.genTitle('CREDITS')
            self.push(thing.__credits__)
            self.push()

    def genlevel(self, level):
        pad = '  ' * level
        return pad
    
    def genbases(self, obj):
        level = 0
        self.genbases_aux(obj, level)
    
    def genbases_aux(self, obj, level):
        bases = obj.__bases__
        for base in bases:
            pad = self.genlevel(level)
            self.push('%s- %s' % (pad, base.__name__))
            self.push()
            self.genbases_aux(base, level + 1)

    def docclass(self, thing, name=None, mod=None):
        """Produce text documentation for a given class object."""
        realname = thing.__name__
        name = name or realname
        self.genTitle('class %s' % name)
        self.incTitleLevel()
        docstr = getdoc(thing)
        self.genTitle('Description')
        if docstr:
            self.push(docstr)
        else:
            self.push('No documentation for this class.')
        self.push()
        self.genTitle('Base Classes')
        if thing.__bases__:
            self.genbases(thing)
        else:
            self.push('No superclasses for this class.')
        self.push()
        attrs = filter(lambda (name, kind, cls, value): visiblename(name),
            inspect.classify_class_attrs(thing))
        self.genTitle('Methods')
        self.incTitleLevel()
        found = 0
        for attr in attrs:
            if attr[1] == 'method':
                self.document(attr[3], attr[0])
                found = 1
        if not found:
            self.push('No methods for this class')
        self.decTitleLevel()
        self.genTitle('Data')
        self.incTitleLevel()
        found = 0
        for attr in attrs:
            if attr[1] == 'data':
                self.document(attr[3], attr[0])
                found = 1
        if not found:
            self.push('No data for this class')
            self.push()
        self.decTitleLevel()
        self.decTitleLevel()

    def docroutine(self, thing, name=None, mod=None, cl=None):
        """Produce text documentation for a function or method object."""
        realname = thing.__name__
        name = name or realname
        note = ''
        skipdocs = 0
        if inspect.ismethod(thing):
            imclass = thing.im_class
            if cl:
                if imclass is not cl:
                    note = 'from ' + classname(imclass, mod)
            else:
                if thing.im_self:
                    note = 'method of %s instance' % classname(
                        thing.im_self.__class__, mod)
                else:
                    note = 'unbound %s method' % classname(imclass,mod)
            thing = thing.im_func
        if name == realname:
            title = self.emphasize(realname)
        else:
            if (cl and realname in cl.__dict__ and
                cl.__dict__[realname] is thing):
                skipdocs = 1
            title = '%s = %s' % (self.emphasize(name), realname)
        if inspect.isfunction(thing):
            args, varargs, varkw, defaults = inspect.getargspec(thing)
            argspec = inspect.formatargspec(
                args, varargs, varkw, defaults, formatvalue=self.formatvalue)
            if realname == '<lambda>':
                title = 'lambda'
                argspec = argspec[1:-1] # remove parentheses
        else:
            argspec = '(...)'
        argspec = self.escape(argspec)
        decl = '%s %s %s' % (title, argspec, note)
        if inspect.isfunction(thing):
            line = 'function %s' % name
        elif inspect.ismethod(thing):
            line = 'method %s' % name
        else:
            line = 'thing %s' % name
        self.genTitle(line)
        self.incTitleLevel()
        self.genTitle('Prototype')
        self.push(decl)
        self.push()
        if not skipdocs:
            self.genTitle('Description')
            doc = getdoc(thing)
            if doc:
                doc = doc.rstrip()
                self.push(doc)
            else:
                self.push('No description for this function/method.')
        self.push()
        self.decTitleLevel()

    def docother(self, thing, name=None, mod=None, maxlen=None, doc=None):
        """Produce text documentation for a data object."""
        objRepr = self.repr(thing)
        if name:
            line = '- %s = %s' % (self.emphasize(name), objRepr)
        else:
            line = objRepr
        self.push(line)
        self.push()

# end class ReSTDoc


class PythonLaTeXDoc(Doc):
    pass


def extract_to_rest(thing,
        usePager=None,
        title='Python Library Documentation: %s',
        forceload=0
        ):
    """Display reSturcturedText documentation, given 
    an object or a path to an object.
    Push the resulting text through a pager, if requested,
    else print it to stdout.
    Add a minimal amount of header and trailer information to
    the document.
    """
    formatter = ReSTDoc()
    try:
        thing1, name = resolve(thing, forceload)
    except (ImportError, ErrorDuringImport), value:
        print value
        return
    desc = describe(thing1)
    module = inspect.getmodule(thing1)
    if name and '.' in name:
        desc += ' in ' + name[:name.rfind('.')]
    elif module and module is not thing1:
        desc += ' in module ' + module.__name__
    title1 = title % desc
    formatter.genTitle(title1)
    formatter.incTitleLevel()
    #
    # Add and replace header content here.
    #
    formatter.push('Generated by extract_doc.py on %s.' % time.ctime())
    formatter.push()
    formatter.document(thing1, name)
    #
    # Add and replace trailer content here.
    #
    doclines = formatter.get_lines()
    content = '\n'.join(doclines)
    #
    # Add and replace post-processing here.
    # You might consider sending the output through one of the
    #   reSTructuredText writers.
    #
    if usePager:
        pager(content)
    else:
        sys.stdout.write(content)
        sys.stdout.write('\n')


def extract_to_latex(module_name):
    print 'Not implemented yet'




USAGE_TEXT = """
Usage:
    python extract_doc.py [options] <module_name>
Options:
    -h, --help      Display this help message.
    -r, --rest      Extract to decorated reST.
    -l, --latex     Extract to Python LaTeX (module doc type). Not implemented.
    -p, --pager     Use a pager; else write to stdout.
    -o, --over      Use over *and* under title adornment, else only under.
Example:
    python extract_doc.py -r mymodule1
    python extract_doc.py -p -o -r mymodule2
"""

def usage():
    print USAGE_TEXT
    sys.exit(-1)


def main():
    global UseOverTitleAdornment, TitleAdornmentChars
    args = sys.argv[1:]
    try:
        opts, args = getopt.getopt(args, 'hrlpdo',
            ['help', 'rest', 'latex', 'pager', 'over'])
    except:
        usage()
    target = None
    usePager = None
    for opt, val in opts:
        if opt in ('-h', '--help'):
            usage()
        elif opt in ('-r', '--rest'):
            target = TARGET_rest
        elif opt in ('-l', '--latex'):
            target = TARGET_latex
        elif opt in ('-p', '--pager'):
            usePager = 1
        elif opt in ('-o', '--over'):
            UseOverTitleAdornment = 1
            TitleAdornmentChars = TitleAdornmentChars_over
    if len(args) != 1:
        usage()
    if target == TARGET_rest:
        extract_to_rest(args[0], usePager)
    elif target == TARGET_latex:
        extract_to_latex(args[0], usePager)
    else:
        usage()


if __name__ == '__main__':
    args = sys.argv[1:]
    if '-d' not in args:
        main()
    else:
        import pdb
        pdb.run('main()')


