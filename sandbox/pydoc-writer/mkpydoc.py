#!/usr/bin/python

# Convert the ctypes docs to LaTeX for use in Python docs

# This script is a hacked version taken from the Optik SVN repository.

import sys, os
import re
from popen2 import popen2
from glob import glob
import rfc822
from distutils.dep_util import newer_group, newer
from docutils.core import Publisher
from docutils.readers.standalone import Reader as StandaloneReader
from docutils.transforms import Transform
from docutils.writers.latex2e import Writer as LaTeXWriter, LaTeXTranslator
from docutils import nodes

class OptikReader(StandaloneReader):
    #default_transforms = (StandaloneReader.default_transforms +
    #                      (ReplacementTransform,))
    pass

# python 2.3
if not hasattr(__builtins__,"set"):
    import sets
    set = sets.Set
if not hasattr(__builtins__,"sorted"):
    def sorted(list):
        if hasattr(list,"sort"):
            return list.sort()
        # maybe it is sorted
        return list

from markup import codemarkup
missing = set()

class PyLaTeXWriter(LaTeXWriter):
    def __init__(self):
        LaTeXWriter.__init__(self)
        self.translator_class = PyLaTeXTranslator

class PyLaTeXTranslator(LaTeXTranslator):
    remap_title = {
        }

    # XXX need to factor this out
    module_name = "ctypes"
    module_summary = "A foreign function library for Python."
    module_type = "standard"
    module_author = "Thomas Heller"
    module_author_email = "theller@python.net"
    module_synopsis = ("A foreign function library for Python.")
    version_added = "2.5"

    refuri_override = {
        "reference" : "reference-guide",
        "callbacks" : "option-callbacks",
        }

    def __init__(self, document):
        LaTeXTranslator.__init__(self, document)
        self.head_prefix = []
        self.head = []
        self.body_prefix = []
        self.in_title = False

        # Disable a bunch of methods from the base class.
        empty_method = lambda self: None
        for nodetype in ('field_argument',
                         'field_body',
                         'field_list',
                         'field_name'):
            setattr(self, 'visit_' + nodetype, empty_method)
            setattr(self, 'depart_' + nodetype, empty_method)

        self.head_prefix = [
            "\\section{\\module{%(module_name)s} --- %(module_summary)s}\n"
            "\\declaremodule{%(module_type)s}{%(module_name)s}\n"
            "\\moduleauthor{%(module_author)s}{%(module_author_email)s}\n"
            "\\modulesynopsis{%(module_synopsis)s}\n"
            "\\versionadded{%(version_added)s}\n"
            % vars(self.__class__)
            ]
        # TODO definitions get from latexwriter
        # TODO definitions must be guarded if multiple modules are included
        # e.g. "\\ifx\\locallinewidth\\undefined\\newlength{\\locallinewidth}\\fi\n"
        self.definitions = [
                "\\ifx\\locallinewidth\\undefined\\newlength{\\locallinewidth}\\fi\n"
                "\\setlength{\\locallinewidth}{\\linewidth}\n"
            ]
    def astext(self):
        return ''.join(self.definitions +
                       self.head_prefix +
                       self.head +
                       self.body_prefix +
                       self.body +
                       self.body_suffix)

    def generate_section_label(self, title):
        title = title.lower()
        title = re.sub(r'\([^\)]*\)', '', title)
        title = re.sub(r'[^\w\s\-]', '', title)
        title = re.sub(r'\b(the|an?|and|your|are)\b', '', title)
        title = re.sub(r'(example \d+).*', r'\1', title)
##        title = title.replace("optik", "optparse")
        return "ctypes-" + "-".join(title.split())

    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass

    def visit_docinfo(self, node):
        #print "visit_docinfo: %r" % node
        self.docinfo = []

    def depart_docinfo(self, node):
        #print "depart_docinfo: %r" % node
        self.body = self.docinfo + self.body
        self.docinfo = None

    def visit_docinfo_item(self, node, name):
        #print "visit_docinfo_item: node=%r, name=%r" % (node, name)
        if name == "author":
            (name, email) = rfc822.parseaddr(node.astext())
            self.docinfo.append("\\sectionauthor{%s}{%s}\n" % (name, email))
            raise nodes.SkipNode

    def depart_docinfo_item(self, node):
        pass

    #def visit_field(self, node):
    #    (name, value) = (node[0].astext(), node[1].astext())
    #    print "visit_field: node=%r (name=%r, value=%r)" % (node, name, value)
    #    if self.docinfo is not None:
    #        if name == "VersionAdded":
    #            self.docinfo.append("\\versionadded{%s}\n" % value)
    #            raise nodes.SkipNode

    _quoted_string_re = re.compile(r'\"[^\"]*\"')
    _short_opt_string_re = re.compile(r'-[a-zA-Z]')
    _long_opt_string_re = re.compile(r'--[a-zA-Z-]+')
    _identifier_re = re.compile(r'[a-zA-Z_][a-zA-Z_0-9]*'
                                r'(\.[a-zA-Z_][a-zA-Z_0-9]*)*'
                                r'(\(\))?$')

    def visit_literal(self, node):
        assert isinstance(node[0], nodes.Text)
        text = node[0].data
####        text = re.sub(r'optik(\.[a-z]+)?\.', 'optparse.', text)
        if self.in_title:
            cmd = None
        elif self._quoted_string_re.match(text):
            cmd = 'code'
        elif self._short_opt_string_re.match(text):
            cmd = 'programopt'
        elif self._long_opt_string_re.match(text):
            cmd = 'longprogramopt'
            text = text[2:]
        elif self._identifier_re.match(text):
            cmd = codemarkup.get(text)
            if cmd is None:
##                print "warning: unrecognized code word %r" % text
                missing.add(text)
                cmd = 'code'
        else:
            cmd = 'code'

        self.literal = 1
        node[0].data = text
        if cmd is not None:
            self.body.append('\\%s{' % cmd)

    def depart_literal(self, node):
        if not self.in_title:
            self.body.append('}')
        self.literal = 0

    def visit_literal_block(self, node):
        self.body.append("\\begin{verbatim}\n")
        self.verbatim = 1

    def depart_literal_block(self, node):
        self.verbatim = 0
        self.body.append("\n\\end{verbatim}\n")

    def visit_title(self, node):
        title = node.astext()
        title = self.remap_title.get(title, title)
        label = self.generate_section_label(title)
        #print "%s -> %s" % (title, label)
        section_name = self.d_class.section(self.section_level + 1)
        self.body.append("\n\n\\%s{" % section_name)
        self.context.append("\\label{%s}}\n" % label)
        self.in_title = True

    def depart_title(self, node):
        self.in_title = False
        self.body.append(self.context.pop())
        
    def visit_target(self, node):
        pass

    def depart_target(self, node):
        pass

    def bookmark(self, node):
        pass

    def visit_definition(self, node):
        pass

    def depart_definition(self, node):
        pass
    
    def visit_definition_list_item(self, node):
        pass

    def depart_definition_list_item(self, node):
        pass

    def visit_reference(self, node):
        if node.has_key('refuri'):
            refuri = node['refuri']
            basename = os.path.splitext(refuri)[0]
            label = "optparse-" + self.refuri_override.get(basename, basename)
            print "got refuri=%r, label=%r" % (refuri, label)
        elif node.has_key('refid'):
            label = self.generate_section_label(node['refid'])
            print "got refid=%r, label=%r" % (node['refid'], label)
        else:
            print "warning: unhandled reference: node=%r" % node
            LaTeXTranslator.visit_reference(self, node)          

        self.body.append("section~\\ref{%s}, " % label)
        raise nodes.SkipDeparture

    _quoted_phrase_re = re.compile(r'"([^"]+)"')
    _em_dash_re = re.compile(r'\s+\-\-\s+')

    def visit_Text(self, node):
        text = node.astext()
        if self.in_title:
            text = self.remap_title.get(text, text)

        if not (self.literal or self.verbatim):
            text = self._em_dash_re.sub(u"\u2014", text)
            text = self._quoted_phrase_re.sub(u"\u201C\\1\u201D", text)
            text = re.sub(r'\bdocument\b', "section", text)
####        text = re.sub(r'optik(\.[a-z]+)?', 'optparse', text)
        text = self.encode(text)

        # A couple of transformations are easiest if they go direct
        # to LaTeX, so do them *after* encode().
##        text = text.replace("Optik", "\\module{optparse}")
        text = text.replace("UNIX", "\\UNIX{}")

        self.body.append(text)

    def depart_Text(self, node):
        pass



def concatenate_sources(sources, target):
    print "concatenating source files to %s" % target
    outdir = os.path.dirname(target)
    if not os.path.isdir(outdir):
        os.makedirs(outdir)
    outfile = open(target, "wt")
    for filename in sources:
        file = open(filename, "rt")
        for line in file:
            outfile.write(line)
        outfile.write("\n\n")
        file.close()
    outfile.close()
    
def convert(infilename, outfilename):

    print "converting %s to %s" % (infilename, outfilename)
    pub = Publisher()
    pub.set_components('standalone',        # reader
                       'restructuredtext',  # parser
                       'latex')             # writer (arg, will be discarded)
    pub.reader = OptikReader()
    pub.writer = PyLaTeXWriter()
    pub.process_programmatic_settings(None, None, None)
    pub.set_source(source_path=infilename)
    pub.set_destination(destination_path=outfilename)
    pub.publish()

def main():
    convert(sys.argv[1], sys.argv[2])
    if missing:
        mod = open("missing.py", "w")
        mod.write("# possible markups:\n")
        mod.write("# module, code, method, class, function, member, var.  Are there more?\n")
        mod.write("codemarkup = {\n")
        keys = sorted(missing)
        for name in keys:
            mod.write("    '%s': 'code',\n" % name)
        mod.write("}\n")
        mod.close()

main()
