#!/usr/bin/python
#
# $Id$
# Author: Engelbert Gruber <grubert@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
rst2vers.py -- LaTeX verse writer
=================================

Usage :

  rst2vers.py input output

simple transformation from a reStructured text into a LaTeX 
verse poem.

reSt input ::

  All My Exes
  ---------------

  | your dogs, your daughters
  | or the state of your kitchen.
  | 
  | I will live in the secure orbit

output ::

  \poemtitle{All My Exes}

  \begin{verse}
  your dogs, your daughters \\
  or the state of your kitchen. \\
  
  I will live in the secure orbit \\

  \end{verse}
"""

import sys

from docutils.core import Publisher
from docutils.readers.standalone import Reader as StandaloneReader
from docutils.writers.latex2e import Writer as LaTeXWriter, LaTeXTranslator
from docutils import nodes

class Reader(StandaloneReader):
    pass

class VerseLaTeXWriter(LaTeXWriter):
    def __init__(self):
        LaTeXWriter.__init__(self)
        self.translator_class = VerseLaTeXTranslator

class VerseLaTeXTranslator(LaTeXTranslator):
    remap_title = {
        }
    roman = (None,None,"ii","iii","iv","v")

    refuri_override = {
        "reference" : "reference-guide",
        "callbacks" : "option-callbacks",
        }

    def __init__(self, document):
        LaTeXTranslator.__init__(self, document)
        self._verse_in = []     # add vin indentation.
        self._in_stanza = 0

    def astext(self):
        return ''.join(self.body)
    
    def visit_document(self, node):
        pass
    def depart_document(self, node):
        pass
    
    def visit_title(self, node):
        self.body.append('\\poemtitle{')
    def depart_title(self, node):
        self.body.append('}\n')
    
    def visit_line_block(self, node):
        if isinstance(node.parent, nodes.line_block):
            # BUG only one indentation supported 
            self._verse_in.append('\\vin ')
        else:
            self.body.append('\\begin{verse}\n')
    def depart_line_block(self, node):
        if len(self._verse_in)>0:
            self._verse_in.pop()
        else:
            self.body.append('\\end{verse}\n')

    def visit_line(self, node):
        if len(node.astext().strip())==0:
            if self._in_stanza != 0:
                # change last line end to ``\\!\n``
                self.body[-1] = self.body[-1].rstrip() + '!\n'
            self.body.append('\n')
            self._in_stanza = 0
            raise nodes.SkipNode
        if len(self._verse_in)>0:
            # BUG assume only one indentation is needed
            self.body.append(self._verse_in[0])
    def depart_line(self, node):
        self._in_stanza = 1
        self.body.append(' \\\\\n')


def convert(infilename, outfilename):
    print "converting %s to %s" % (infilename, outfilename)
    pub = Publisher()
    pub.set_components('standalone',        # reader
                       'restructuredtext',  # parser
                       'latex')             # writer (arg, will be discarded)
    pub.reader = StandaloneReader()
    pub.writer = VerseLaTeXWriter()
    pub.process_programmatic_settings(None, None, None)
    pub.set_source(source_path=infilename)
    pub.set_destination(destination_path=outfilename)
    pub.publish()

def main():
    convert(sys.argv[1], sys.argv[2])

main()
