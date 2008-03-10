# Author: Guenter Milde
# Contact: 
# Revision: $Revision$
# Date: $Date: 2005-06-28$
# Copyright: Licensed under the Academic Free License version 1.2

"""
Work in progress. The aim is to create a
HTML document tree Writer producing strict HTML 4 relying on CSS 2.

This is a subclass of docutils' standard html4css1 writer.

The output conforms to the XHTML version 1.0 Strict DTD. 
It contains no formatting information that prevents layout design by 
cascading style sheets. 
"""

__docformat__ = 'reStructuredText'

import sys
import os
import os.path
import time
import re
from types import ListType
try:
    import Image                        # check for the Python Imaging Library
except ImportError:
    Image = None
import docutils
from docutils import frontend, nodes, utils, writers, languages

from docutils.writers import html4css1
# from html4css1 import SimpleListChecker

class Writer(html4css1.Writer):

    supported = ('html', 'xhtml', 
                 'html4strict', 'xhtml1strict',
                 'html4css2', 'xhtml1css2')
    """Formats this writer supports."""
    
    config_section = 'html4strict writer'
    
    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = HTMLTranslator



class HTMLTranslator(html4css1.HTMLTranslator):

    """
    This HTML writer has been changed to produce html without formatting 
    hints that interfere with a CSS stylesheet. 
    
    TODO: remove all direct formatting. 
          Make all output comply to strict html.
    """
    
    # no hard-coded border setting in the table head
    def visit_table(self, node):
        self.body.append(
            self.starttag(node, 'table', CLASS='docutils table'))

    # field-lists as definition list for styling with CSS
    # ---------------------------------------------------
    
    def visit_field_list(self, node):
        self.body.append(
            self.starttag(node, 'dl', CLASS='docutils field-list'))
    
    def depart_field_list(self, node):
        self.body.append('</dl>\n')
    
    def visit_field(self, node):
        pass
        
    def depart_field(self, node):
        pass
        
    def visit_field_name(self, node):
        self.body.append(self.starttag(node, 'dt', '', CLASS='field-name'))

    def depart_field_name(self, node):
        self.body.append('</dt>\n')

    def visit_field_body(self, node):
        self.body.append(self.starttag(node, 'dd', '', CLASS='field-body'))
        self.set_class_on_child(node, 'first', 0)
        field = node.parent
        if (self.compact_field_list or
            isinstance(field.parent, nodes.docinfo) or
            field.parent.index(field) == len(field.parent) - 1):
            # If we are in a compact list, the docinfo, or if this is
            # the last field of the field list, do not add vertical
            # space after last element.
            self.set_class_on_child(node, 'last', -1)


    def depart_field_body(self, node):
        self.body.append('</dd>\n')
        
    # docinfo list also a definition list        
    # -----------------------------------

    def visit_docinfo(self, node):
        self.body.append(
            self.starttag(node, 'dl', CLASS='docutils docinfo'))
        
    def depart_docinfo(self, node):
        self.body.append('</dl>\n')
        
    def visit_docinfo_item(self, node, name, meta=1):
        if meta:
            meta_tag = '<meta name="%s" content="%s" />\n' \
                       % (name, self.attval(node.astext()))
            self.add_meta(meta_tag)
        self.body.append('<dt class="field-name docinfo">%s</dt>\n'
                         % self.language.labels[name])
        self.body.append(self.starttag(node, 'dd', '', CLASS='field-body docinfo'))
        if len(node):
            if isinstance(node[0], nodes.Element):
                node[0]['classes'].append('first')
            if isinstance(node[-1], nodes.Element):
                node[-1]['classes'].append('last')

    def depart_docinfo_item(self):
        self.body.append('</dd>\n')


    

    
