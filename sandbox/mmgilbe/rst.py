# -*- coding: iso-8859-1 -*-
"""
    MoinMoin - ReStructured Text Parser

    @copyright: 2002 by J�rgen Hermann <jh@web.de>
    @license: GNU GPL, see COPYING for details.
"""

import sys
from docutils import core, nodes, utils


#############################################################################
### ReStructured Text Parser
#############################################################################


import sys
import os
import os.path
import time
import re
from types import ListType
import docutils
from docutils import frontend, nodes, utils, writers, languages
from docutils.core import publish_string
from docutils.writers import html4css1

def mastext(node):
    # Find Python function that does this for me. string.encode('ascii',
    # 'xmlcharrefreplace')
    s = node.replace(u'\xa0', '&#xa0;')
    return s.replace(u'\u2020', '&#x2020;')

class MoinWriter(html4css1.Writer):
    
    config_section = 'moin writer'
    config_section_dependencies = ('writers',)

    """Final translated form of `document`."""
    output = None
    
    def wiki_resolver(self, node):
        node['refuri'] = node['refname']
        del node['refname']
        return '1'
    
    wiki_resolver.priority = 001
    
    def __init__(self, formatter, request):
        html4css1.Writer.__init__(self)
        self.formatter = formatter
        self.request = request
        self.unknown_reference_resolvers = [self.wiki_resolver]
        
        
    def translate(self):
        visitor = MoinTranslator(self.document, self.formatter, self.request)
        self.document.walkabout(visitor)
        self.output = mastext(visitor.astext())
        

class Parser:
    def __init__(self, raw, request, **kw):
        self.raw = raw
        self.request = request
        self.form = request.form
        # self._ = request.getText
        
    def format(self, formatter):
        text = publish_string(source = self.raw, 
                              writer = MoinWriter(formatter, self.request),
                              enable_exit = None, 
                              settings_overrides = {'traceback': 1})
        self.request.write(mastext(text))
        

class MoinTranslator(html4css1.HTMLTranslator):

    def __init__(self, document, formatter, request):
        html4css1.HTMLTranslator.__init__(self, document)
        self.formatter = formatter
        self.request = request
        self.level = 0
        self.oldWrite = self.request.write
        self.request.write = self.faux_write
    
    def __del__(self):
        self.request.write = oldWrite
        
    def astext(self):
        self.request.write = self.oldWrite
        return html4css1.HTMLTranslator.astext(self)
    
    def faux_write(self, string):
        self.body.append(string)

    def request(self):
        pass
        
    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass
        
    def visit_section(self, node):
        self.level += 1

    def depart_section(self, node):
        self.level -= 1

    def visit_title(self, node):
        self.request.write(self.formatter.heading(self.level, '', on=1))

    def depart_title(self, node):
        self.request.write(self.formatter.heading(self.level, '', on=0))

    #
    # Text markup
    #

    def visit_emphasis(self, node):
        self.request.write(self.formatter.emphasis(1))

    def depart_emphasis(self, node):
        self.request.write(self.formatter.emphasis(0))

    def visit_strong(self, node):
        self.request.write(self.formatter.strong(1))

    def depart_strong(self, node):
        self.request.write(self.formatter.strong(0))

    def visit_literal(self, node):
        self.request.write(self.formatter.code(1))

    def depart_literal(self, node):
        self.request.write(self.formatter.code(0))


    #
    # Blocks
    #

    def visit_paragraph(self, node):
        #if self.topic_class != 'contents':
        self.request.write(self.formatter.paragraph(1))

    def depart_paragraph(self, node):
        self.request.write(self.formatter.paragraph(0))

    def visit_literal_block(self, node):
        self.request.write(self.formatter.preformatted(1))

    def depart_literal_block(self, node):
        self.request.write(self.formatter.preformatted(0))


    #
    # Simple Lists
    #

    def visit_bullet_list(self, node):
        self.request.write(self.formatter.bullet_list(1))

    def depart_bullet_list(self, node):
        self.request.write(self.formatter.bullet_list(0))

    def visit_enumerated_list(self, node):
        self.request.write(self.formatter.number_list(1, start=node.get('start', None)))

    def depart_enumerated_list(self, node):
        self.request.write(self.formatter.number_list(0))

    def visit_list_item(self, node):
        self.request.write(self.formatter.listitem(1))

    def depart_list_item(self, node):
        self.request.write(self.formatter.listitem(0))


    #
    # Definition List
    #

    def visit_definition_list(self, node):
        self.request.write(self.formatter.definition_list(1))

    def visit_definition_list_item(self, node):
        pass

    def visit_term(self, node):
        self.request.write('<dt>')

    def depart_term(self, node):
        self.request.write('</dt>')

    def visit_definition(self, node):
        self.request.write('<dd>')

    def depart_definition(self, node):
        self.request.write('</dd>')

    def depart_definition_list(self, node):
        self.request.write(self.formatter.definition_list(0))


    #
    # Block Quote
    #

    def visit_block_quote(self, node):
        self.request.write(self.formatter.definition_list(1))

    def depart_block_quote(self, node):
        self.request.write(self.formatter.definition_list(0))


    #
    # Admonitions
    #

    def visit_warning(self, node):
        self.request.write(self.formatter.highlight(1))

    def depart_warning(self, node):
        self.request.write(self.formatter.highlight(0))


    #
    # Misc
    #

    # def visit_system_message(self, node):
        # self.request.write(self.formatter.highlight(1))
        # self.request.write('[%s]' % node.astext())
        # self.request.write(self.formatter.highlight(0))


