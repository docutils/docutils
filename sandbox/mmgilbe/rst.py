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
    # return node..astext()
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
    """ Parse RST via "docutils". """

    def __init__(self, raw, request, **kw):
        self.raw = raw
        self.request = request
        self.form = request.form
        self._ = request.getText

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
        
    def faux_paragraph(*args):
        return ''
        
    def faux_write(self, string):
        self.body.append(string)
        
    # def visit_Text(self, node):
        # from MoinMoin.parser.wiki import Parser
        # oldWrite = self.request.write
        # self.request.write = self.faux_write
        # wikiparser = Parser(node.astext(), self.request)
        # self.formatter.paragraph = self.faux_paragraph
        # wikiparser.format(self.formatter)
        # self.request.write = oldWrite
        # # self.body.append(self.formatter.text(node.astext()))
