# -*- coding: iso-8859-1 -*-
"""
    MoinMoin - ReStructured Text Parser

    @copyright: 2004 by Matthew Gilbert <gilbert@voxmea.net>
    @license: GNU GPL, see COPYING for details.
    
    REQUIRES docutils 0.3.3 or later
    .
"""

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
from docutils import core, frontend, nodes, utils, writers, languages
from docutils.core import publish_parts
from docutils.writers import html4css1
from docutils.nodes import fully_normalize_name


def html_escape_unicode(node):
    # Find Python function that does this for me. string.encode('ascii',
    # 'xmlcharrefreplace') only 2.3 and above.
    for i in node:
        if ord(i) > 127:
            node = node.replace(i, '&#%d;' % (ord(i)))
    return node

class MoinWriter(html4css1.Writer):
    
    config_section = 'moin writer'
    config_section_dependencies = ('writers',)

    """Final translated form of `document`."""
    output = None
    
    def wiki_resolver(self, node):
        if getattr(node, 'indirect_reference_name', None):
            node['refuri'] = node.indirect_reference_name
            return 1
        elif 'id' in node.attributes:
            # I'm pretty sure the first test should catch any targets or
            # references with the "id" attribute. Therefore, if we get to here
            # its probably an internal link that didn't work so we let it go
            # through as an error.
            return 0
        node['refuri'] = node['refname']
        del node['refname']
        self.nodes.append(node)
        return 1
    
    wiki_resolver.priority = 001
    
    def __init__(self, formatter, request):
        html4css1.Writer.__init__(self)
        self.formatter = formatter
        self.request = request
        # Add our wiki unknown_reference_resolver to our list of functions to
        # run when a target isn't found
        self.unknown_reference_resolvers = [self.wiki_resolver]
        # We create a new parser to process MoinMoin wiki style links in the 
        # restructured text.
        from MoinMoin.parser.wiki import Parser
        self.wikiparser = Parser('', self.request)
        self.wikiparser.formatter = self.formatter
        self.wikiparser.hilite_re = None
        self.nodes = []
        
        
    def translate(self):
        visitor = MoinTranslator(self.document, 
                                 self.formatter, 
                                 self.request,
                                 self.wikiparser,
                                 self)
        self.document.walkabout(visitor)
        self.visitor = visitor
        self.output = html_escape_unicode(visitor.astext())
        

# TODO: Evaluate if this would be easier to sub class the real moin wiki parser.
class Parser:
    
    # allow caching - This should be turned off when testing.
    caching = 1
    
    def __init__(self, raw, request, **kw):
        self.raw = raw
        self.request = request
        self.form = request.form
        
    def format(self, formatter):
        parts =  publish_parts(source = self.raw,
                               writer = MoinWriter(formatter, self.request))
        text = ''
        if parts['title']:
            text += '<h2>' + parts['title'] + '</h2>'
        # If there is only one subtitle then it is held in parts['subtitle'].
        # However, if there is more than one subtitle then this is empty and
        # fragment contains all of the subtitles.
        if parts['subtitle']:
            text += '<h3>' + parts['subtitle'] + '</h3>'
        text += parts['fragment']
        self.request.write(html_escape_unicode(text))
        

class MoinTranslator(html4css1.HTMLTranslator):

    def __init__(self, document, formatter, request, parser, writer):
        html4css1.HTMLTranslator.__init__(self, document)
        self.formatter = formatter
        self.request = request
        # We supply our own request.write so that the html is added to the
        # html4css1 body list instead of printed to stdout by the default
        # MoinTranslator writer. TODO: Confirm this is really what we're doing. 
        self.old_write = self.request.write
        self.request.write = self.rst_write
        self.wikiparser = parser
        self.wikiparser.request = request
        self.wikiparser.request.write = self.rst_write
        # When we use MoinMoin to interpret a MoinMoin refuri we want to strip
        # the paragraph tags to keep the correct formatting. This is used in
        # rst_write where by default we don't need to strip anything which is
        # why it is initialized to 0. 
        self.strip_paragraph = 0
        self.writer = writer
        # MoinMoin likes to start the initial headers at level 3 and the title
        # gets level 2, so to comply with their style's we do so here also. 
        # TODO: Could this be fixed by passing this value in settings_overrides?
        self.initial_header_level = 3
        
    def astext(self):
        self.request.write = self.old_write
        return html4css1.HTMLTranslator.astext(self)
    
    def rst_write(self, string):
        if self.strip_paragraph:
            replacements = {'<p>': '', '</p>': '', '\n': '', '> ': '>'}
            for src, dst in replacements.items():
                string = string.replace(src, dst)
            # Everything seems to have a space ending the text block. We want to
            # get rid of this
            if string and string[-1] == ' ':
                string = string[:-1]
        self.body.append(string)
        
    def visit_reference(self, node):
        target = None
        
        # These are the special link schemes that MoinMoin supports. We let
        # MoinMoin handle these types.
        moin_link_schemes = ['wiki:', 'attachment:', 'inline:', 'drawing:']
        
        # Do I need to lstrip? TODO: Find this out. Doesn't look like I need to
        # since I don't when assigning target.
        if 'refuri' in node.attributes:
            refuri = node['refuri']
            if [i for i in moin_link_schemes if refuri.lstrip().startswith(i)]:
                target = refuri
            # What is this? TODO: Figure this out and comment
            elif ('name' in node.attributes and 
                  fully_normalize_name(node['name']) == refuri):
                target = ':%s:' % (node['name'])
            # The node should have a whitespace normalized name if the docutlis 
            # restructured text parser would normally fully normalize the name. 
            elif ':' not in refuri:
                target = ':%s:' % (refuri)
        
            if target:
                self.strip_paragraph = 1
                
                # inline is special. We're not doing a link really, we need
                # moinmoin to actually insert the attachment.
                if target.startswith('inline:'):
                    self.wikiparser.raw = target 
                    self.wikiparser.format(self.formatter)
                else:
                    # Sometimes a newline will creep in to the node's text. This
                    # throws the moinmoin regex so a link will not get processed.
                    node_text = node.astext().replace('\n', ' ')
                    self.wikiparser.raw = '[%s %s]' % (target, 
                                                       node_text)
                    self.wikiparser.format(self.formatter)
                    
                self.strip_paragraph = 0
                raise docutils.nodes.SkipNode
                return
                
        html4css1.HTMLTranslator.visit_reference(self, node)


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

    def depart_definition_list(self, node):
        self.request.write(self.formatter.definition_list(0))


    #
    # Admonitions
    #

    def visit_warning(self, node):
        self.request.write(self.formatter.highlight(1))

    def depart_warning(self, node):
        self.request.write(self.formatter.highlight(0))

    def visit_image(self, node):
        # We need process inline images with moinmoin. Otherwise let the normal
        # rst writer do its magic.
        if node['uri'].lstrip().startswith('inline:'):
            self.strip_paragraph = 1
            self.wikiparser.raw = node['uri'] 
            self.wikiparser.format(self.formatter)
            self.strip_paragraph = 0
            raise docutils.nodes.SkipNode
            return
        html4css1.HTMLTranslator.visit_image(self, node)
        
