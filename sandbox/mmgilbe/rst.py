# -*- coding: iso-8859-1 -*-
"""
    MoinMoin - ReStructured Text Parser

    @copyright: 2002 by J�rgen Hermann <jh@web.de>
    @license: GNU GPL, see COPYING for details.
"""

import sys
from docutils import core, nodes, utils
from docutils.parsers import rst


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


class MoinWriter(writers.Writer):

    config_section = 'moin writer'
    config_section_dependencies = ('writers',)

    output = None
    """Final translated form of `document`."""

    def __init__(self, formatter, request):
        writers.Writer.__init__(self)
        self.formatter = formatter
        self.request = request

    def translate(self):
        visitor = MoinTranslator(self.request, self.formatter, self.document)
        self.document.walkabout(visitor)
        # self.output = visitor.astext()
        # self.head_prefix = visitor.head_prefix
        # self.stylesheet = visitor.stylesheet
        # self.head = visitor.head
        # self.body_prefix = visitor.body_prefix
        # self.body_pre_docinfo = visitor.body_pre_docinfo
        # self.docinfo = visitor.docinfo
        # self.body = visitor.body
        # self.body_suffix = visitor.body_suffix


class Parser:
    """ Parse RST via "docutils".
    """

    def __init__(self, raw, request, **kw):
        self.raw = raw
        self.request = request
        self.form = request.form
        self._ = request.getText

    def format(self, formatter):
        """ Send the text.
        """
        #core.publish(source=self.raw, destination=sys.stdout, writer_name='html')
        text = publish_string(source = self.raw, 
                              writer = MoinWriter(formatter, self.request),
                              enable_exit = None, settings_overrides = {'traceback': 1})

        #document.walkabout(MoinTranslator(self.request, formatter, document))
""" replace print by request.write if you use this:
        if 0:
            dom = document.asdom()
            print '<pre>'
            print wikiutil.escape(dom.toprettyxml(indent='  '))
            print '</pre>'

        if 0:
            print '<pre>'
            print wikiutil.escape(document.astext())
            print '</pre>'
"""

def astext(node):
    s = node.astext().replace(u'\xa0', '&nbsp;')
    return s.replace(u'\u2020', '&#x2020;')
    
class MoinTranslator(nodes.GenericNodeVisitor):

    def __init__(self, request, formatter, document):
        nodes.NodeVisitor.__init__(self, document)
        self.request = request
        self.formatter = formatter
        self.level = 0

    def default_visit(self, node):
        #self.default_visit(node)
        self.request.write(self.formatter.highlight(1))
        self.request.write(self.formatter.preformatted(1))
        # self.request.write(self.formatter.text(node.pformat('\xA0\xA0\xA0\xA0', 1)))
        self.request.write(self.formatter.preformatted(0))
        self.request.write(self.formatter.text(astext(node)))
        self.request.write(self.formatter.highlight(0))

    def default_departure(self, node):
        pass

    def visit_document(self, node):
        pass

    def visit_section(self, node):
        self.level += 1

    def depart_section(self, node):
        self.level -= 1

    def visit_Text(self, node):
        self.request.write(self.formatter.text(astext(node)))

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
        #if self.topic_class == 'contents':
        #    self.request.write(self.formatter.linebreak())
        #else:
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
    # Field List
    #

    def visit_field_list(self, node):
        self.request.write(self.formatter.bullet_list(1))

    def visit_field(self, node):
        self.request.write(self.formatter.listitem(1))

    def visit_field_name(self, node):
        self.request.write(self.formatter.strong(1))

    def depart_field_name(self, node):
        self.request.write(': ' + self.formatter.strong(0))

    def visit_field_body(self, node):
        pass

    def depart_field(self, node):
        self.request.write(self.formatter.listitem(0))

    def depart_field_list(self, node):
        self.request.write(self.formatter.bullet_list(0))


    #
    # Links
    #

    def visit_reference(self, node):
        self.request.write(self.formatter.highlight(1))
        self.request.write('&lt;&lt;&lt;')

    def depart_reference(self, node):
        self.request.write('&gt;&gt;&gt;')
        self.request.write(self.formatter.highlight(0))


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

    def visit_system_message(self, node):
        self.request.write(self.formatter.highlight(1))
        self.request.write('[%s]' % astext(node))
        self.request.write(self.formatter.highlight(0))


