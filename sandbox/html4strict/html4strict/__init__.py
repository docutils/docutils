# .. coding: utf8
# :Author: Günter Milde <milde@users.berlios.de>
# :Revision: $Revision$
# :Date: $Date: 2005-06-28$
# :Copyright: © 2005, 2009 Günter Milde.
#             Released  without warranties or conditions of any kind
#             under the terms of the Apache License, Version 2.0
#             http://www.apache.org/licenses/LICENSE-2.0

"""
Strict HyperText Markup Language document tree Writer.

This is a variant of Docutils' standard 'html4css1' writer.

GOAL:
 * The output conforms to the XHTML version 1.0 Strict DTD.
 * It contains no hard-coded formatting information that would prevent
   layout design by cascading style sheets.
"""

__docformat__ = 'reStructuredText'

import os
import os.path
import re

import docutils
from docutils import frontend, nodes, utils, writers, languages
from docutils.writers import html4css1

class Writer(html4css1.Writer):

    supported = ('html', 'xhtml', 'xhtml1',
                 'html4strict', 'xhtml1strict',
                 'html4css2', 'xhtml1css2')
    """Formats this writer supports."""

    default_stylesheets = ['html4css1.css', 'html4css2.css']

    default_stylesheet_path = ','.join(
        [os.path.join(os.path.dirname(__file__), stylesheet)
         for stylesheet in default_stylesheets])

    config_section = 'html4strict writer'
    config_section_dependencies = ('writers', 'html4css1 writer')

    settings_spec = frontend.filter_settings_spec(
        html4css1.Writer.settings_spec,
        'field_name_limit', 'option_limit',
        stylesheet_path = (
            'Specify comma separated list of stylesheet paths. '
            'With --link-stylesheet, '
            'the path is rewritten relative to the output HTML file. '
            'Default: "%s"' % default_stylesheet_path,
            ['--stylesheet-path'],
            {'metavar': '<file>', 'overrides': 'stylesheet',
            'default': default_stylesheet_path}))

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = HTMLTranslator


class HTMLTranslator(html4css1.HTMLTranslator):
    """
    This writer generates XHTML 1.0 Strict
    without formatting hints that interfere with a CSS stylesheet.
    """
    doctype = ('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" '
               '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">\n')

    # Compact lists
    # ------------
    # Include field lists (in addition to ordered and unordered lists)
    # in the test if a list is "simple"  (cf. the html4css1.HTMLTranslator
    # docstring and the SimpleListChecker class at the end of this file).

    def is_compactable(self, node):
        # print "is_compactable %s ?" % node.__class__,
        # explicite class arguments have precedence
        if 'compact' in node['classes']:
            # print "explicitely compact"
            return True
        if 'open' in node['classes']:
            # print "explicitely open"
            return False
        # check config setting:
        if (isinstance(node, nodes.field_list) and
            not self.settings.compact_field_lists):
            # print "`compact-field-lists` is False"
            return False
        if (isinstance(node, nodes.enumerated_list) or
            isinstance(node, nodes.bullet_list)
           ) and not self.settings.compact_lists:
            # print "`compact-lists` is False"
            return False
        # more special cases:
        if (self.compact_simple or self.topic_classes == ['contents']):
            # print "self.compact_simple is True"
            return True
        # check the list items:
        visitor = SimpleListChecker(self.document)
        try:
            node.walk(visitor)
        except nodes.NodeFound:
            # print "complex node"
            return False
        else:
            # print "simple list"
            return True


    # citations
    # ---------
    # Use definition list instead of table for bibliographic references.
    # Join adjacent citation entries.

    def visit_citation(self, node):
        if self.body[-1] == '<-- next citation -->':
            del(self.body[-1])
        else:
            self.body.append('<dl class="citation">')
        self.context.append(self.starttag(node, 'dd'))
        self.footnote_backrefs(node)

    def depart_citation(self, node):
        self.body.append('<dd>\n')
        if isinstance(node.next_node(), nodes.citation):
            self.body.append('<-- next citation -->')
        else:
            self.body.append('</dl>\n')

    # docinfo
    # -------
    # use definition list instead of table

    def visit_docinfo(self, node):
        classes = 'docinfo'
        if (self.is_compactable(node)):
            classes += ' simple'
        self.body.append(self.starttag(node, 'dl', CLASS=classes))

    def depart_docinfo(self, node):
        self.body.append('</dl>\n')

    def visit_docinfo_item(self, node, name, meta=1):
        if meta:
            meta_tag = '<meta name="%s" content="%s" />\n' \
                       % (name, self.attval(node.astext()))
            self.add_meta(meta_tag)
        self.body.append('<dt>%s</dt>\n'
                         % self.language.labels[name])
        self.body.append(self.starttag(node, 'dd', ''))

    def depart_docinfo_item(self):
        self.body.append('</dd>\n')


    # enumerated lists
    # ----------------
    # The 'start' attribute does not conform to HTML4/XHTML1 Strict
    # (it will resurface in HTML5)

    def visit_enumerated_list(self, node):
        atts = {}
        if 'start' in node:
            atts['style'] = 'counter-reset: item %d;' % (
                                                node['start'] - 1)
        classes = node.setdefault('classes', [])
        if 'enumtype' in node:
            classes.append(node['enumtype'])
        if self.is_compactable(node) and not self.compact_simple:
            classes.append('simple')
        # @@@ To do: prefix, suffix. (?)
        self.context.append((self.compact_simple, self.compact_p))
        self.compact_p = False
        self.body.append(self.starttag(node, 'ol', **atts))

    # field-list
    # ----------
    # set as definition list, styled with CSS

    def visit_field_list(self, node):
        # Keep simple paragraphs in the field_body to enable CSS
        # rule to start body on new line if the label is too long
        self.context.append((self.compact_field_list, self.compact_p))
        self.compact_field_list, self.compact_p = False, False
        #
        classes = 'field-list'
        if (self.is_compactable(node)):
            classes += ' simple'
        self.body.append(self.starttag(node, 'dl', CLASS=classes))

    def depart_field_list(self, node):
        self.compact_field_list, self.compact_p = self.context.pop()
        self.body.append('</dl>\n')

    def visit_field(self, node):
        pass

    def depart_field(self, node):
        pass

    def visit_field_name(self, node):
        self.body.append(self.starttag(node, 'dt', ''))

    def depart_field_name(self, node):
        self.body.append('</dt>\n')

    def visit_field_body(self, node):
        self.body.append(self.starttag(node, 'dd', ''))

    def depart_field_body(self, node):
        self.body.append('</dd>\n')

    # footnotes
    # ---------
    # use definition list instead of table for footnote text

    def visit_footnote(self, node):
        if self.body[-1] == '<-- next footnote -->':
            del(self.body[-1])
        else:
            self.body.append('<dl class="docutils footnote">')
        self.context.append(self.starttag(node, 'dd'))
        self.footnote_backrefs(node)

    def depart_footnote(self, node):
        self.body.append('</dd>\n')
        next_siblings = node.traverse(descend=0, siblings=1,
                                      include_self=0)
        next = next_siblings and next_siblings[0]
        if isinstance(next, nodes.footnote):
            self.body.append('<-- next footnote -->')
        else:
            self.body.append('</dl>\n')

    # footnote and citation label
    def visit_label(self, node):
        # Context added in footnote_backrefs.
        self.body.append(self.starttag(node, 'dt', '%s[' % self.context.pop(),
                                       CLASS='label'))

    def depart_label(self, node):
        # Context added in footnote_backrefs.
        backref = self.context.pop()
        text = self.context.pop()
        # <dd> starttag added in visit_footnote() / visit_citation()
        starttag = self.context.pop()
        self.body.append(']%s</dt>\n%s%s' % (backref, starttag, text))

    # Literal role pre-formatted
    # --------------------------

    # Setting tt.literal to {white-space: pre} (in the style sheet)  will
    # protect text like "--an-option" or the regular expression
    # ``[+]?(\d+(\.\d*)?|\.\d+)`` from bad line wrapping and protect runs of
    # multiple spaces but allow clean HTML code (see bug #1938891)
    #
    # The wrapping can be configured with CSS (see ``html4css2.css``).
    # Possible values: normal, nowrap, pre, pre-wrap, pre-line.

    def visit_literal(self, node):
        self.body.append(
            self.starttag(node, 'tt', '', CLASS='docutils literal'))

    def depart_literal(self, node):
        self.body.append('</tt>')

    # option-list as definition list, styled with CSS
    # ----------------------------------------------

    def visit_option_list(self, node):
        # Keep also simple paragraphs in the field_body to enable CSS
        # rule to start body on new line if label is too long
        self.context.append((self.compact_field_list, self.compact_p))
        self.compact_field_list, self.compact_p = False, False
        self.body.append(
            self.starttag(node, 'dl', CLASS='docutils option-list'))

    def depart_option_list(self, node):
        self.compact_field_list, self.compact_p = self.context.pop()
        self.body.append('</dl>\n')

    def visit_option_list_item(self, node):
        pass

    def depart_option_list_item(self, node):
        pass

    def visit_option_group(self, node):
        self.body.append(self.starttag(node, 'dt', ''))
        self.body.append('<kbd>')

    def depart_option_group(self, node):
        self.body.append('</kbd></dt>\n')

    def visit_option(self, node):
        self.body.append(self.starttag(node, 'span', '', CLASS='option'))

    def depart_option(self, node):
        self.body.append('</span>')
        if isinstance(node.next_node(descend=False, siblings=True),
                      nodes.option):
            self.body.append(', ')

    def visit_description(self, node):
        self.body.append(self.starttag(node, 'dd', ''))

    def depart_description(self, node):
        self.body.append('</dd>\n')

    # no hard-coded border setting in the table head
    # ----------------------------------------------

    def visit_table(self, node):
        self.body.append(
            self.starttag(node, 'table', CLASS='docutils table'))


class SimpleListChecker(html4css1.SimpleListChecker):

    """
    Raise `nodes.NodeFound` if non-simple list item is encountered.

    Here "simple" means a list item containing nothing other than a single
    paragraph, a simple list, or a paragraph followed by a simple list.

    This version also checks for simple field lists and docinfo.
    """
    # # debugging: copy of parent methods with `print` calls
    # def default_visit(self, node):
    #     print "found", node.__class__, "in", node.parent.__class__
    #     raise nodes.NodeFound

    def _pass_node(self, node):
        pass

    def _simple_node(self, node):
        # nodes that are never complex (can contain only inline nodes)
        raise nodes.SkipNode

    def visit_list_item(self, node):
        # print "visiting list item", node.__class__
        children = []
        for child in node.children:
            if not isinstance(child, nodes.Invisible):
                children.append(child)
        # print "has %s visible children" % len(children)
        if (children and isinstance(children[0], nodes.paragraph)
            and (isinstance(children[-1], nodes.bullet_list) or
                 isinstance(children[-1], nodes.enumerated_list) or
                 isinstance(children[-1], nodes.field_list))):
            children.pop()
        # print "%s children remain" % len(children)
        if len(children) <= 1:
            return
        else:
            # print "found", child.__class__, "in", node.__class__
            raise nodes.NodeFound

    # Docinfo nodes:
    visit_docinfo = _pass_node
    visit_author = _simple_node
    visit_authors = visit_list_item
    visit_address = visit_list_item
    visit_contact = _pass_node
    visit_copyright = _simple_node
    visit_date = _simple_node
    visit_organization = _simple_node
    visit_status = _simple_node
    visit_version = visit_list_item

    # Field list items
    visit_field_list = _pass_node
    visit_field = _pass_node
    # the field body corresponds to a list item
    # visit_field_body = html4css1.SimpleListChecker.visit_list_item
    visit_field_body = visit_list_item
    visit_field_name = html4css1.SimpleListChecker.invisible_visit

    # Inline nodes
    visit_Text = _pass_node
