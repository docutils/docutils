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
 * The output conforms to the XHTML version 1.1 DTD.
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
    This writer generates XHTML 1.1
    without formatting hints that interfere with a CSS stylesheet.
    """
    doctype = ('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" '
               '"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">\n')
    doctype_mathml = (
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN" '
        '"http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd">\n')

    # there is no attribute "lang" in XHTML 1.1
    head_prefix_template = ('<html xmlns="http://www.w3.org/1999/xhtml"'
                            ' xml:lang="%(lang)s">\n<head>\n')
    lang_attribute = 'xml:lang' # changed from 'lang' in XHTML 1.0


    # Do not  mark the first child with 'class="first"' and the last
    # child with 'class="last"' in definitions, table cells, field
    # bodies, option descriptions, and list items. Use the
    # ``:first-child`` and ``:last-child`` selectors instad.

    def set_first_last(self, node):
        pass

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
        self.body.append('</dd>\n')
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
        self.body.append('<dt class="%s">%s</dt>\n'
                         % (name, self.language.labels[name]))
        self.body.append(self.starttag(node, 'dd', '', CLASS=name))

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
            self.body.append('<dl class="footnote">')
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
    def label_delim(self, node, bracket, superscript):
        """put brackets around label?"""
        if isinstance(node.parent, nodes.footnote):
            if self.settings.footnote_references == 'brackets':
                return bracket
            else:
                return superscript
        else:
            assert isinstance(node.parent, nodes.citation)
            return bracket

    def visit_label(self, node):
        # Context added in footnote_backrefs.
        suffix = '%s%s' % (self.context.pop(),
                           self.label_delim(node, '[', ''))
        self.body.append(self.starttag(node, 'dt', suffix, CLASS='label'))

    def depart_label(self, node):
        delim = self.label_delim(node, ']', '')
        # Context added in footnote_backrefs.
        backref = self.context.pop()
        text = self.context.pop()
        # <dd> starttag added in visit_footnote() / visit_citation()
        starttag = self.context.pop()
        self.body.append('%s%s</dt>\n%s%s' % (delim, backref, starttag, text))


    def visit_generated(self, node):
        if 'sectnum' in node['classes']:
            # get section number (strip trailing no-break-spaces)
            sectnum = node.astext().rstrip(u' ')
            # print sectnum.encode('utf-8')
            self.body.append('<span class="sectnum">%s</span> '
                                    % self.encode(sectnum))
            # Content already processed:
            raise nodes.SkipNode

    def depart_generated(self, node):
        pass

    # Do not  mark the first child with 'class="first"'
    def visit_list_item(self, node):
        self.body.append(self.starttag(node, 'li', ''))

    # inline literal
    def visit_literal(self, node):
        """Process text to prevent in-word line wrapping."""
        self.body.append(
            self.starttag(node, 'tt', '', CLASS='literal'))
        text = node.astext()
        # remove hard line breaks (except if in a parsed-literal block)
        if not isinstance(node.parent, nodes.literal_block):
            text = text.replace('\n', ' ')
        # Protect text like ``--an-option`` and the regular expression
        # ``[+]?(\d+(\.\d*)?|\.\d+)`` from bad line wrapping
        for token in self.words_and_spaces.findall(text):
            if token.strip() and self.sollbruchstelle.search(token):
                self.body.append('<span class="pre">%s</span>'
                                    % self.encode(token))
            else:
                self.body.append(self.encode(token))
        self.body.append('</tt>')
        # Content already processed:
        raise nodes.SkipNode

    # literal block: no newline after <pre> tag 
    # (leads to blank line in XHTML1.1)
    def visit_literal_block(self, node,):
        self.body.append(self.starttag(node, 'pre', suffix='', 
                                       CLASS='literal-block'))

    # Meta tags: 'lang' attribute replaced by 'xml:lang' in XHTML 1.1
    def visit_meta(self, node):
        if node.hasattr('lang'):
            node['xml:lang'] = node['lang']
            del(node['lang'])
        meta = self.emptytag(node, 'meta', **node.non_default_attributes())
        self.add_meta(meta)


    # option-list as definition list, styled with CSS
    # ----------------------------------------------

    def visit_option_list(self, node):
        self.body.append(
            self.starttag(node, 'dl', CLASS='option-list'))

    def depart_option_list(self, node):
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

    # Do not omit <p> tags
    # --------------------
    #
    # The HTML4CSS1 writer does this to "produce
    # visually compact lists (less vertical whitespace)". This writer
    # relies on CSS rules for"visual compactness".
    #
    # * In XHTML 1.1, e.g. a <blockquote> element may not contain
    #   character data, so you cannot drop the <p> tags.
    # * Keeping simple paragraphs in the field_body enables a CSS
    #   rule to start the field-body on new line if the label is too long
    # * it makes the code simpler.
    #
    # TODO: omit paragraph tags in simple table cells. 

    def visit_paragraph(self, node):
        self.body.append(self.starttag(node, 'p', ''))

    def depart_paragraph(self, node):
        self.body.append('</p>')
        if not (isinstance(node.parent, (nodes.list_item, nodes.entry)) and 
                # (node is node.parent[-1])
                (len(node.parent) == 1)
               ):
            self.body.append('\n')


    # no hard-coded border setting in the table head
    # ----------------------------------------------

    def visit_table(self, node):
        classes = [cls.strip(u' \t\n')
                   for cls in self.settings.table_style.split(',')]
        tag = self.starttag(node, 'table', CLASS=' '.join(classes))
        self.body.append(tag)


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
