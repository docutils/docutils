# Author: Aahz
# Contact: aahz@pythoncraft.com
# Revision: 
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
OpenOffice writer

The output is an OpenOffice.org 1.0-compatible document.
"""

__docformat__ = 'reStructuredText'


import sys
from warnings import warn
import re

import docutils
from docutils import nodes, utils, writers, languages

import OOtext


section_styles = [
    '.ch title',
    '.head 1',
    '.head 2'
    ]

class Writer(writers.Writer):

    supported = ('OpenOffice')
    """Formats this writer supports."""

    def __init__(self):
        writers.Writer.__init__(self)
        self.output = None
        self.translator_class = Translator

    def translate(self):
        visitor = self.translator_class(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()


class Translator(nodes.NodeVisitor):

    header = [OOtext.content_header]
    footer = [OOtext.content_footer]

    start_para = '\n<text:p text:style-name="%s">\n'
    end_para = '\n</text:p>\n'

    start_charstyle = '<text:span text:style-name="%s">'
    end_charstyle = '</text:span>'

    line_break = '\n<text:line-break/>'
    re_spaces = re.compile('  +')
    spaces = '<text:s text:c="%d"/>'

    re_annotation = re.compile(r'#\d+(?:, #\d+)*$')

    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.settings = document.settings
        self.body = []
        self.section_level = 0
        self.skip_para_tag = False

    def astext(self):
        return ''.join(self.header + self.body + self.footer)

    def encode(self, text):
        """Encode special characters in `text` & return."""
        # @@@ A codec to do these and all other HTML entities would be nice.
        text = text.replace("&", "&amp;")
        text = text.replace("<", "&lt;")
        text = text.replace('"', "&quot;")
        text = text.replace(">", "&gt;")
        return text

    def compress_spaces(self, line):
        while 1:
            match = self.re_spaces.search(line)
            if match:
                start, end = match.span()
                numspaces = end - start
                line = line[:start] + (self.spaces % numspaces) + line[end:]
            else:
                break
        return line

    def fix_annotation(self, line):
        match = self.re_annotation.search(line)
        if match:
            pos = match.start()
            line = line[:pos] + '|' + line[pos:]
        return line

    def visit_Text(self, node):
        self.body.append(self.encode(node.astext()))

    def depart_Text(self, node):
        pass

    def visit_admonition(self, node, name):
        self.skip_para_tag = True
        self.body.append(self.start_para % '.CALLOUT')

    def depart_admonition(self):
        self.body.append(self.end_para)
        self.skip_para_tag = False

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

    def visit_block_quote(self, node):
        self.skip_para_tag = True
        self.body.append(self.start_para % '.quotes')

    def depart_block_quote(self, node):
        self.body.append(self.end_para)
        self.skip_para_tag = False

    def visit_bullet_list(self, node):
        self.body.append('\n<text:unordered-list text:style-name=".bullet">\n')

    def depart_bullet_list(self, node):
        self.body.append('</text:unordered-list>\n')

    def visit_caption(self, node):
        self.body.append(self.starttag(node, 'p', '', CLASS='caption'))

    def depart_caption(self, node):
        self.body.append('</p>\n')

    def visit_caution(self, node):
        self.visit_admonition(node, 'caution')

    def depart_caution(self, node):
        self.depart_admonition()

    def visit_citation(self, node):
        self.body.append(self.starttag(node, 'table', CLASS='citation',
                                       frame="void", rules="none"))
        self.footnote_backrefs(node)

    def depart_citation(self, node):
        self.body.append('</td></tr>\n'
                         '</tbody>\n</table>\n')

    def visit_citation_reference(self, node):
        href = ''
        if node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        self.body.append(self.starttag(node, 'a', '[', href=href,
                                       CLASS='citation-reference'))

    def depart_citation_reference(self, node):
        self.body.append(']</a>')

    def visit_classifier(self, node):
        self.body.append(' <span class="classifier-delimiter">:</span> ')
        self.body.append(self.starttag(node, 'span', '', CLASS='classifier'))

    def depart_classifier(self, node):
        self.body.append('</span>')

    def visit_colspec(self, node):
        self.colspecs.append(node)

    def depart_colspec(self, node):
        pass

    def write_colspecs(self):
        width = 0
        for node in self.colspecs:
            width += node['colwidth']
        for node in self.colspecs:
            colwidth = int(node['colwidth'] * 100.0 / width + 0.5)
            self.body.append(self.emptytag(node, 'col',
                                           colwidth='%i%%' % colwidth))
        self.colspecs = []

    def visit_comment(self, node):
        raise nodes.SkipNode

    def visit_decoration(self, node):
        pass

    def depart_decoration(self, node):
        pass

    def visit_definition(self, node):
        self.body.append('</dt>\n')
        self.body.append(self.starttag(node, 'dd', ''))
        if len(node) and isinstance(node[0], nodes.paragraph):
            node[0].set_class('first')

    def depart_definition(self, node):
        self.body.append('</dd>\n')

    def visit_definition_list(self, node):
        print node.astext()
        self.body.append(self.starttag(node, 'dl'))

    def depart_definition_list(self, node):
        self.body.append('</dl>\n')

    def visit_definition_list_item(self, node):
        pass

    def depart_definition_list_item(self, node):
        pass

    def visit_description(self, node):
        self.body.append(self.starttag(node, 'td', ''))
        if len(node) and isinstance(node[0], nodes.paragraph):
            node[0].set_class('first')

    def depart_description(self, node):
        self.body.append('</td>')

    def visit_doctest_block(self, node):
        self.visit_literal_block(node)

    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass

    def visit_emphasis(self, node):
        self.body.append(self.start_charstyle % 'italic')

    def depart_emphasis(self, node):
        self.body.append(self.end_charstyle)

    def visit_entry(self, node):
        if isinstance(node.parent.parent, nodes.thead):
            tagname = 'th'
        else:
            tagname = 'td'
        atts = {}
        if node.has_key('morerows'):
            atts['rowspan'] = node['morerows'] + 1
        if node.has_key('morecols'):
            atts['colspan'] = node['morecols'] + 1
        self.body.append(self.starttag(node, tagname, '', **atts))
        self.context.append('</%s>\n' % tagname.lower())
        if len(node) == 0:              # empty cell
            self.body.append('&nbsp;')
        elif isinstance(node[0], nodes.paragraph):
            node[0].set_class('first')

    def depart_entry(self, node):
        self.body.append(self.context.pop())

    def visit_enumerated_list(self, node):
        """
        The 'start' attribute does not conform to HTML 4.01's strict.dtd, but
        CSS1 doesn't help. CSS2 isn't widely enough supported yet to be
        usable.
        """
        atts = {}
        if node.has_key('start'):
            atts['start'] = node['start']
        if node.has_key('enumtype'):
            atts['class'] = node['enumtype']
        # @@@ To do: prefix, suffix. How? Change prefix/suffix to a
        # single "format" attribute? Use CSS2?
        old_compact_simple = self.compact_simple
        self.context.append((self.compact_simple, self.compact_p))
        self.compact_p = None
        self.compact_simple = (self.options.compact_lists and
                               (self.compact_simple
                                or self.topic_class == 'contents'
                                or self.check_simple_list(node)))
        if self.compact_simple and not old_compact_simple:
            atts['class'] = (atts.get('class', '') + ' simple').strip()
        self.body.append(self.starttag(node, 'ol', **atts))

    def depart_enumerated_list(self, node):
        self.compact_simple, self.compact_p = self.context.pop()
        self.body.append('</ol>\n')

    def visit_error(self, node):
        self.visit_admonition(node, 'error')

    def depart_error(self, node):
        self.depart_admonition()

    def visit_field(self, node):
        self.body.append(self.starttag(node, 'tr', '', CLASS='field'))

    def depart_field(self, node):
        self.body.append('</tr>\n')

    def visit_field_body(self, node):
        self.body.append(self.starttag(node, 'td', '', CLASS='field-body'))
        if len(node) and isinstance(node[0], nodes.paragraph):
            node[0].set_class('first')

    def depart_field_body(self, node):
        self.body.append('</td>\n')

    def visit_field_list(self, node):
        self.body.append(self.starttag(node, 'table', frame='void',
                                       rules='none', CLASS='field-list'))
        self.body.append('<col class="field-name" />\n'
                         '<col class="field-body" />\n'
                         '<tbody valign="top">\n')

    def depart_field_list(self, node):
        self.body.append('</tbody>\n</table>\n')

    def visit_field_name(self, node):
        atts = {}
        if self.in_docinfo:
            atts['class'] = 'docinfo-name'
        else:
            atts['class'] = 'field-name'
        if len(node.astext()) > 14:
            atts['colspan'] = 2
            self.context.append('</tr>\n<tr><td>&nbsp;</td>')
        else:
            self.context.append('')
        self.body.append(self.starttag(node, 'th', '', **atts))

    def depart_field_name(self, node):
        self.body.append(':</th>')
        self.body.append(self.context.pop())

    def visit_figure(self, node):
        self.body.append(self.start_para % '.figure')

    def depart_figure(self, node):
        self.body.append(self.end_para)

    def visit_footnote(self, node):
        raise nodes.SkipNode

    def footnote_backrefs(self, node):
        warn("footnote backrefs not available")

    def depart_footnote(self, node):
        pass

    def visit_footnote_reference(self, node):
        name = node['refid']
        id = node['id']
        number = node['auto']
        for footnote in self.document.autofootnotes:
            if name == footnote['name']:
                break
        self.body.append('<text:footnote text:id="%s">\n' % id)
        self.body.append('<text:footnote-citation text:string-value="%s"/>\n' % number)
        self.body.append('<text:footnote-body>\n')
        self.body.append(self.start_para % '.body')
        for child in footnote.children:
            if isinstance(child, nodes.paragraph):
                self.body.append(child.astext())
        self.body.append(self.end_para)
        self.body.append('</text:footnote-body>\n')
        self.body.append('</text:footnote>')
        raise nodes.SkipNode

    def depart_footnote_reference(self, node):
        pass

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

    def visit_header(self, node):
        self.context.append(len(self.body))

    def depart_header(self, node):
        start = self.context.pop()
        self.body_prefix.append(self.starttag(node, 'div', CLASS='header'))
        self.body_prefix.extend(self.body[start:])
        self.body_prefix.append('<hr />\n</div>\n')
        del self.body[start:]

    def visit_hint(self, node):
        self.visit_admonition(node, 'hint')

    def depart_hint(self, node):
        self.depart_admonition()

    def visit_image(self, node):
        name = "Figure: %s\n" % node.attributes['uri']
        self.body.append(name)

    def depart_image(self, node):
        pass

    def visit_important(self, node):
        self.visit_admonition(node, 'important')

    def depart_important(self, node):
        self.depart_admonition()

    def visit_index_entry(self, node):
        index_format = '<text:alphabetical-index-mark text:string-value="%s"/>\n'
        self.body.append(self.start_para % '.body')
        entries = node.astext().split('\n')
        for entry in entries:
            self.body.append(index_format % self.encode(entry))
        self.body.append(self.end_para)
        raise nodes.SkipNode

    def visit_interpreted(self, node):
        # @@@ Incomplete, pending a proper implementation on the
        # Parser/Reader end.
        #self.body.append(node['role'] + ':')
        self.body.append(node.astext())
        raise nodes.SkipNode

    def depart_interpreted(self, node):
        pass

    # Don't need footnote labels/numbers
    def visit_label(self, node):
        print "!"
        raise nodes.SkipNode

    def visit_legend(self, node):
        self.body.append(self.starttag(node, 'div', CLASS='legend'))

    def depart_legend(self, node):
        self.body.append('</div>\n')

    def visit_line_block(self, node):
        self.body.append(self.start_para % '.quotes')
        lines = node.astext()
        lines = lines.split('\n')
        lines = self.line_break.join(lines)
        self.body.append(lines)
        self.body.append(self.end_para)
        raise nodes.SkipNode

    def visit_list_item(self, node):
        self.body.append('<text:list-item>')

    def depart_list_item(self, node):
        self.body.append('</text:list-item>\n')

    def visit_literal(self, node):
        self.body.append(self.start_charstyle % 'code')

    def depart_literal(self, node):
        self.body.append(self.end_charstyle)

    def visit_literal_block(self, node):
        self.body.append(self.start_para % '.code first')
        self.body.append(self.end_para)
        lines = self.encode(node.astext())
        lines = lines.split('\n')
        while lines[-1] == '':
            lines.pop()
        for line in lines:
            self.body.append(self.start_para % '.code')
            line = self.fix_annotation(line)
            line = self.compress_spaces(line)
            self.body.append(line)
            self.body.append(self.end_para)
        self.body.append(self.start_para % '.code last')
        self.body.append(self.end_para)
        raise nodes.SkipNode

    def visit_note(self, node):
        self.visit_admonition(node, '.note')

    def depart_note(self, node):
        self.depart_admonition()

    def visit_option(self, node):
        if self.context[-1]:
            self.body.append(', ')

    def depart_option(self, node):
        self.context[-1] += 1

    def visit_option_argument(self, node):
        self.body.append(node.get('delimiter', ' '))
        self.body.append(self.starttag(node, 'var', ''))

    def depart_option_argument(self, node):
        self.body.append('</var>')

    def visit_option_group(self, node):
        atts = {}
        if len(node.astext()) > 14:
            atts['colspan'] = 2
            self.context.append('</tr>\n<tr><td>&nbsp;</td>')
        else:
            self.context.append('')
        self.body.append(self.starttag(node, 'td', **atts))
        self.body.append('<kbd>')
        self.context.append(0)          # count number of options

    def depart_option_group(self, node):
        self.context.pop()
        self.body.append('</kbd></td>\n')
        self.body.append(self.context.pop())

    def visit_option_list(self, node):
        self.body.append(
              self.starttag(node, 'table', CLASS='option-list',
                            frame="void", rules="none"))
        self.body.append('<col class="option" />\n'
                         '<col class="description" />\n'
                         '<tbody valign="top">\n')

    def depart_option_list(self, node):
        self.body.append('</tbody>\n</table>\n')

    def visit_option_list_item(self, node):
        self.body.append(self.starttag(node, 'tr', ''))

    def depart_option_list_item(self, node):
        self.body.append('</tr>\n')

    def visit_option_string(self, node):
        self.body.append(self.starttag(node, 'span', '', CLASS='option'))

    def depart_option_string(self, node):
        self.body.append('</span>')

    def visit_paragraph(self, node):
        if not self.skip_para_tag:
            self.body.append(self.start_para % '.body')

    def depart_paragraph(self, node):
        if not self.skip_para_tag:
            self.body.append(self.end_para)

    def visit_problematic(self, node):
        if node.hasattr('refid'):
            self.body.append('<a href="#%s" name="%s">' % (node['refid'],
                                                           node['id']))
            self.context.append('</a>')
        else:
            self.context.append('')
        self.body.append(self.starttag(node, 'span', '', CLASS='problematic'))

    def depart_problematic(self, node):
        self.body.append('</span>')
        self.body.append(self.context.pop())

    def visit_raw(self, node):
        if node.has_key('format') and node['format'] == 'html':
            self.body.append(node.astext())
        raise nodes.SkipNode

    def visit_reference(self, node):
        pass

    def depart_reference(self, node):
        pass

    def visit_row(self, node):
        self.body.append(self.starttag(node, 'tr', ''))

    def depart_row(self, node):
        self.body.append('</tr>\n')

    def visit_section(self, node):
        self.section_level += 1

    def depart_section(self, node):
        self.section_level -= 1

    def visit_strong(self, node):
        self.body.append('<strong>')

    def depart_strong(self, node):
        self.body.append('</strong>')

    def visit_table(self, node):
        self.body.append(
              self.starttag(node, 'table', CLASS="table",
                            frame='border', rules='all'))

    def depart_table(self, node):
        self.body.append('</table>\n')

    def visit_target(self, node):
        if not (node.has_key('refuri') or node.has_key('refid')
                or node.has_key('refname')):
            self.body.append(self.starttag(node, 'a', '', CLASS='target'))
            self.context.append('</a>')
        else:
            self.context.append('')

    def depart_target(self, node):
        self.body.append(self.context.pop())

    def visit_tbody(self, node):
        self.write_colspecs()
        self.body.append(self.context.pop()) # '</colgroup>\n' or ''
        self.body.append(self.starttag(node, 'tbody', valign='top'))

    def depart_tbody(self, node):
        self.body.append('</tbody>\n')

    def visit_term(self, node):
        self.body.append(self.starttag(node, 'dt', ''))

    def depart_term(self, node):
        """
        Leave the end tag to `self.visit_definition()`, in case there's a
        classifier.
        """
        pass

    def visit_tgroup(self, node):
        # Mozilla needs <colgroup>:
        self.body.append(self.starttag(node, 'colgroup'))
        # Appended by thead or tbody:
        self.context.append('</colgroup>\n')

    def depart_tgroup(self, node):
        pass

    def visit_thead(self, node):
        self.write_colspecs()
        self.body.append(self.context.pop()) # '</colgroup>\n'
        # There may or may not be a <thead>; this is for <tbody> to use:
        self.context.append('')
        self.body.append(self.starttag(node, 'thead', valign='bottom'))

    def depart_thead(self, node):
        self.body.append('</thead>\n')

    def visit_tip(self, node):
        self.visit_admonition(node, 'tip')

    def depart_tip(self, node):
        self.depart_admonition()

    def visit_title(self, node):
        """Only 3 section levels are supported by this writer."""
        title_tag = self.start_para % section_styles[self.section_level]
        self.body.append(title_tag)

    def depart_title(self, node):
        self.body.append(self.end_para)

    def visit_warning(self, node):
        self.visit_admonition(node, 'warning')

    def depart_warning(self, node):
        self.depart_admonition()

    def visit_system_message(self, node):
        print node.astext()

    def depart_system_message(self, node):
        pass

    def unknown_visit(self, node):
        print "Failure processing at line", node.line
        print "Failure is", node.astext()
        raise NotImplementedError('visiting unimplemented node type: %s'
                                  % node.__class__.__name__)
