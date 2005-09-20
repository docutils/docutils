# Author: Aahz
# Contact: aahz@pythoncraft.com
# Revision: 
# Copyright: This module has been placed in the public domain.

"""
MIF writer

The output is a FrameMaker MIF file.
"""

__docformat__ = 'reStructuredText'

import sys
from warnings import warn
import re

import docutils
from docutils import nodes, utils, writers, languages
from docutils import transforms


LineBreak = '<Char HardReturn>'
LineBreak = r'\x09 '
NoBreakSpace = r'\x11 '
NoBreakHyphen = r'\x15 '
emDash = r'\xd1 '


class FrameBool(int):
    def __str__(self):
        if self:
            return 'Yes'
        else:
            return 'No'

# register_directive()


class makeFrameIDs(transforms.Transform):
    default_priority = 950

    def apply(self):
        frameID = 1000
        doc = self.document
        nodes = (doc.autofootnotes + doc.symbol_footnotes +
                doc.footnotes)
        for node in nodes:
            node['frameID'] = frameID
            frameID += 1


class Writer(writers.Writer):

    supported = ('MIF')
    """Formats this writer supports."""

    def get_transforms(self):
        return writers.Writer.get_transforms(self) + [makeFrameIDs]

    def __init__(self, style_map):
        writers.Writer.__init__(self)
        self.output = None
        self.style_map = style_map
        self.translator_class = Translator

    def translate(self):
        visitor = self.translator_class(self.document, self.style_map)
        self.document.walkabout(visitor)
        self.output = visitor.astext()


class Translator(nodes.NodeVisitor):
    header = ['<MIFFile 7.0>\n\n']
    footer = []

    re_spaces = re.compile('  +')

    re_annotation = re.compile(r'#\d+(?:, #\d+)*$')

    def __init__(self, document, style_map):
        nodes.NodeVisitor.__init__(self, document)
        self.settings = document.settings
        self.style_map = style_map
        self.body = []
        self.footnotes = []
        self.section_level = 0
        self.skip_para_tag = False
        self.in_index = False

        self.curr_contents = self.body

        self._indent_level = 0
        self._frameIDcounter = 1000

    def astext(self):
        return ''.join(self.header + self.footnotes + self.body + 
                self.footer)

    def encode(self, text):
        """Encode special characters in `text` & return."""
        text = text.replace('\\', '\\\\')
        text = text.replace("'", r'\q')
        text = text.replace('`', r'\Q')
        text = text.replace('\t', r'\t')
        text = text.replace('>', r'\>')
        return text

    def literal(self, text):
        if not self.in_index:
            return (" " * self._indent_level) + ("  <String `%s'>\n" % text)
        else:
            return text


    def charstyle_start(self, font):
        if not self.in_index:
            return "    <Font\n      <FTag `%s'>\n    >\n" % font
        else:
            return "<%s\>" % font

    def charstyle_end(self):
        if not self.in_index:
            return "    <Font\n      <FTag `'>\n    >\n"
        else:
            return "<Default Para Font\>"

    def para_start(self, font):
        indent = ' ' * self._indent_level
        tmp = []
        tmp.append(indent + "<Para\n")
        tmp.append(indent + " <PgfTag `%s'>\n" % font)
        tmp.append(indent + " <ParaLine\n")
        return ''.join(tmp)

    def para_end(self):
        indent = ' ' * self._indent_level
        tmp = []
        tmp.append(indent + " >\n")
        tmp.append(indent + ">\n")
        if self._indent_level == 0:
            tmp.append(indent + "\n")
        return ''.join(tmp)

    def visit_Text(self, node):
        text = node.astext()
        text = text.replace('.\n', '. \n')
        text = text.replace('\n', ' \n')
        lines = text.split('\n')
        for line in lines:
            line = self.literal(self.encode(line))
            self.curr_contents.append(line)

    def depart_Text(self, node):
        pass

    def visit_admonition(self, node, name):
        self.skip_para_tag = True
        self.body.append(self.para_start(self.style_map['admonition']))

    def depart_admonition(self):
        self.body.append(self.para_end())
        self.skip_para_tag = False

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

    def visit_block_quote(self, node):
        raise nodes.SkipNode
        self.skip_para_tag = True
        self.body.append(self.para_start('.quotes'))

    def depart_block_quote(self, node):
        self.body.append(self.para_end())
        self.skip_para_tag = False

    def visit_bullet_list(self, node):
        raise nodes.SkipNode
        self.body.append('\n<text:unordered-list text:style-name=".bullet">\n')

    def depart_bullet_list(self, node):
        self.body.append('</text:unordered-list>\n')

    def visit_caption(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'p', '', CLASS='caption'))

    def depart_caption(self, node):
        self.body.append('</p>\n')

    def visit_caution(self, node):
        self.visit_admonition(node, 'caution')

    def depart_caution(self, node):
        self.depart_admonition()

    def visit_citation(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'table', CLASS='citation',
                                       frame="void", rules="none"))
        self.footnote_backrefs(node)

    def depart_citation(self, node):
        self.body.append('</td></tr>\n'
                         '</tbody>\n</table>\n')

    def visit_citation_reference(self, node):
        raise NotImplementedError
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
        raise NotImplementedError
        self.body.append(' <span class="classifier-delimiter">:</span> ')
        self.body.append(self.starttag(node, 'span', '', CLASS='classifier'))

    def depart_classifier(self, node):
        self.body.append('</span>')

    def visit_colspec(self, node):
        raise NotImplementedError
        self.colspecs.append(node)

    def depart_colspec(self, node):
        pass

    def write_colspecs(self):
        raise NotImplementedError
        width = 0
        for node in self.colspecs:
            width += node['colwidth']
        for node in self.colspecs:
            colwidth = int(node['colwidth'] * 100.0 / width + 0.5)
            self.body.append(self.emptytag(node, 'col',
                                           colwidth='%i%%' % colwidth))
        self.colspecs = []

    def visit_comment(self, node):
        print node.astext()
        raise nodes.SkipNode

    def visit_decoration(self, node):
        pass

    def depart_decoration(self, node):
        pass

    def visit_definition(self, node):
        raise NotImplementedError
        self.body.append('</dt>\n')
        self.body.append(self.starttag(node, 'dd', ''))
        if len(node) and isinstance(node[0], nodes.paragraph):
            node[0].set_class('first')

    def depart_definition(self, node):
        self.body.append('</dd>\n')

    def visit_definition_list(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'dl'))

    def depart_definition_list(self, node):
        self.body.append('</dl>\n')

    def visit_definition_list_item(self, node):
        raise NotImplementedError
        raise nodes.SkipNode
        pass

    def depart_definition_list_item(self, node):
        pass

    def visit_description(self, node):
        raise NotImplementedError
        raise nodes.SkipNode
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
        self.body.append(self.charstyle_start(self.style_map['emphasis']))

    def depart_emphasis(self, node):
        self.body.append(self.charstyle_end())

    def visit_entry(self, node):
        raise NotImplementedError
        raise nodes.SkipNode
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
        raise NotImplementedError
        raise nodes.SkipNode
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
        raise NotImplementedError
        raise NotImplementedError
        self.body.append(self.starttag(node, 'tr', '', CLASS='field'))

    def depart_field(self, node):
        self.body.append('</tr>\n')

    def visit_field_body(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'td', '', CLASS='field-body'))
        if len(node) and isinstance(node[0], nodes.paragraph):
            node[0].set_class('first')

    def depart_field_body(self, node):
        self.body.append('</td>\n')

    def visit_field_list(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'table', frame='void',
                                       rules='none', CLASS='field-list'))
        self.body.append('<col class="field-name" />\n'
                         '<col class="field-body" />\n'
                         '<tbody valign="top">\n')

    def depart_field_list(self, node):
        self.body.append('</tbody>\n</table>\n')

    def visit_field_name(self, node):
        raise NotImplementedError
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
        raise nodes.SkipNode
        self.body.append(self.para_start('.figure'))

    def depart_figure(self, node):
        self.body.append(self.para_end())

    def visit_footnote(self, node):
        self.footnotes.append("<Notes\n <FNote\n")
        self.footnotes.append("  <ID %s>\n" % node['frameID'])
        self._indent_level += 2
        self.footnotes.append(self.para_start(self.style_map['footnote']))
        self.curr_contents = self.footnotes
        self.skip_para_tag = True

    def depart_footnote(self, node):
        self.footnotes.append(self.para_end())
        self._indent_level -= 2
        self.footnotes.append(' >\n>\n\n')
        self.skip_para_tag = False
        self.curr_contents = self.body

    def footnote_backrefs(self, node):
        warn("footnote backrefs not available")

    def visit_footnote_reference(self, node):
        name = node['refid']
        footnoteID = self.document.nameids[name]
        footnote = self.document.ids[footnoteID]
        self.body.append('  <FNote %s>\n' % footnote['frameID'])
        raise nodes.SkipNode

    def depart_footnote_reference(self, node):
        pass

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

    def visit_header(self, node):
        raise NotImplementedError
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
        raise nodes.SkipNode
        name = "Figure: %s\n" % node.attributes['uri']
        self.body.append(name)

    def depart_image(self, node):
        pass

    def visit_important(self, node):
        self.visit_admonition(node, 'important')

    def depart_important(self, node):
        self.depart_admonition()

    def visit_index_entry(self, node):
        self.body.append("    <Marker\n")
        self.body.append("      <MTypeName `Index'>\n")
        self.body.append("      <MText `")
        self.in_index = True
        return

    def depart_index_entry(self, node):
        self.body.append("'>\n")
        self.body.append("    >\n")
        self.in_index = False

    def visit_index_entry_level(self, node):
        pass

    def depart_index_entry_level(self, node):
        # index_entry can only contain index_entry_level
        if node.parent[-1] != node:
            self.body.append(':')

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
        raise nodes.SkipNode

    def depart_label(self, node):
        pass

    def visit_legend(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'div', CLASS='legend'))

    def depart_legend(self, node):
        self.body.append('</div>\n')

    def visit_line_block(self, node):
        raise nodes.SkipNode
        self.body.append(self.para_start('.quotes'))
        lines = node.astext()
        lines = lines.split('\n')
        lines = self.line_break.join(lines)
        self.body.append(lines)
        self.body.append(self.para_end())
        raise nodes.SkipNode

    def visit_list_item(self, node):
        raise nodes.SkipNode
        self.body.append('<text:list-item>')

    def depart_list_item(self, node):
        self.body.append('</text:list-item>\n')

    def visit_literal(self, node):
        self.body.append(self.charstyle_start(self.style_map['literal']))

    def depart_literal(self, node):
        self.body.append(self.charstyle_end())

    def visit_literal_block(self, node):
        lines = self.encode(node.astext())
        lines = lines.split('\n')
        while lines[-1] == '':
            lines.pop()
        for line in lines:
            self.body.append(self.para_start(self.style_map['literal_block']))
            self.body.append(self.literal(line))
            self.body.append(self.para_end())
        raise nodes.SkipNode

    def visit_note(self, node):
        self.visit_admonition(node, '.note')

    def depart_note(self, node):
        self.depart_admonition()

    def visit_option(self, node):
        raise NotImplementedError
        if self.context[-1]:
            self.body.append(', ')

    def depart_option(self, node):
        self.context[-1] += 1

    def visit_option_argument(self, node):
        raise NotImplementedError
        self.body.append(node.get('delimiter', ' '))
        self.body.append(self.starttag(node, 'var', ''))

    def depart_option_argument(self, node):
        self.body.append('</var>')

    def visit_option_group(self, node):
        raise NotImplementedError
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
        raise NotImplementedError
        self.body.append(
              self.starttag(node, 'table', CLASS='option-list',
                            frame="void", rules="none"))
        self.body.append('<col class="option" />\n'
                         '<col class="description" />\n'
                         '<tbody valign="top">\n')

    def depart_option_list(self, node):
        self.body.append('</tbody>\n</table>\n')

    def visit_option_list_item(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'tr', ''))

    def depart_option_list_item(self, node):
        self.body.append('</tr>\n')

    def visit_option_string(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'span', '', CLASS='option'))

    def depart_option_string(self, node):
        self.body.append('</span>')

    def visit_paragraph(self, node):
        if not self.skip_para_tag:
            self.body.append(self.para_start(self.style_map['paragraph']))

    def depart_paragraph(self, node):
        if not self.skip_para_tag:
            self.body.append(self.para_end())

    def visit_problematic(self, node):
        raise NotImplementedError
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
        raise NotImplementedError
        if node.has_key('format') and node['format'] == 'html':
            self.body.append(node.astext())
        raise nodes.SkipNode

    def visit_reference(self, node):
        # XXX
        # Keep from processing URLs
        pass

    def depart_reference(self, node):
        pass

    def visit_row(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'tr', ''))

    def depart_row(self, node):
        self.body.append('</tr>\n')

    def visit_section(self, node):
        self.section_level += 1

    def depart_section(self, node):
        self.section_level -= 1

    def visit_strong(self, node):
        self.body.append(self.charstyle_start(self.style_map['strong']))

    def depart_strong(self, node):
        self.body.append(self.charstyle_end())

    def visit_table(self, node):
        raise NotImplementedError
        self.body.append(
              self.starttag(node, 'table', CLASS="table",
                            frame='border', rules='all'))

    def depart_table(self, node):
        self.body.append('</table>\n')

    def visit_target(self, node):
        raise NotImplementedError
        if not (node.has_key('refuri') or node.has_key('refid')
                or node.has_key('refname')):
            self.body.append(self.starttag(node, 'a', '', CLASS='target'))
            self.context.append('</a>')
        else:
            self.context.append('')

    def depart_target(self, node):
        self.body.append(self.context.pop())

    def visit_tbody(self, node):
        raise NotImplementedError
        self.write_colspecs()
        self.body.append(self.context.pop()) # '</colgroup>\n' or ''
        self.body.append(self.starttag(node, 'tbody', valign='top'))

    def depart_tbody(self, node):
        self.body.append('</tbody>\n')

    def visit_term(self, node):
        raise NotImplementedError
        self.body.append(self.starttag(node, 'dt', ''))

    def depart_term(self, node):
        """
        Leave the end tag to `self.visit_definition()`, in case there's a
        classifier.
        """
        pass

    def visit_tgroup(self, node):
        raise NotImplementedError
        # Mozilla needs <colgroup>:
        self.body.append(self.starttag(node, 'colgroup'))
        # Appended by thead or tbody:
        self.context.append('</colgroup>\n')

    def depart_tgroup(self, node):
        pass

    def visit_thead(self, node):
        raise NotImplementedError
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
        style = self.style_map['sections'][self.section_level]
        title_tag = self.para_start(style)
        self.body.append(title_tag)

    def depart_title(self, node):
        self.body.append(self.para_end())

    def visit_warning(self, node):
        self.visit_admonition(node, 'warning')

    def depart_warning(self, node):
        self.depart_admonition()

    def visit_system_message(self, node):
        print node.astext()
        raise nodes.SkipNode

    def depart_system_message(self, node):
        pass

    def unknown_visit(self, node):
        print "Failure processing at line", node.line
        print "Failure is", node.astext()
        raise NotImplementedError('visiting unimplemented node type: %s'
                                  % node.__class__.__name__)
