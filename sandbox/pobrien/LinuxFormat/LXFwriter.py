"""Linux Format writer."""

__author__ = "Patrick K. O'Brien <pobrien@orbtech.com>"
__cvsid__ = "$Id$"
__revision__ = "$Revision$"[11:-2]


__docformat__ = 'reStructuredText'


import sys
import os
import time
import re
from types import ListType

import docutils
from docutils import nodes, utils, writers, languages


class Writer(writers.Writer):

    supported = ('lxf', 'LinuxFormat')
    """Formats this writer supports."""

    output = None
    """Final translated form of `document`."""

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = Translator

    def translate(self):
        visitor = self.translator_class(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()


class Translator(nodes.NodeVisitor):
    """Produces Linux Format Magazine output."""

    words_and_spaces = re.compile(r'\S+| +|\n')

    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.settings = settings = document.settings
        lcode = settings.language_code
        self.language = languages.get_language(lcode)
        self.head = []
        self.body = []
        self.foot = []
        self.part = self.body
        self.section_level = 0
        self.context = []
        self.topic_class = ''
        self.colspecs = []
        self.compact_p = 1
        self.compact_simple = None
        self.in_bullet_list = False
        self.in_docinfo = False
        self.in_sidebar = False
        self.sidebar_start = False
        self.filterNewlines = True

    def astext(self):
        """Return the final formatted document as a string."""
        return ''.join(self.head + ['\n///BODY COPY START///'] +
                       self.body + ['///END BODY COPY///\n'] + self.foot)

    def encode(self, text):
        if self.filterNewlines:
            text = text.replace('\n', ' ')
        return text

    def visit_Text(self, node):
        self.part.append(self.encode(node.astext()))

    def depart_Text(self, node):
        pass

    def visit_address(self, node):
        raise NotImplementedError, node.astext()
        self.visit_docinfo_item(node, 'address', meta=None)

    def depart_address(self, node):
        self.depart_docinfo_item()

    def visit_admonition(self, node, name):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'div', CLASS=name))
        self.part.append('<p class="admonition-title">'
                         + self.language.labels[name] + '</p>\n')

    def depart_admonition(self):
        raise NotImplementedError, node.astext()
        self.part.append('</div>\n')

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

    def visit_author(self, node):
        raise NotImplementedError, node.astext()
        self.visit_docinfo_item(node, 'author')

    def depart_author(self, node):
        self.depart_docinfo_item()

    def visit_authors(self, node):
        pass

    def depart_authors(self, node):
        pass

    def visit_block_quote(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'blockquote'))

    def depart_block_quote(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</blockquote>\n')

    def check_simple_list(self, node):
        raise NotImplementedError, node.astext()
        """Check for a simple list that can be rendered compactly."""
        visitor = SimpleListChecker(self.document)
        try:
            node.walk(visitor)
        except nodes.NodeFound:
            return None
        else:
            return 1

    def visit_bullet_list(self, node):
        self.in_bullet_list = True

    def depart_bullet_list(self, node):
        self.in_bullet_list = False

    def visit_caption(self, node):
        self.part.append('\n///CAPTION///')
        self.visit_paragraph(node)

    def depart_caption(self, node):
        self.depart_paragraph(node)

    def visit_caution(self, node):
        self.visit_admonition(node, 'caution')

    def depart_caution(self, node):
        self.depart_admonition()

    def visit_citation(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'table', CLASS='citation',
                                       frame="void", rules="none"))
        self.part.append('<colgroup><col class="label" /><col /></colgroup>\n'
                         '<col />\n'
                         '<tbody valign="top">\n'
                         '<tr>')
        self.footnote_backrefs(node)

    def depart_citation(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</td></tr>\n'
                         '</tbody>\n</table>\n')

    def visit_citation_reference(self, node):
        raise NotImplementedError, node.astext()
        href = ''
        if node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        self.part.append(self.starttag(node, 'a', '[', href=href,
                                       CLASS='citation-reference'))

    def depart_citation_reference(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(']</a>')

    def visit_classifier(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(' <span class="classifier-delimiter">:</span> ')
        self.part.append(self.starttag(node, 'span', '', CLASS='classifier'))

    def depart_classifier(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</span>')

    def visit_colspec(self, node):
        self.colspecs.append(node)

    def depart_colspec(self, node):
        pass

    def write_colspecs(self):
        raise NotImplementedError, node.astext()
        width = 0
        for node in self.colspecs:
            width += node['colwidth']
        for node in self.colspecs:
            colwidth = int(node['colwidth'] * 100.0 / width + 0.5)
            self.part.append(self.emptytag(node, 'col',
                                           width='%i%%' % colwidth))
        self.colspecs = []

    def visit_comment(self, node):
        raise nodes.SkipNode

    def visit_contact(self, node):
        raise NotImplementedError, node.astext()
        self.visit_docinfo_item(node, 'contact', meta=None)

    def depart_contact(self, node):
        self.depart_docinfo_item()

    def visit_copyright(self, node):
        raise NotImplementedError, node.astext()
        self.visit_docinfo_item(node, 'copyright')

    def depart_copyright(self, node):
        self.depart_docinfo_item()

    def visit_danger(self, node):
        self.visit_admonition(node, 'danger')

    def depart_danger(self, node):
        self.depart_admonition()

    def visit_date(self, node):
        raise NotImplementedError, node.astext()
        self.visit_docinfo_item(node, 'date')

    def depart_date(self, node):
        self.depart_docinfo_item()

    def visit_decoration(self, node):
        pass

    def depart_decoration(self, node):
        pass

    def visit_definition(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</dt>\n')
        self.part.append(self.starttag(node, 'dd', ''))
        if len(node):
            node[0].set_class('first')
            node[-1].set_class('last')

    def depart_definition(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</dd>\n')

    def visit_definition_list(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'dl'))

    def depart_definition_list(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</dl>\n')

    def visit_definition_list_item(self, node):
        pass

    def depart_definition_list_item(self, node):
        pass

    def visit_description(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'td', ''))
        if len(node):
            node[0].set_class('first')
            node[-1].set_class('last')

    def depart_description(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</td>')

    def visit_docinfo(self, node):
        self.in_docinfo = True

    def depart_docinfo(self, node):
        self.in_docinfo = False

    def visit_docinfo_item(self, node, name, meta=1):
        raise NotImplementedError, node.astext()
        if meta:
            self.head.append('<meta name="%s" content="%s" />\n'
                             % (name, self.attval(node.astext())))
        self.part.append(self.starttag(node, 'tr', ''))
        self.part.append('<th class="docinfo-name">%s:</th>\n<td>'
                         % self.language.labels[name])
        if len(node):
            if isinstance(node[0], nodes.Element):
                node[0].set_class('first')
            if isinstance(node[0], nodes.Element):
                node[-1].set_class('last')

    def depart_docinfo_item(self):
        raise NotImplementedError, node.astext()
        self.part.append('</td></tr>\n')

    def visit_doctest_block(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'pre', CLASS='doctest-block'))

    def depart_doctest_block(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('\n</pre>\n')

    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass

    def visit_emphasis(self, node):
        self.part.append('/')

    def depart_emphasis(self, node):
        self.part.append('/')

    def visit_entry(self, node):
        raise NotImplementedError, node.astext()
        if isinstance(node.parent.parent, nodes.thead):
            tagname = 'th'
        else:
            tagname = 'td'
        atts = {}
        if node.has_key('morerows'):
            atts['rowspan'] = node['morerows'] + 1
        if node.has_key('morecols'):
            atts['colspan'] = node['morecols'] + 1
        self.part.append(self.starttag(node, tagname, '', **atts))
        self.context.append('</%s>\n' % tagname.lower())
        if len(node) == 0:              # empty cell
            self.part.append('&nbsp;')
        else:
            node[0].set_class('first')
            node[-1].set_class('last')

    def depart_entry(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.context.pop())

    def visit_enumerated_list(self, node):
        raise NotImplementedError, node.astext()
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
        self.compact_simple = (self.settings.compact_lists and
                               (self.compact_simple
                                or self.topic_class == 'contents'
                                or self.check_simple_list(node)))
        if self.compact_simple and not old_compact_simple:
            atts['class'] = (atts.get('class', '') + ' simple').strip()
        self.part.append(self.starttag(node, 'ol', **atts))

    def depart_enumerated_list(self, node):
        raise NotImplementedError, node.astext()
        self.compact_simple, self.compact_p = self.context.pop()
        self.part.append('</ol>\n')

    def visit_error(self, node):
        self.visit_admonition(node, 'error')

    def depart_error(self, node):
        self.depart_admonition()

    def visit_field(self, node):
        if self.in_docinfo:
            self.part = self.foot

    def depart_field(self, node):
        self.part = self.body

    def visit_field_body(self, node):
        pass

    def depart_field_body(self, node):
        pass

    def visit_field_list(self, node):
        raise NotImplementedError, node.astext()

    def depart_field_list(self, node):
        raise NotImplementedError, node.astext()

    def visit_field_name(self, node):
        if self.in_docinfo and node.astext() == 'Next':
            self.part.append('\n\n///NEXT MONTH///')
            raise nodes.SkipNode
        else:
            raise NotImplementedError, node.astext()

    def depart_field_name(self, node):
        pass

    def visit_figure(self, node):
        self.part = self.foot
        self.part.append('\n\n///PIC///\n')

    def depart_figure(self, node):
        self.part = self.body

    def visit_footer(self, node):
        raise NotImplementedError, node.astext()
        self.context.append(len(self.part))

    def depart_footer(self, node):
        raise NotImplementedError, node.astext()
        start = self.context.pop()
        footer = (['<hr class="footer"/>\n',
                   self.starttag(node, 'div', CLASS='footer')]
                  + self.part[start:] + ['</div>\n'])
        self.body_suffix[:0] = footer
        del self.part[start:]

    def visit_footnote(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'table', CLASS='footnote',
                                       frame="void", rules="none"))
        self.part.append('<colgroup><col class="label" /><col /></colgroup>\n'
                         '<tbody valign="top">\n'
                         '<tr>')
        self.footnote_backrefs(node)

    def footnote_backrefs(self, node):
        raise NotImplementedError, node.astext()
        if self.settings.footnote_backlinks and node.hasattr('backrefs'):
            backrefs = node['backrefs']
            if len(backrefs) == 1:
                self.context.append('')
                self.context.append('<a class="fn-backref" href="#%s" '
                                    'name="%s">' % (backrefs[0], node['id']))
            else:
                i = 1
                backlinks = []
                for backref in backrefs:
                    backlinks.append('<a class="fn-backref" href="#%s">%s</a>'
                                     % (backref, i))
                    i += 1
                self.context.append('<em>(%s)</em> ' % ', '.join(backlinks))
                self.context.append('<a name="%s">' % node['id'])
        else:
            self.context.append('')
            self.context.append('<a name="%s">' % node['id'])

    def depart_footnote(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</td></tr>\n'
                         '</tbody>\n</table>\n')

    def visit_footnote_reference(self, node):
        raise NotImplementedError, node.astext()
        href = ''
        if node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        format = self.settings.footnote_references
        if format == 'brackets':
            suffix = '['
            self.context.append(']')
        elif format == 'superscript':
            suffix = '<sup>'
            self.context.append('</sup>')
        else:                           # shouldn't happen
            suffix = '???'
            self.content.append('???')
        self.part.append(self.starttag(node, 'a', suffix, href=href,
                                       CLASS='footnote-reference'))

    def depart_footnote_reference(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.context.pop() + '</a>')

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

    def visit_header(self, node):
        raise NotImplementedError, node.astext()
        self.context.append(len(self.part))

    def depart_header(self, node):
        raise NotImplementedError, node.astext()
        start = self.context.pop()
        self.body_prefix.append(self.starttag(node, 'div', CLASS='header'))
        self.body_prefix.extend(self.part[start:])
        self.body_prefix.append('<hr />\n</div>\n')
        del self.part[start:]

    def visit_hint(self, node):
        self.visit_admonition(node, 'hint')

    def depart_hint(self, node):
        self.depart_admonition()

    def visit_image(self, node):
        name = str(node.attributes['uri'])
        self.part.append(name)
        pass

    def depart_image(self, node):
        pass

    def visit_important(self, node):
        self.visit_admonition(node, 'important')

    def depart_important(self, node):
        self.depart_admonition()

    def visit_label(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'td', '%s[' % self.context.pop(),
                                       CLASS='label'))

    def depart_label(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(']</a></td><td>%s' % self.context.pop())

    def visit_legend(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'div', CLASS='legend'))

    def depart_legend(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</div>\n')

    def visit_line_block(self, node):
        raise NotImplementedError, node.astext()

    def depart_line_block(self, node):
        raise NotImplementedError, node.astext()

    def visit_list_item(self, node):
        if self.in_bullet_list:
            self.part.append('\n* ')

    def depart_list_item(self, node):
        pass

    def visit_literal(self, node):
        self.part.append('///code///')

    def depart_literal(self, node):
        self.part.append('///code ends///')

    def visit_literal_block(self, node):
        self.part.append('\n///CODE///\n')
        self.filterNewlines = False

    def depart_literal_block(self, node):
        self.part.append('\n///END CODE///\n')
        self.filterNewlines = True

    def visit_meta(self, node):
        raise NotImplementedError, node.astext()
        self.head.append(self.emptytag(node, 'meta', **node.attributes))

    def depart_meta(self, node):
        pass

    def visit_note(self, node):
        self.visit_admonition(node, 'note')

    def depart_note(self, node):
        self.depart_admonition()

    def visit_option(self, node):
        raise NotImplementedError, node.astext()
        if self.context[-1]:
            self.part.append(', ')

    def depart_option(self, node):
        raise NotImplementedError, node.astext()
        self.context[-1] += 1

    def visit_option_argument(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(node.get('delimiter', ' '))
        self.part.append(self.starttag(node, 'var', ''))

    def depart_option_argument(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</var>')

    def visit_option_group(self, node):
        raise NotImplementedError, node.astext()
        atts = {}
        if len(node.astext()) > 14:
            atts['colspan'] = 2
            self.context.append('</tr>\n<tr><td>&nbsp;</td>')
        else:
            self.context.append('')
        self.part.append(self.starttag(node, 'td', **atts))
        self.part.append('<kbd>')
        self.context.append(0)          # count number of options

    def depart_option_group(self, node):
        raise NotImplementedError, node.astext()
        self.context.pop()
        self.part.append('</kbd></td>\n')
        self.part.append(self.context.pop())

    def visit_option_list(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(
              self.starttag(node, 'table', CLASS='option-list',
                            frame="void", rules="none"))
        self.part.append('<col class="option" />\n'
                         '<col class="description" />\n'
                         '<tbody valign="top">\n')

    def depart_option_list(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</tbody>\n</table>\n')

    def visit_option_list_item(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'tr', ''))

    def depart_option_list_item(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</tr>\n')

    def visit_option_string(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'span', '', CLASS='option'))

    def depart_option_string(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</span>')

    def visit_organization(self, node):
        raise NotImplementedError, node.astext()
        self.visit_docinfo_item(node, 'organization')

    def depart_organization(self, node):
        raise NotImplementedError, node.astext()
        self.depart_docinfo_item()

    def visit_paragraph(self, node):
        if self.in_sidebar and self.sidebar_start:
            self.part.append('///BOX BODY///')
            self.sidebar_start = False
        if not  self.in_bullet_list:
            self.part.append('\n')

    def depart_paragraph(self, node):
        self.part.append('\n')

    def visit_problematic(self, node):
        raise NotImplementedError, node.astext()
        if node.hasattr('refid'):
            self.part.append('<a href="#%s" name="%s">' % (node['refid'],
                                                           node['id']))
            self.context.append('</a>')
        else:
            self.context.append('')
        self.part.append(self.starttag(node, 'span', '', CLASS='problematic'))

    def depart_problematic(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</span>')
        self.part.append(self.context.pop())

    def visit_raw(self, node):
        raise NotImplementedError, node.astext()
        if node.get('format') == 'html':
            self.part.append(node.astext())
        # Keep non-HTML raw text out of output:
        raise nodes.SkipNode

    def visit_reference(self, node):
        pass

    def depart_reference(self, node):
        pass

    def visit_revision(self, node):
        raise NotImplementedError, node.astext()
        self.visit_docinfo_item(node, 'revision', meta=None)

    def depart_revision(self, node):
        self.depart_docinfo_item()

    def visit_row(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'tr', ''))

    def depart_row(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</tr>\n')

    def visit_section(self, node):
        self.section_level += 1

    def depart_section(self, node):
        self.section_level -= 1

    def visit_sidebar(self, node):
        self.part = self.foot
        self.in_sidebar = True
        self.sidebar_start = True
        self.part.append('\n\n///BOXOUT///\n')

    def depart_sidebar(self, node):
        self.part.append('///END BOX BODY///\n')
        self.in_sidebar = False
        self.part = self.body

    def visit_status(self, node):
        raise NotImplementedError, node.astext()
        self.visit_docinfo_item(node, 'status', meta=None)

    def depart_status(self, node):
        self.depart_docinfo_item()

    def visit_strong(self, node):
        self.part.append('*')

    def depart_strong(self, node):
        self.part.append('*')

    def visit_substitution_definition(self, node):
        """Internal only."""
        raise nodes.SkipNode

    def visit_substitution_reference(self, node):
        self.unimplemented_visit(node)

    def visit_subtitle(self, node):
        if self.in_sidebar:
            self.part.append('///BOXOUT SUBHEAD///\n')
        else:
            self.part = self.head
            self.part.append('\n///DESCRIPTION///\n')

    def depart_subtitle(self, node):
        self.part.append('\n')
        if self.in_sidebar:
            pass
        else:
            self.part = self.body

    def visit_system_message(self, node):
        raise NotImplementedError, node.astext()
        if node['level'] < self.document.reporter['writer'].report_level:
            # Level is too low to display:
            raise nodes.SkipNode
        self.part.append(self.starttag(node, 'div', CLASS='system-message'))
        self.part.append('<p class="system-message-title">')
        attr = {}
        backref_text = ''
        if node.hasattr('id'):
            attr['name'] = node['id']
        if node.hasattr('backrefs'):
            backrefs = node['backrefs']
            if len(backrefs) == 1:
                backref_text = ('; <em><a href="#%s">backlink</a></em>'
                                % backrefs[0])
            else:
                i = 1
                backlinks = []
                for backref in backrefs:
                    backlinks.append('<a href="#%s">%s</a>' % (backref, i))
                    i += 1
                backref_text = ('; <em>backlinks: %s</em>'
                                % ', '.join(backlinks))
        if node.hasattr('line'):
            line = ', line %s' % node['line']
        else:
            line = ''
        if attr:
            a_start = self.starttag({}, 'a', '', **attr)
            a_end = '</a>'
        else:
            a_start = a_end = ''
        self.part.append('System Message: %s%s/%s%s (<tt>%s</tt>%s)%s</p>\n'
                         % (a_start, node['type'], node['level'], a_end,
                            node['source'], line, backref_text))

    def depart_system_message(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</div>\n')

    def visit_table(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(
              self.starttag(node, 'table', CLASS="table",
                            frame='border', rules='all'))

    def depart_table(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</table>\n')

    def visit_target(self, node):
        raise NotImplementedError, node.astext()
        if not (node.has_key('refuri') or node.has_key('refid')
                or node.has_key('refname')):
            self.part.append(self.starttag(node, 'a', '', CLASS='target'))
            self.context.append('</a>')
        else:
            self.context.append('')

    def depart_target(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.context.pop())

    def visit_tbody(self, node):
        raise NotImplementedError, node.astext()
        self.write_colspecs()
        self.part.append(self.context.pop()) # '</colgroup>\n' or ''
        self.part.append(self.starttag(node, 'tbody', valign='top'))

    def depart_tbody(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</tbody>\n')

    def visit_term(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'dt', ''))

    def depart_term(self, node):
        """
        Leave the end tag to `self.visit_definition()`, in case there's a
        classifier.
        """
        raise NotImplementedError, node.astext()
        pass

    def visit_tgroup(self, node):
        raise NotImplementedError, node.astext()
        # Mozilla needs <colgroup>:
        self.part.append(self.starttag(node, 'colgroup'))
        # Appended by thead or tbody:
        self.context.append('</colgroup>\n')

    def depart_tgroup(self, node):
        pass

    def visit_thead(self, node):
        raise NotImplementedError, node.astext()
        self.write_colspecs()
        self.part.append(self.context.pop()) # '</colgroup>\n'
        # There may or may not be a <thead>; this is for <tbody> to use:
        self.context.append('')
        self.part.append(self.starttag(node, 'thead', valign='bottom'))

    def depart_thead(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</thead>\n')

    def visit_tip(self, node):
        self.visit_admonition(node, 'tip')

    def depart_tip(self, node):
        self.depart_admonition()

    def visit_title(self, node):
        """Only 2 section levels are supported."""
        if isinstance(node.parent, nodes.topic):
            if self.topic_class == 'abstract':
                self.part = self.head
                self.part.append('\n///STRAP///')
                raise nodes.SkipNode
        elif self.in_sidebar:
            # sidebar title
            self.part.append('///BOXOUT HEAD///\n')
        elif self.section_level == 0:
            # document title
            self.part = self.head
            self.part.append('///TITLE///\n')
        else:
            self.part.append('\n///CROSSHEAD///\n')

    def depart_title(self, node):
        self.part.append('\n')
        if not self.in_sidebar:
            self.part = self.body

    def visit_title_reference(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.starttag(node, 'cite', ''))

    def depart_title_reference(self, node):
        raise NotImplementedError, node.astext()
        self.part.append('</cite>')

    def visit_topic(self, node):
        self.topic_class = node.get('class')

    def depart_topic(self, node):
        self.topic_class = ''
        self.part = self.body

    def visit_transition(self, node):
        raise NotImplementedError, node.astext()
        self.part.append(self.emptytag(node, 'hr'))

    def depart_transition(self, node):
        pass

    def visit_version(self, node):
        raise NotImplementedError, node.astext()
        self.visit_docinfo_item(node, 'version', meta=None)

    def depart_version(self, node):
        self.depart_docinfo_item()

    def visit_warning(self, node):
        self.visit_admonition(node, 'warning')

    def depart_warning(self, node):
        self.depart_admonition()

    def unimplemented_visit(self, node):
        raise NotImplementedError('visiting unimplemented node type: %s'
                                  % node.__class__.__name__)


class SimpleListChecker(nodes.GenericNodeVisitor):

    """
    Raise `nodes.SkipNode` if non-simple list item is encountered.

    Here "simple" means a list item containing nothing other than a single
    paragraph, a simple list, or a paragraph followed by a simple list.
    """

    def default_visit(self, node):
        raise nodes.NodeFound

    def visit_bullet_list(self, node):
        pass

    def visit_enumerated_list(self, node):
        pass

    def visit_list_item(self, node):
        children = []
        for child in node.get_children():
            if not isinstance(child, nodes.Invisible):
                children.append(child)
        if (children and isinstance(children[0], nodes.paragraph)
            and (isinstance(children[-1], nodes.bullet_list)
                 or isinstance(children[-1], nodes.enumerated_list))):
            children.pop()
        if len(children) <= 1:
            return
        else:
            raise nodes.NodeFound

    def visit_paragraph(self, node):
        raise nodes.SkipNode

    def invisible_visit(self, node):
        """Invisible nodes should be ignored."""
        pass

    visit_comment = invisible_visit
    visit_substitution_definition = invisible_visit
    visit_target = invisible_visit
    visit_pending = invisible_visit
