#! /usr/bin/env python

"""
:Author: Engelbert Gruber
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Simple pdf writer.

The output uses reportlabs module.

Some stylesheet is needed.
"""

__docformat__ = 'reStructuredText'


import time
from types import ListType, TupleType, UnicodeType
from docutils import writers, nodes, languages

from stylesheet import getStyleSheet
from rltemplate import RLDocTemplate

from reportlab.lib.styles import ParagraphStyle
from reportlab.lib.enums import *
from reportlab.lib.pagesizes import A4
from reportlab.platypus import *
from reportlab.platypus.para import Paragraph
from reportlab.lib import colors
from reportlab.lib.units import inch
from StringIO import StringIO

class Writer(writers.Writer):

    supported = ('pdf','rlpdf')
    """Formats this writer supports."""

    settings_spec = (
        'PDF-Specific Options',
        None,
        (('Format for footnote references: one of "superscript" or '
          '"brackets".  Default is "brackets".',
          ['--footnote-references'],
          {'choices': ['superscript', 'brackets'], 'default': 'brackets',
           'metavar': '<FORMAT>'}),))

    output = None
    """Final translated form of `document`."""

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = PDFTranslator

    def translate(self):
        visitor = self.translator_class(self.document)
        self.document.walkabout(visitor)
        self.story = visitor.as_what()
        self.output = self.record()

    def record(self):
        from reportlab.platypus import SimpleDocTemplate
        out = StringIO()
        doc = SimpleDocTemplate(out, pagesize=A4)
        doc.build(self.story)
        return out.getvalue()

    def lower(self):
        return 'pdf'

class PDFTranslator(nodes.NodeVisitor):

    def __init__(self, doctree):
        self.settings = settings = doctree.settings
        self.styleSheet = getStyleSheet()
        nodes.NodeVisitor.__init__(self, doctree)
        self.language = languages.get_language(doctree.settings.language_code)
        self.in_docinfo = None
        self.head = []
        self.body = []
        self.foot = []
        self.sectionlevel = 0
        self.context = []
        self.topic_class = ''
        self.story = []
        self.bulletText = '\267'	# maybe move this into stylesheet.
        self.bulletlevel = 0

    def as_what(self):
        return self.story

    def encode(self, text):
        """Encode special characters in `text` & return."""
        if type(text) is UnicodeType:
            text = text.replace(u'\u2020', u' ')
            text = text.replace(u'\xa0', u' ')
            text = text.encode('utf-8')
        #text = text.replace("&", "&amp;")
        #text = text.replace("<", '"')
        #text = text.replace('"', "(quot)")
        #text = text.replace(">", '"')
        # footnotes have character values above 128 ?
        return text

    def append_para(self, text, in_style='Normal', bulletText=None):
        if type(text) in (ListType, TupleType):
            text = ''.join([self.encode(t) for t in text])
        style = self.styleSheet[in_style]
        self.story.append(Paragraph(self.encode(text), style, \
                                    bulletText=bulletText, context=self.styleSheet))

    def starttag(self, node, tagname, suffix='\n', **attributes):
        atts = {}
        for (name, value) in attributes.items():
            atts[name.lower()] = value
        for att in ('class',):          # append to node attribute
            if node.has_key(att):
                if atts.has_key(att):
                    atts[att] = node[att] + ' ' + atts[att]
        for att in ('id',):             # node attribute overrides
            if node.has_key(att):
                atts[att] = node[att]
        attlist = atts.items()
        attlist.sort()
        parts = [tagname]
        for name, value in attlist:
            if value is None:           # boolean attribute
                parts.append(name.lower())
            elif isinstance(value, ListType):
                values = [str(v) for v in value]
                parts.append('%s="%s"' % (name.lower(),
                                          self.encode(' '.join(values))))
            else:
                parts.append('%s="%s"' % (name.lower(),
                                          self.encode(str(value))))
        return '<%s>%s' % (' '.join(parts), suffix)

    def visit_Text(self, node):
        self.context.append('#text')
        self.body.append(node.astext())

    def depart_Text(self, node):
        self.context.pop()

    def visit_admonition(self, node, name):
        pass

    def depart_admonition(self):
        pass

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

    def visit_author(self, node):
        self.visit_docinfo_item(node, 'author')

    def depart_author(self, node):
        self.depart_docinfo_item()

    def visit_address(self, node):
        self.visit_docinfo_item(node, 'address')

    def depart_address(self, node):
        self.depart_docinfo_item()

    def visit_version(self, node):
        self.visit_docinfo_item(node, 'version')

    def depart_version(self, node):
        self.depart_docinfo_item()

    def visit_system_message(self, node):
        pass

    def depart_system_message(self, node):
        pass

    def visit_term(self, node):
        self.context.append('dt')
        self.body.append(self.starttag(node, 'dt', ''))

    def depart_term(self, node):
        # Closes on visit_definition
        self.context.pop()

    def visit_authors(self, node):
        pass

    def depart_authors(self, node):
        pass

    def visit_block_quote(self, node):
        pass

    def depart_block_quote(self, node):
        pass

    def visit_bullet_list(self, node):
        self.context.append(len(self.body))
        self.context.append('ul')
        self.body.append('<ul bulletText="%s">' % self.bulletText)

    def depart_bullet_list(self, node):
        self.context.pop()
        self.body.append('</ul>')
        start = self.context.pop()
        if not 'ul' in self.context:
            self.append_para(self.body[start:])
            self.body = self.body[:start]

    def visit_caption(self, node):
        pass

    def depart_caption(self, node):
        pass

    def visit_caution(self, node):
        self.visit_admonition(node, 'caution')

    def depart_caution(self, node):
        self.depart_admonition()

    def visit_citation(self, node):
        pass

    def depart_citation(self, node):
        pass

    def visit_citation_reference(self, node):
        pass

    def depart_citation_reference(self, node):
        pass

    def visit_classifier(self, node):
        pass

    def depart_classifier(self, node):
        pass

    def visit_colspec(self, node):
        pass

    def depart_colspec(self, node):
        pass

    def visit_contact(self, node):
        self.visit_docinfo_item(node, 'contact')

    def depart_contact(self, node):
        self.depart_docinfo_item()

    def visit_copyright(self, node):
        self.visit_docinfo_item(node, 'copyright')

    def depart_copyright(self, node):
        self.depart_docinfo_item()

    def visit_danger(self, node):
        self.visit_admonition(node, 'danger')

    def depart_danger(self, node):
        self.depart_admonition()

    def visit_date(self, node):
        self.visit_docinfo_item(node, 'date')

    def depart_date(self, node):
        self.depart_docinfo_item()

    def visit_definition(self, node):
        self.body.append('</dt>')
        self.context.append('dd')
        self.body.append(self.starttag(node, 'dd'))

    def depart_definition(self, node):
        self.context.pop()
        self.body.append('</dd>')

    def visit_definition_list(self, node):
        self.context.append(len(self.body))
        self.context.append('dl')
        self.body.append(self.starttag(node, 'dl'))

    def depart_definition_list(self, node):
        self.context.pop()
        self.body.append('</dl>')
        start = self.context.pop()
        if not 'dl' in self.context:
            self.append_para(self.body[start:])
            self.body = self.body[:start]

    def visit_definition_list_item(self, node):
        pass

    def depart_definition_list_item(self, node):
        pass

    def visit_description(self, node):
        pass

    def depart_description(self, node):
        pass

    def visit_docinfo(self, node):
        self.context.append(len(self.body))
        self.in_docinfo = 1

    def depart_docinfo(self, node):
        start = self.context.pop()
        docinfo = self.body[start:]
        self.body = self.body[:start]
        self.append_para(docinfo)
        self.in_docinfo = None

    def visit_docinfo_item(self, node, name):
        self.body.append('<para style="DocInfo"><b>%s: </b>' % self.language.labels[name])

    def depart_docinfo_item(self):
        self.body.append('</para>')

    def visit_doctest_block(self, node):
        self.visit_literal_block(node)

    def depart_doctest_block(self, node):
        self.depart_literal_block(node)

    def visit_line_block(self, node):
        self.visit_literal_block(node)

    def depart_line_block(self, node):
        self.depart_literal_block(node)

    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass

    def visit_emphasis(self, node):
        self.context.append('i')
        self.body.append('<i>')

    def depart_emphasis(self, node):
        self.context.pop()
        self.body.append('</i>')

    def visit_entry(self, node):
        pass

    def depart_entry(self, node):
        pass

    def visit_enumerated_list(self, node):
        self.context.append(len(self.body))
        self.context.append('ol')
        self.body.append('<ol>')

    def depart_enumerated_list(self, node):
        self.context.pop()
        self.body.append('</ol>')
        start = self.context.pop()
        if not 'ol' in self.context:
            self.append_para(self.body[start:])
            self.body = self.body[:start]

    def visit_error(self, node):
        self.visit_admonition(node, 'error')

    def depart_error(self, node):
        self.depart_admonition()

    def visit_field(self, node):
        self.body.append('<para>')

    def depart_field(self, node):
        self.body.append('</para>')

    def visit_field_argument(self, node):
        pass

    def depart_field_argument(self, node):
        pass

    def visit_field_list(self, node):
        self.context.append(len(self.body))
        self.body.append('<para>')

    def depart_field_list(self, node):
        start = self.context.pop()
        self.body.append('</para>')
        self.append_para(self.body[start:])
        self.body = self.body[:start]

    def visit_field_name(self, node):
        self.body.append('<b>')

    def depart_field_name(self, node):
        self.body.append(': </b>')

    def visit_field_body(self, node):
        pass

    def depart_field_body(self, node):
        pass

    def visit_footnote(self, node):
        self.context.append('footnotes')
        self.footnote_backrefs(node)

    def footnote_backrefs(self, node):
        if self.settings.footnote_backlinks and node.hasattr('backrefs'):
            backrefs = node['backrefs']
            if len(backrefs) == 1:
                self.context.append("%s%s" % (self.starttag({}, 'setLink', '', destination=node['id']), \
                                    '</setLink>'))
                self.context.append("%s%s" % (self.starttag({}, 'link', '', destination=backrefs[0]), \
                                    '</link>'))
            else:
                i = 1
                backlinks = []
                for backref in backrefs:
                    backlinks.append("%s%s%s" % (self.starttag({}, 'link', '', destination=backref),
                                                 i,
                                                 '</link>'))
                    i += 1
                self.context.append(' <i>(%s)</i> ' % ', '.join(backlinks))
                self.context.append("%s%s" % (self.starttag({}, 'setLink', '', destination=node['id']), \
                                 '</setLink>'))
        else:
            self.context.append("%s%s" % (self.starttag({}, 'setLink', '', destination=node['id']), \
                                '</setLink>'))
            self.context.append('')

    def footnote_backrefs_depart(self, node):
        if not self.context and self.body:
            self.append_para(self.body)
            self.body = []

    def depart_footnote(self, node):
        self.context.pop()
        self.footnote_backrefs_depart(node)

    def visit_footnote_reference(self, node):
        # for backrefs
        if self.settings.footnote_backlinks and node.has_key('id'):
            self.body.append(self.starttag(node, 'setLink', '', destination=node['id']))
            self.context.append('</setLink>')
        else:
            self.context.append('')

        href = ''
        if node.has_key('refid'):
            href = node['refid']
        elif node.has_key('refname'):
            href = self.document.nameids[node['refname']]
        format = self.settings.footnote_references
        if format == 'brackets':
            suffix = '['
            self.context.append(']')
        elif format == 'superscript':
            suffix = '<super>'
            self.context.append('</super>')
        else:                           # shouldn't happen
            suffix = '???'
            self.content.append('???')
        self.body.append(self.starttag(node, 'link', suffix, destination=href))

    def depart_footnote_reference(self, node):
        self.body.append(self.context.pop())
        self.body.append('</link>')
        self.body.append(self.context.pop())

    def visit_hint(self, node):
        self.visit_admonition(node, 'hint')

    def depart_hint(self, node):
        self.depart_admonition()

    def visit_image(self, node):
        pass

    def depart_image(self, node):
        pass

    def visit_important(self, node):
        self.visit_admonition(node, 'important')

    def depart_important(self, node):
        self.depart_admonition()

    def visit_interpreted(self, node):
        pass

    def depart_interpreted(self, node):
        pass

    def visit_label(self, node):
        if 'footnotes' in self.context:
            self.body.append('[')

    def depart_label(self, node):
        if 'footnotes' in self.context:
            self.body.append(']')
            self.body.append(self.context.pop())
            self.body.append(self.context.pop())
        self.body.append('   ')

    def visit_legend(self, node):
        pass

    def depart_legend(self, node):
        pass

    def visit_list_item(self, node):
        self.context.append('li')
        self.body.append('<li>')

    def depart_list_item(self, node):
        self.context.pop()
        self.body.append('</li>')

    def visit_literal(self, node):
        self.context.append('literal')

    def depart_literal(self, node):
        self.context.pop()

    def visit_literal_block(self, node):
        self.story.append(Preformatted(node.astext(), self.styleSheet['Code']))
        raise nodes.SkipNode

    def depart_literal_block(self, node):
        pass

    def visit_meta(self, node):
        self.head.append(self.starttag(node, 'meta', **node.attributes))

    def depart_meta(self, node):
        pass

    def visit_note(self, node):
        self.visit_admonition(node, 'note')

    def depart_note(self, node):
        self.depart_admonition()

    def visit_option(self, node):
        pass

    def depart_option(self, node):
        pass

    def visit_option_argument(self, node):
        pass

    def depart_option_argument(self, node):
        pass

    def visit_option_group(self, node):
        pass

    def depart_option_group(self, node):
        pass

    def visit_option_list(self, node):
        self.context.append(len(self.body))
        self.context.append('option_list')

    def depart_option_list(self, node):
        self.context.pop()
        start = self.context.pop()
        if not 'option_list' in self.context:
            self.append_para(self.body[start:])
            self.body = self.body[:start]

    def visit_option_list_item(self, node):
        pass

    def depart_option_list_item(self, node):
        pass

    def visit_option_string(self, node):
        pass

    def depart_option_string(self, node):
        pass

    def visit_organization(self, node):
        self.visit_docinfo_item(node, 'organization')

    def depart_organization(self, node):
        self.depart_docinfo_item()

    def visit_paragraph(self, node):
        self.context.append(len(self.body))
        self.context.append('p')

    def depart_paragraph(self, node):
        self.context.pop()
        start = self.context.pop()
        if not self.context and self.body:
            self.append_para(self.body[start:])
            self.body = self.body[:start]

    def visit_problematic(self, node):
        pass

    def depart_problematic(self, node):
        pass

    def visit_raw(self, node):
        if node.has_key('format') and node['format'] == 'html':
            self.body.append(node.astext())
        raise nodes.SkipNode

    def visit_target(self, node):
        if not (node.has_key('refuri') or node.has_key('refid')
                or node.has_key('refname')):
            href = ''
            if node.has_key('id'):
                href = node['id']
            elif node.has_key('name'):
                href = node['name']
            self.body.append("%s%s" % (self.starttag(node, 'setLink', '', destination=href), \
                             '</setLink>'))
        raise nodes.SkipNode

    def depart_target(self, node):
        pass

    def visit_reference(self, node):
        self.context.append('a')
        if node.has_key('refuri'):
            href = node['refuri']
            self.body.append(self.starttag(node, 'a', '', href=href))
            self.context.append('</a>')
        else:
            if node.has_key('id'):
                self.body.append(self.starttag({}, 'setLink', '', destination=node['id']))
                self.context.append('</setLink>')
            if node.has_key('refid'):
                href = node['refid']
            elif node.has_key('refname'):
                href = self.document.nameids[node['refname']]
            self.body.append(self.starttag(node, 'link', '', destination=href))
            self.context.append('</link>')

    def depart_reference(self, node):
        if node.has_key('id') and \
           not node.has_key('refuri'):
            self.body.append(self.context.pop())
        self.body.append(self.context.pop())
        self.context.pop()

    def visit_revision(self, node):
        self.visit_docinfo_item(node, 'revision')

    def depart_revision(self, node):
        self.depart_docinfo_item()

    def visit_row(self, node):
        pass

    def depart_row(self, node):
        pass

    def visit_section(self, node):
        self.sectionlevel += 1

    def depart_section(self, node):
        self.sectionlevel -= 1

    def visit_status(self, node):
        self.visit_docinfo_item(node, 'status')

    def depart_status(self, node):
        self.depart_docinfo_item()

    def visit_strong(self, node):
        self.context.append('b')
        self.body.append('<b>')

    def depart_strong(self, node):
        self.context.pop()
        self.body.append('</b>')

    def visit_subtitle(self, node):
        self.context.append(len(self.body))
        self.context.append('subtitle')

    def depart_subtitle(self, node):
        style = self.context.pop()
        start = self.context.pop()
        self.append_para(self.body[start:], style)
        self.body = self.body[:start]

    def visit_title(self, node):
        atts = {}
        self.context.append(len(self.body))
        self.context.append('title')
        if isinstance(node.parent, nodes.topic):
            self.context.append('')
            self.topic_class = 'topic-title'
        elif self.sectionlevel == 0:
            self.context.append('title')
        else:
            self.context.append("h%s" % self.sectionlevel)

        if self.context[-1] != 'title':
            if node.parent.hasattr('id'):
                self.context.append('</setLink>')
                self.body.append(self.starttag({}, 'setLink', '', destination=node.parent['id']))
            if node.hasattr('refid'):
                self.context.append('</link>')
                self.body.append(self.starttag({}, 'link', '', destination=node['refid']))
        else:
            self.context.append('')

    def depart_title(self, node):
        if node.hasattr('refid'):
            self.body.append(self.context.pop())
        if node.parent.hasattr('id'):
            self.body.append(self.context.pop())
        style = self.context.pop()
        self.context.pop()
        if isinstance(node.parent, nodes.topic):
            style = self.topic_class
        start = self.context.pop()
        self.append_para(self.body[start:], style)
        self.body = self.body[:start]

    def unimplemented_visit(self, node):
        raise NotImplementedError('visiting unimplemented node type: %s'
                                  % node.__class__.__name__)

    def visit_topic(self, node):
        if node.hasattr('id'):
            self.context.append('</setLink>')
            self.body.append(self.starttag({}, 'setLink', '', destination=node['id']))

    def depart_topic(self, node):
        if node.hasattr('id'):
            self.body.append(self.context.pop())

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

    def invisible_visit(self, node):
        """Invisible nodes should be ignored."""
        pass

    def visit_comment(self, node):
        raise nodes.SkipNode

    depart_comment = invisible_visit
    visit_substitution_definition = visit_comment
    depart_substitution_definition = depart_comment
    visit_figure = visit_comment
    depart_figure = depart_comment

    visit_sidebar = invisible_visit
    visit_warning = invisible_visit
    visit_tip = invisible_visit
    visit_tbody = invisible_visit
    visit_thead = invisible_visit
    visit_tgroup = invisible_visit
    visit_table = invisible_visit
    visit_title_reference = invisible_visit
    visit_transition = invisible_visit
    visit_pending = invisible_visit
    depart_pending = invisible_visit
    depart_transition = invisible_visit
    depart_title_reference = invisible_visit
    depart_table = invisible_visit
    depart_tgroup = invisible_visit
    depart_thead = invisible_visit
    depart_tbody = invisible_visit
    depart_tip = invisible_visit
    depart_warning = invisible_visit
    depart_sidebar = invisible_visit
