"""
:Author: Lalo Martins
:Contact: lalo@laranja.org
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Lout document tree Writer. See http://lout.sf.net/
"""

__docformat__ = 'reStructuredText'

# initial code based on copy-pastes from the latex and html writers.
#
# convention: deactivate code by two # e.g. ##.

import sys
import time
import re
import string
from types import ListType
from docutils import frontend, nodes, languages, writers

try:
    True
except NameError:
    True, False = 1, 0

class Writer(writers.Writer):

    supported = ('lout', 'loutdoc', 'loutbook', 'loutreport')
    """Formats this writer supports."""

    settings_spec = (
        'Lout-Specific Options',
        'The Lout "--output-encoding" default is "latin-1:strict".',
        (('Specify document type.  Default is "doc".  '
          'If you don\'t specify a setup file, the document type should '
          'be the name of a system include file, which will be "@SysInclude"d '
          'by Lout in the document header.  If you specify a setup file, no '
          '@SysInclude will be generated, but you still must specify the '
          'document type, because the writer will take some decisions based '
          'on known document types (for example, "doc" has no titlepage).  '
          'Accepted values are "doc", "book" and "report".',
          ['--doctype'],
          {'default': 'doc', }),
         ('Specify a setup file. The file will be "@Includ"ed by Lout in '
          'the document header.  Default is no setup file ("").  '
          'See --doctype; overridden by --setupfile-path.',
          ['--setupfile'],
          {'default': '', 'metavar': '<file>'}),
         ('Specify a setupfile file, relative to the current working '
          'directory.  Overrides --setupfile; see also --doctype.',
          ['--setupfile-path'],
          {'metavar': '<file>'}),
         ('Let Lout print docutils document info in the title page.',
          ['--use-lout-docinfo'], #not implemented
          {'default': 0, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Insert "smart-quotes" (replace "foo" by ``foo\'\').  '
          'Default: yes',
          ['--smart-quotes'],
          {'default': 1, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Do not insert "smart-quotes".  ',
          ['--no-smart-quotes'],
          {'dest': 'smart_quotes', 'action': 'store_false',
           'validator': frontend.validate_boolean}),
         ('Mark the first chapter as a preface.  '
          'Optionally, any non-titled text before the first chapter '
          'is a preface - but then it cannot have sections.  '
          '("book" doctype only)',
          ['--has-preface'],
          {'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Mark the first chapter as an introduction '
          '(or the second if the first is a preface) '
          '("book" doctype only)',
          ['--has-introduction'],
          {'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('The first level divisions are parts, not chapters.  '
          '("book" doctype only)',
          ['--has-parts'],
          {'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Use dashes (Lout --).  See dashes-regexp.',
          ['--use-dashes'],
          {'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Regular expression used to find dashes, if use-dashes is true.  '
          'Default: `(?<![\\w"{-])[-](?![\\w"}-])\'.',
          ['--dashes-regexp'],
          {'default': r'(?<![\w"{-])[-](?![\w"}-])', 'metavar': '<regexp>'}),
        ))

    settings_defaults = {'output_encoding': 'latin-1'}
    # FIXME: default doctype should depend on the writer name

    config_section = 'lout writer'
    config_section_dependencies = ('writers',)

    output = None
    """Final translated form of `document`."""

    def translate(self):
        visitor = LoutTranslator(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()
        self.body = visitor.body


_language_names = {
    'it': 'it',
    'cs': 'cs',
    'no': 'no',
    'da': 'da',
    'pl': 'pl',
    'nl': 'nl',
    'pt': 'pt',
    'pt-br': 'pt-BR', # depends on patch to Lout, comment out if it breaks for you
    'en': 'en',
    'ru': 'ru',
    'en-gb': 'en-GB',
    'fi': 'fi',
    'sl': 'sl',
    'fr': 'fr',
    'es': 'es',
    'de': 'de',
    'sv': 'sv',
    'hu': 'hu',
    'hr': 'Croatian',
    'sk': 'Slovak',
    'hsb': 'UpperSorbian',
}

def _get_language_name(lcode):
    lcode = lcode.lower()
    if _language_names.has_key(lcode):
        return _language_names[lcode]
    # support dialects
    l = lcode.split('-')[0]
    if _language_names.has_key(l):
        return _language_names[l]
    raise KeyError, 'Language %s not supported by Lout' % lcode

_lout_header = '''
# This document was generated automatically by the Python Docutils
# package. It is not meant to be edited manually.
# http://docutils.sourceforge.net/

extend @BasicSetup @DocumentSetup

    def @SideBar
	right @Text
    {
	def @Send into { @ColTopPlace&&following }
	    right x
	{
	    x
	}
	@Send @CurveBox margin { 1m } paint { rgb 1 1 0.93 } @Text
    }
''' # the @SideBar command

_book_options = {
    'title': '@Title',
    'author': '@Author',
    'version': '@Edition',
    'organization': '@Publisher',
    'beforetitlepage': '@BeforeTitlePage',
    'ontitlepage': '@OnTitlePage',
    'aftertitlepage': '@AfterTitlePage',
    'copyright': '@AfterTitlePage',
    'atend': '@AtEnd',
    'initialfont': '@InitialFont',
    'initialbreak': '@InitialBreak',
    'initialspace': '@InitialSpace',
    'initiallanguage': '@InitialLanguage',
    'pageorientation': '@PageOrientation',
    'pageheaders': '@PageHeaders',
    'columnnumber': '@ColumnNumber',
    'firstpagenumber': '@FirstPageNumber',
    'introfirstpagenumber': '@IntroFirstPageNumber',
    'optimizepages': '@OptimizePages',
    'glossarytext': '@GlossaryText',
    'indextext': '@IndexText',
    'indexatext': '@IndexAText',
    'indexbtext': '@IndexBText',
}

class LoutTranslator(nodes.NodeVisitor):

    """
    WRITEME
    """
    _special_chars = '#&/@^{|}~'
    words_and_spaces = re.compile(r'\S+| +|\n') # REMOVEME

    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.settings = settings = document.settings
        lcode = settings.language_code
        self.language = languages.get_language(lcode)
        self.language_name = _get_language_name(lcode)
        self.section_level = 0
        self.section_title = None
        self.section_commands = []
        self.section_nesting = 0
        self.section_nesting_stack = []
        self.body = []
        self.literal_block = self.literal = None
        self.in_dquote = None
        self.docinfo = {}
        self.doctype = self.settings.doctype.lower()
        if self.doctype == 'book':
            self.chapters = []
            self.parts = []
            self.preface = []
            self.introduction = []
        self.topic_class = ''
        self.kill_next_paragraph = False
        self.dashes_re = re.compile(settings.dashes_regexp)

    def astext(self):
        return ''.join(self.body)

    def encode(self, text):
        """Encode special characters in `text` & return."""
        # small complication: must encode " and \ at the same time
        if (not self.settings.smart_quotes) or self.literal_block or self.literal:
            newt = []
            for part in text.split('"'):
                newt.append(part.replace('\\', r'"\\"'))
            text = '"\\""'.join(newt)
        else:
            oldt = text.split('"')
            newt = [oldt[0].replace('\\', r'"\\"')]
            for part in range(1, len(oldt)):
                if oldt[part-1] and oldt[part-1][-1] == '\\':
                    newt[-1] = oldt[part-1][:-1] + '"\\""'
                elif not (oldt[part-1] and oldt[part-1][-1].isalnum()):
                    newt.append("``")
                    self.in_dquote = 1
                elif self.in_dquote and not (oldt[part] and oldt[part][0].isalnum()):
                    newt.append("''")
                    self.in_dquote = 1
                else:
                    newt.append('"\\""')
                newt.append(oldt[part].replace('\\', r'"\\"'))
            text = ''.join(newt)
        # special chars
        for c in self._special_chars:
            text = text.replace(c, '"%s"' % c)
        # Lout replaces -- and --- for wide dash characters - we don't want that
        newt = []
        for part in text.split('---'):
            newt.append(part.replace('--', '"--"'))
        text = '{@OneRow {- |2p {-} |2p -}}'.join(newt)
        # insert dashes, if requested
        # FIXME: have to document how this interacts with previous replacements
        if self.settings.use_dashes:
            text = self.dashes_re.sub('--', text)
        # done
        return text

    def attval(self, text, whitespace=re.compile('[\n\r\t\v\f]')):
        """REMOVEME"""
        return ''

    def starttag(self, node, tagname, suffix='\n', infix='', **attributes):
        """REMOVEME"""
        return '\n@CurveBox paint { red } { %s %s }\n' % (tagname,
                                                      self.encode(repr(attributes)))

    def emptytag(self, node, tagname, suffix='\n', **attributes):
        """REMOVEME"""
        return ''

    def visit_Text(self, node):
        self.body.append(self.encode(node.astext()))

    def depart_Text(self, node):
        pass

    def visit_abbreviation(self, node):
        # @@@ implementation incomplete ("title" attribute)
        self.body.append(self.starttag(node, 'abbr', ''))

    def depart_abbreviation(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" abbr }\n')

    def visit_acronym(self, node):
        # @@@ implementation incomplete ("title" attribute)
        self.body.append(self.starttag(node, 'acronym', ''))

    def depart_acronym(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" acronym }\n')

    def visit_address(self, node):
        self.visit_docinfo_item(node, 'address', meta=None)
        self.body.append(self.starttag(node, 'pre', CLASS='address'))

    def depart_address(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" pre }\n')
        self.depart_docinfo_item()

    def visit_admonition(self, node, name=''):
        self.body.append(self.starttag(node, 'div',
                                        CLASS=(name or 'admonition')))
        if name:
            self.body.append('\n@CurveBox paint { red } { p class="admonition-title" }\n'
                             + self.language.labels[name] + '\n@CurveBox paint { red } { "/" p }\n')

    def depart_admonition(self, node=None):
        self.body.append('\n@CurveBox paint { red } { "/" div }\n')

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

    attribution_formats = {'dash': ('&mdash;', ''),
                           'parentheses': ('(', ')'),
                           'parens': ('(', ')'),
                           'none': ('', '')}

    def visit_attribution(self, node):
        pass

    def depart_attribution(self, node):
        pass

    def visit_author(self, node):
        self.visit_docinfo_item(node, 'author')

    def depart_author(self, node):
        self.depart_docinfo_item()

    def visit_authors(self, node):
        pass

    def depart_authors(self, node):
        pass

    def visit_block_quote(self, node):
        self.body.append('\n@QuotedDisplay {\n')
        self.kill_next_paragraph = True

    def depart_block_quote(self, node):
        self.body.append('\n}\n')

    def visit_bullet_list(self, node):
        if self.topic_class == 'contents':
            # we let Lout generate contents, thank you sir
            raise nodes.SkipNode
        self.body.append('\n@')
        if self.kill_next_paragraph:
            self.kill_next_paragraph = False
            self.body.append('Raw')
        self.body.append('BulletList\n')

    def depart_bullet_list(self, node):
        self.body.append('\n@RawEndList\n')
    
    def visit_caption(self, node):
        self.body.append(self.starttag(node, 'p', '', CLASS='caption'))

    def depart_caption(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" p }\n')

    def visit_caution(self, node):
        self.visit_admonition(node, 'caution')

    def depart_caution(self, node):
        self.depart_admonition()

    def visit_citation(self, node):
        self.body.append(self.starttag(node, 'table', CLASS='citation',
                                       frame="void", rules="none"))
        self.body.append('\n@CurveBox paint { red } { colgroup }@CurveBox paint { red } { col class="label" "/" }@CurveBox paint { red } { col "/" }@CurveBox paint { red } { "/" colgroup }\n'
                         '\n@CurveBox paint { red } { col "/" }\n'
                         '\n@CurveBox paint { red } { tbody valign="top" }\n'
                         '\n@CurveBox paint { red } { tr }\n')
        self.footnote_backrefs(node)

    def depart_citation(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" td }@CurveBox paint { red } { "/" tr }\n'
                         '\n@CurveBox paint { red } { "/" tbody }\n@CurveBox paint { red } { "/" table }\n')

    def visit_citation_reference(self, node):
        href = ''
        if node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        self.body.append(self.starttag(node, 'a', '[', href=href,
                                       CLASS='citation-reference'))

    def depart_citation_reference(self, node):
        self.body.append(']@CurveBox paint { red } { "/" a }\n')

    def visit_classifier(self, node):
        self.body.append(' @CurveBox paint { red } { span class="classifier-delimiter" }:@CurveBox paint { red } { "/" span } ')
        self.body.append(self.starttag(node, 'span', '', CLASS='classifier'))

    def depart_classifier(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" span }\n')

    def visit_colspec(self, node):
        raise nodes.SkipNode

    def depart_colspec(self, node):
        pass

    def visit_comment(self, node):
        for line in node.astext().split('\n'):
            self.body.append('# %s\n' % line)
        # Content already processed:
        raise nodes.SkipNode

    def visit_contact(self, node):
        self.visit_docinfo_item(node, 'contact', meta=None)

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

    def visit_decoration(self, node):
        pass

    def depart_decoration(self, node):
        pass

    def visit_definition(self, node):
        self.body.append(' { ')
        self.kill_next_paragraph = True

    def depart_definition(self, node):
        self.body.append(' }\n')

    def visit_definition_list(self, node):
        self.body.append('\n@')
        if self.kill_next_paragraph:
            self.kill_next_paragraph = False
            self.body.append('Raw')
        self.body.append('TaggedList\n')

    def depart_definition_list(self, node):
        self.body.append('\n@RawEndList\n')

    def visit_definition_list_item(self, node):
        pass

    def depart_definition_list_item(self, node):
        pass

    def visit_description(self, node):
        self.body.append(self.starttag(node, 'td', ''))
        if len(node):
            node[0].set_class('first')
            node[-1].set_class('last')

    def depart_description(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" td }\n')

    def visit_docinfo(self, node):
        pass

    def depart_docinfo(self, node):
        pass

    def visit_docinfo_item(self, node, name, meta=1):
        if self.docinfo.get(name):
            self.docinfo[name] = ', '.join((self.docinfo[name], node.astext()))
        else:
            self.docinfo[name] = node.astext()
        raise nodes.SkipNode
        self.body.append(self.starttag(node, 'tr', ''))
        self.body.append('\n@CurveBox paint { red } { th class="docinfo-name" }%s:@CurveBox paint { red } { "/" th }\n@CurveBox paint { red } { td }\n'
                         % self.language.labels[name])
        if len(node):
            if isinstance(node[0], nodes.Element):
                node[0].set_class('first')
            if isinstance(node[-1], nodes.Element):
                node[-1].set_class('last')

    def depart_docinfo_item(self):
        return
        self.body.append('\n@CurveBox paint { red } { "/" td }@CurveBox paint { red } { "/" tr }\n')

    def visit_doctest_block(self, node):
        self.body.append(self.starttag(node, 'pre', CLASS='doctest-block'))

    def depart_doctest_block(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" pre }\n')

    def visit_document(self, node):
        if self.doctype == 'report':
            raise NotImplemented
        pass

    def depart_document(self, node):
        if self.doctype == 'doc':
            # FIXME: add docinfo as a table?
            self.body.insert(0, "@SysInclude { doc }\n")
            self.body.insert(1, "@Doc @Text @Begin\n")
            self.body.append("\n@End @Text\n")
        elif self.doctype == 'book':
            # FIXME: add docinfo
            self.body = ["@SysInclude { book }\n",
                         "@Book\n"
                         "    @InitialLanguage { %s }\n"
                         % self.language_name]
            extra_docinfo = []
            if self.docinfo.has_key('subtitle'):
                title = self.encode(self.docinfo.get('title', ''))
                subt = self.encode(self.docinfo['subtitle'])
                del self.docinfo['title'], self.docinfo['subtitle']
                self.body.append("    @Title {\n%s\n@LLP\n-6p @Font @II { %s }\n}\n" % (
                    title, subt))
            for name, value in self.docinfo.items():
                key = name.lower()
                if key in _book_options.keys():
                    self.body.append("    %s { %s }\n" % (_book_options[key],
                                                        self.encode(value)))
                else:
                    if name == key:
                        name = name.capitalize()
                    extra_docinfo.append('%s: %s' % (name, self.encode(value)))
            if extra_docinfo:
                if not self.docinfo.has_key('beforetitlepage'):
                    self.body.append("    @BeforeTitlePage { %s }\n" % ('\n'.join(extra_docinfo)))
                elif not self.docinfo.has_key('aftertitlepage'):
                    self.body.append("    @AfterTitlePage { %s }\n" % ('\n'.join(extra_docinfo)))
            self.body.append("//\n")
            if self.preface:
                self.body.extend(self.preface)
            if self.introduction:
                self.body.extend(self.introduction)
            if self.settings.has_parts:
                for part in self.parts:
                    for chapter in part:
                        self.body.extend(chapter)
            else:
                for chapter in self.chapters:
                    self.body.extend(chapter)
        elif self.doctype == 'report':
            raise NotImplemented
        if self.settings.setupfile_path or self.settings.setupfile:
            if self.settings.setupfile_path:
                name = self.settings.setupfile_path
            else:
                name = self.settings.setupfile
            self.body[0] = '@Include { "%s" }\n' % name
        else:
            self.body.insert(0, '@SysInclude { tbl }\n')
        self.body.insert(1, _lout_header)

    def visit_emphasis(self, node):
        self.body.append('@II { ')

    def depart_emphasis(self, node):
        self.body.append(' }')

    def visit_entry(self, node):
        letter = chr(node.parent.index(node) + 65)
        self.body.append('%s { ' % letter)
        if isinstance(node.parent.parent, nodes.thead):
            self.body.append('@Heading { ')
        self.kill_next_paragraph = True
        return
        atts = {}
        if node.has_key('morerows'):
            atts['rowspan'] = node['morerows'] + 1
        if node.has_key('morecols'):
            atts['colspan'] = node['morecols'] + 1
        self.body.append(self.starttag(node, tagname, '', **atts))
        if len(node) == 0:              # empty cell
            self.body.append('&nbsp;')
        else:
            node[0].set_class('first')
            node[-1].set_class('last')

    def depart_entry(self, node):
        if isinstance(node.parent.parent, nodes.thead):
            self.body.append(' }')
        self.body.append(' }\n')

    def visit_enumerated_list(self, node):
        self.body.append('\n@')
        if self.kill_next_paragraph:
            self.kill_next_paragraph = False
            self.body.append('Raw')
        if node.get('prefix', '') == '(' and node.get('suffix', '') == ')':
            self.body.append('Paren')
        if not node.has_key('enumtype'):
            self.body.append('NumberedList\n')
        elif node['enumtype'] == 'arabic':
            self.body.append('NumberedList\n')
        elif node['enumtype'] == 'loweralpha':
            self.body.append('AlphaList\n')
        elif node['enumtype'] == 'upperalpha':
            self.body.append('UCAlphaList\n')
        elif node['enumtype'] == 'lowerroman':
            self.body.append('RomanList\n')
        elif node['enumtype'] == 'upperroman':
            self.body.append('UCRomanList\n')
        if node.has_key('start'):
            self.body.append('  start { %s }\n' % node['start'])

    def depart_enumerated_list(self, node):
        self.body.append('\n@RawEndList\n')

    def visit_error(self, node):
        self.visit_admonition(node, 'error')

    def depart_error(self, node):
        self.depart_admonition()

    def visit_field(self, node):
        if isinstance(node.parent, nodes.docinfo):
            self.docinfo[node[0].astext()] = node[1].astext()
            raise nodes.SkipNode

    def depart_field(self, node):
        pass

    def visit_field_body(self, node):
        self.body.append(' { ')
        self.kill_next_paragraph = True

    def depart_field_body(self, node):
        self.body.append(' }\n')

    def visit_field_list(self, node):
        self.body.append('\n@')
        if self.kill_next_paragraph:
            self.kill_next_paragraph = False
            self.body.append('Raw')
        self.body.append('WideTaggedList\n')
        return

    def depart_field_list(self, node):
        self.body.append('\n@RawEndList\n')

    def visit_field_name(self, node):
        if len(node.astext()) > 7:
            self.body.append('@DropTagItem { ')
        else:
            self.body.append('@TagItem { ')

    def depart_field_name(self, node):
        self.body.append(' }')

    def visit_figure(self, node):
        atts = {'class': 'figure'}
        if node.get('width'):
            atts['style'] = 'width: %spx' % node['width']
        self.body.append(self.starttag(node, 'div', **atts))

    def depart_figure(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" div }\n')

    def visit_footer(self, node):
        pass

    def depart_footer(self, node):
        footer = (['\n@CurveBox paint { red } { hr class="footer" "/" }\n',
                   self.starttag(node, 'div', CLASS='footer')]
                  + self.body[start:] + ['\n@CurveBox paint { red } { "/" div }\n'])
        self.body_suffix[:0] = footer
        del self.body[start:]

    def visit_footnote(self, node):
        self.body.append(self.starttag(node, 'table', CLASS='footnote',
                                       frame="void", rules="none"))
        self.body.append('\n@CurveBox paint { red } { colgroup }@CurveBox paint { red } { col class="label" "/" }@CurveBox paint { red } { col "/" }@CurveBox paint { red } { "/" colgroup }\n'
                         '\n@CurveBox paint { red } { tbody valign="top" }\n'
                         '\n@CurveBox paint { red } { tr }\n')
        self.footnote_backrefs(node)

    def footnote_backrefs(self, node):
        if self.settings.footnote_backlinks and node.hasattr('backrefs'):
            backrefs = node['backrefs']
            if len(backrefs) == 1:
                pass
            else:
                i = 1
                backlinks = []
                for backref in backrefs:
                    backlinks.append('\n@CurveBox paint { red } { a class="fn-backref" href="#%s" }%s@CurveBox paint { red } { "/" a }\n'
                                     % (backref, i))
                    i += 1

    def depart_footnote(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" td }@CurveBox paint { red } { "/" tr }\n'
                         '\n@CurveBox paint { red } { "/" tbody }\n@CurveBox paint { red } { "/" table }\n')

    def visit_footnote_reference(self, node):
        href = ''
        if node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        format = self.settings.footnote_references
        if format == 'brackets':
            suffix = '['
        elif format == 'superscript':
            suffix = '\n@CurveBox paint { red } { sup }\n'
        else:                           # shouldn't happen
            suffix = '???'
            self.content.append('???')
        self.body.append(self.starttag(node, 'a', suffix, href=href,
                                       CLASS='footnote-reference'))

    def depart_footnote_reference(self, node):
        pass

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

    def visit_header(self, node):
        pass

    def depart_header(self, node):
        self.body_prefix.append(self.starttag(node, 'div', CLASS='header'))
        self.body_prefix.extend(self.body[start:])
        self.body_prefix.append('\n@CurveBox paint { red } { hr "/" }\n@CurveBox paint { red } { "/" div }\n')
        del self.body[start:]

    def visit_hint(self, node):
        self.visit_admonition(node, 'hint')

    def depart_hint(self, node):
        self.depart_admonition()

    def visit_image(self, node):
        atts = node.attributes.copy()
        if atts.has_key('class'):
            del atts['class']           # prevent duplication with node attrs
        atts['src'] = atts['uri']
        del atts['uri']
        if not atts.has_key('alt'):
            atts['alt'] = atts['src']
        if isinstance(node.parent, nodes.TextElement):
            pass
        else:
            if atts.has_key('align'):
                self.body.append('\n@CurveBox paint { red } { p align="%s" }\n' %
                                 (self.attval(atts['align'],)))
            else:
                self.body.append('\n@CurveBox paint { red } { p }\n')
        self.body.append(self.emptytag(node, 'img', '', **atts))

    def depart_image(self, node):
        pass

    def visit_important(self, node):
        self.visit_admonition(node, 'important')

    def depart_important(self, node):
        self.depart_admonition()

    def visit_inline(self, node):
        self.body.append(self.starttag(node, 'span', ''))

    def depart_inline(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" span }\n')

    def visit_label(self, node):
        pass

    def depart_label(self, node):
        pass

    def visit_legend(self, node):
        self.body.append(self.starttag(node, 'div', CLASS='legend'))

    def depart_legend(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" div }\n')

    def visit_line_block(self, node):
        self.body.append(self.starttag(node, 'pre', CLASS='line-block'))

    def depart_line_block(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" pre }\n')

    def visit_list_item(self, node):
        self.body.append('\n@ListItem { ')
        self.kill_next_paragraph = True

    def depart_list_item(self, node):
        self.body.append(' }\n')

    def visit_literal(self, node):
        """Process text to prevent tokens from wrapping."""
        self.body.append(self.starttag(node, 'tt', '', CLASS='literal'))
        text = node.astext()
        for token in self.words_and_spaces.findall(text):
            if token.strip():
                # Protect text like "--an-option" from bad line wrapping:
                self.body.append('\n@CurveBox paint { red } { span class="pre" }%s@CurveBox paint { red } { "/" span }\n'
                                 % self.encode(token))
            elif token in ('\n', ' '):
                # Allow breaks at whitespace:
                self.body.append(token)
            else:
                # Protect runs of multiple spaces; the last space can wrap:
                self.body.append('&nbsp;' * (len(token) - 1) + ' ')
        self.body.append('\n@CurveBox paint { red } { "/" tt }\n')
        # Content already processed:
        raise nodes.SkipNode

    def visit_literal_block(self, node):
        self.body.append(self.starttag(node, 'pre', CLASS='literal-block'))

    def depart_literal_block(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" pre }\n')

    def visit_meta(self, node):
        pass

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
        self.body.append(node.get('delimiter', ' '))
        self.body.append(self.starttag(node, 'var', ''))

    def depart_option_argument(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" var }\n')

    def visit_option_group(self, node):
        atts = {}
        if len(node.astext()) > 10:
            atts['colspan'] = 2
        self.body.append(self.starttag(node, 'td', **atts))
        self.body.append('\n@CurveBox paint { red } { kbd }\n')

    def depart_option_group(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" kbd }@CurveBox paint { red } { "/" td }\n')

    def visit_option_list(self, node):
        self.body.append(
              self.starttag(node, 'table', CLASS='option-list',
                            frame="void", rules="none"))
        self.body.append('\n@CurveBox paint { red } { col class="option" "/" }\n'
                         '\n@CurveBox paint { red } { col class="description" "/" }\n'
                         '\n@CurveBox paint { red } { tbody valign="top" }\n')

    def depart_option_list(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" tbody }\n@CurveBox paint { red } { "/" table }\n')
        #self.kill_next_paragraph = True

    def visit_option_list_item(self, node):
        self.body.append(self.starttag(node, 'tr', ''))

    def depart_option_list_item(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" tr }\n')

    def visit_option_string(self, node):
        self.body.append(self.starttag(node, 'span', '', CLASS='option'))

    def depart_option_string(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" span }\n')

    def visit_organization(self, node):
        self.visit_docinfo_item(node, 'organization')

    def depart_organization(self, node):
        self.depart_docinfo_item()

    def visit_paragraph(self, node):
        if self.kill_next_paragraph:
            self.kill_next_paragraph = False
        else:
            self.body.append('\n@PP\n')

    def depart_paragraph(self, node):
        pass

    def visit_problematic(self, node):
        if node.hasattr('refid'):
            self.body.append('\n@CurveBox paint { red } { a href="#%s" name="%s" }\n' % (node['refid'],
                                                           node['id']))
        self.body.append(self.starttag(node, 'span', '', CLASS='problematic'))

    def depart_problematic(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" span }\n')

    def visit_raw(self, node):
        if node.get('format') == 'lout':
            self.body.extend(['\n', node.astext(), '\n'])
        # Keep non-Lout raw text out of output:
        raise nodes.SkipNode

    def visit_reference(self, node):
        if node.has_key('refuri'):
            self.body.append('{"%s"} @ExternalLink { ' % node['refuri'])
            return
        elif node.has_key('refid'):
            href = node['refid']
        elif node.has_key('refname'):
            href = node['refid'] = self.document.nameids[node['refname']]
        self.body.append('%s @CrossLink { ' % href)

    def depart_reference(self, node):
        if node.has_key('refid'):
            # FIXME should be a setting
            self.body.append(' (p. @PageOf {%s})' % node['refid'])
        self.body.append(' }')

    def visit_revision(self, node):
        self.visit_docinfo_item(node, 'revision', meta=None)

    def depart_revision(self, node):
        self.depart_docinfo_item()

    def visit_row(self, node):
        self.body.append('@Row format { ')
        col = 65
        for child in node:
            # FIXME: support spans
            self.body.append('@Cell %s' % chr(col))
            self.body.append(' | ')
            col += 1
        self.body[-1] = ' }\n'

    def depart_row(self, node):
        pass

    def visit_rubric(self, node):
        # red perhaps?
        self.body.append('@RightDisplay @I {')


    def depart_rubric(self, node):
        self.body.append('}\n')
        self.kill_next_paragraph = True

    def visit_section(self, node):
        #print 'visit section level', self.section_level, node.__dict__
        self.section_level += 1
        if self.doctype == 'doc':
            pass
        elif self.doctype == 'book':
            if self.section_level == 1:
                if not (self.chapters or self.parts):
                    # first one
                    if self.body and not (self.preface or self.introduction):
                        # text before first chapter is always a preface
                        self.body.insert(0, '@Preface\n'
                                         '@Begin\n')
                        self.body.append('\n@End @Preface\n')
                        self.preface = self.body
                        self.body = []
                        # don't return, as we're actually starting the *next* chapter
                        # after the preface
                    elif self.settings.has_preface and not self.preface:
                        self.body = self.preface = [
                            '@Preface\n'
                            '  @Title { ',
                            '', # title placeholder
                            ' }\n'
                            '@Begin\n']
                        self.section_commands.append('@Preface')
                        self.section_title = 1
                        return
                    if self.settings.has_introduction and not self.introduction:
                        self.body = self.introduction = [
                            '\n@Introduction\n'
                            '  @Title { ',
                            '', # title placeholder
                            ' }\n'
                            '@Begin\n']
                        self.section_commands.append('@Introduction')
                        self.section_title = 1
                        return
                    # no, it's actually a chapter or part
                    self.body = []
                    self.chapters.append(self.body)
                    if self.settings.has_parts:
                        self.body.extend([0, ''])
                        self.section_title = 1
                        self.parts.append(self.chapters)
                elif self.settings.has_parts:
                    # new part (not first)
                    self.body = [0, '']
                    self.section_title = 1
                    self.chapters = [self.body]
                    self.parts.append(self.chapters)
            level = self.section_level
            extra = ''
            if self.settings.has_parts:
                level -= 1
            if level > 1 and (self.body is self.introduction or self.body is self.preface):
                # prefaces and introductions have no sections (why?)
                # IMPROVEME: perhaps allow two or three different levels?
                self.body.extend(['\n@Display @Heading { ',
                                  '', # title placeholder
                                  ' }\n'])
                self.section_title = len(self.body) - 2
                self.section_commands.append(None)
                # reset state stuff that may be left over
                self.kill_next_paragraph = True
                return
            if level == 1: # chapter
                if self.body and type(self.body[0]) is type(0): # first real chapter in a part
                    del self.chapters[0] # we don't want it on the output
                    del self.body[0]     # delete the marker too
                    ptitle = self.body.pop(0)
                    if self.body and self.body[0] == '\n@PP\n': # always is
                        del self.body[0]
                    ptext = ''.join(self.body).strip()
                    extra = '  @PartTitle { %s }\n' % self.encode(ptitle)
                    if ptext:
                        extra += '\n  @PartText {\n%s\n}\n' % ptext
                self.body = []
                self.chapters.append(self.body)
                self.section_commands.append('@Chapter')
            elif level == 2:
                self.section_commands.append('@Section')
            elif level == 3:
                self.section_commands.append('@SubSection')
            elif level == 4:
                self.section_commands.append('@SubSubSection')
            if 0 < level <= 4:
                if level > 1 and self.section_nesting < self.section_level:
                    self.section_nesting_stack.append((self.section_nesting,
                                                       self.section_commands[-1][1:]))
                    self.body.append('\n@Begin%ss\n' % self.section_nesting_stack[-1][1])
                    self.section_nesting = self.section_level
                self.body.append('\n%s @Title { ' % self.section_commands[-1])
                self.section_title = len(self.body)
                self.body.extend(['', ' }\n'])
                if node.hasattr('id'):
                    self.body.append('  @Tag { %s }\n' % node['id'])
                if extra:
                    self.body.append(extra)
                self.body.append('@Begin\n')
            elif level > 4: # unsupported level, let's cheat
                # IMPROVEME: perhaps allow two or three different levels?
                self.body.extend(['\n@Display @Heading { ',
                                  '', # title placeholder
                                  ' }\n'])
                self.section_title = len(self.body) - 2
                self.section_commands.append(None)
                self.kill_next_paragraph = True
                return
        # reset state stuff that may be left over
        self.kill_next_paragraph = False

    def depart_section(self, node):
        while self.section_nesting > self.section_level:
            self.section_nesting, cmd = self.section_nesting_stack.pop()
            self.body.append('\n@End%ss\n' % cmd)
        self.section_level -= 1
        if self.section_commands:
            cmd = self.section_commands.pop()
            if cmd:
                self.body.append('\n@End %s\n' % cmd)

    def visit_sidebar(self, node):
        self.body.append('\n@SideBar { ')
        self.in_sidebar = 1
        self.kill_next_paragraph = False

    def depart_sidebar(self, node):
        self.body.append(' }\n')
        self.in_sidebar = None

    def visit_status(self, node):
        self.visit_docinfo_item(node, 'status', meta=None)

    def depart_status(self, node):
        self.depart_docinfo_item()

    def visit_strong(self, node):
        self.body.append('@B { ')

    def depart_strong(self, node):
        self.body.append(' }')

    def visit_subscript(self, node):
        self.body.append(self.starttag(node, 'sub', ''))

    def depart_subscript(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" sub }\n')

    def visit_substitution_definition(self, node):
        """Internal only."""
        raise nodes.SkipNode

    def visit_substitution_reference(self, node):
        self.unimplemented_visit(node)

    def visit_subtitle(self, node):
        if isinstance(node.parent, nodes.sidebar):
            self.body.append('@LLP @RawCentredDisplay @I { ')
        else:
            self.docinfo['subtitle'] = node.astext()
            raise nodes.SkipNode

    def depart_subtitle(self, node):
        self.body.append(' }\n')

    def visit_superscript(self, node):
        self.body.append(self.starttag(node, 'sup', ''))

    def depart_superscript(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" sup }\n')

    def visit_system_message(self, node):
        if node['level'] < self.document.reporter['writer'].report_level:
            # Level is too low to display:
            raise nodes.SkipNode
        self.body.append(self.starttag(node, 'div', CLASS='system-message'))
        #self.body.append('\n@CurveBox paint { red } { p class="system-message-title" }\n')
        attr = {}
        backref_text = ''
        if node.hasattr('id'):
            attr['name'] = node['id']
        if node.hasattr('backrefs'):
            backrefs = node['backrefs']
            if len(backrefs) == 1:
                backref_text = ('; @CurveBox paint { red } { : backlink }\n'
                                % backrefs[0])
            else:
                i = 1
                backlinks = []
                for backref in backrefs:
                    backlinks.append('\n@CurveBox paint { red } { -> %s %s }\n' % (backref, i))
                    i += 1
                backref_text = ('; @CurveBox paint { red } { -> %s }\n'
                                % ', '.join(backlinks))
        if node.hasattr('line'):
            line = ', line %s' % node['line']
        else:
            line = ''
        if attr:
            a_start = self.starttag({}, 'a', '', **attr)
            a_end = '\n@CurveBox paint { red } { "/" a }\n'
        else:
            a_start = a_end = ''
        self.body.append('System Message: %s%s/%s%s (%s %s)%s\n'
                         % (a_start, node['type'], node['level'], a_end,
                            self.encode(node['source']), line, backref_text))

    def depart_system_message(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" div }\n')

    def visit_table(self, node):
        self.body.append('\n@Display @Tbl\n')
        if node.get('class', '') != 'norules':
            self.body.append(' rule { yes }\n')
        self.body.append('{\n')

    def depart_table(self, node):
        self.body.append('\n}\n')
        self.kill_next_paragraph = True

    def visit_target(self, node):
        if not (node.has_key('refuri') or node.has_key('refid')
                or node.has_key('refname')):
            self.body.append(' @PageMark { %s } ' % node.parent['id'])

    def depart_target(self, node):
        pass

    def visit_tbody(self, node):
        pass

    def depart_tbody(self, node):
        pass

    def visit_term(self, node):
        if len(node.astext()) > 3:
            self.body.append('\n@DropTagItem { ')
        else:
            self.body.append('\n@TagItem { ')

    def depart_term(self, node):
        self.body.append(' }\n')

    def visit_tgroup(self, node):
        pass

    def depart_tgroup(self, node):
        pass

    def visit_thead(self, node):
        pass

    def depart_thead(self, node):
        pass

    def visit_tip(self, node):
        self.visit_admonition(node, 'tip')

    def depart_tip(self, node):
        self.depart_admonition()

    def visit_title(self, node):
        # there is a limited number of title levels in Lout;
        # worse, the number (and the commands, in fact) depends
        # on the doctype
        check_id = 0
        if isinstance(node.parent, nodes.topic):
            self.body.append(
                  self.starttag(node, 'p', '', CLASS='topic-title'))
            check_id = 1
        elif isinstance(node.parent, nodes.sidebar):
            self.body.append('@RawDisplay @Heading { ')
            check_id = 1
        elif isinstance(node.parent, nodes.admonition):
            self.body.append(
                  self.starttag(node, 'p', '', CLASS='admonition-title'))
            check_id = 1
        else:
            if self.section_level == 0: # book title
                self.docinfo['title'] = node.astext()
            else: # regular title, handled in visit_section
                self.body[self.section_title] = self.encode(node.astext())
            raise nodes.SkipNode
        if check_id:
            if node.parent.hasattr('id'):
                self.body.append('@PageMark { %s }\n' % node.parent['id'])

    def depart_title(self, node):
        if (isinstance(node.parent, nodes.sidebar)):
            self.body.append(' }\n')

    def visit_title_reference(self, node):
        self.body.append(self.starttag(node, 'cite', ''))

    def depart_title_reference(self, node):
        self.body.append('\n@CurveBox paint { red } { "/" cite }\n')

    def visit_topic(self, node):
        self.topic_class = node.get('class')
        if self.topic_class == 'contents':
            raise nodes.SkipNode
        self.body.append(self.starttag(node, 'div', CLASS='topic'))

    def depart_topic(self, node):
        if self.topic_class != 'contents':
            self.body.append('\n@CurveBox paint { red } { "/" div }\n')
        self.topic_class = ''

    def visit_transition(self, node):
        self.body.append('\n@LP @FullWidthRule @PP\n')

    def depart_transition(self, node):
        pass

    def visit_version(self, node):
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
