#! /usr/bin/env python

"""
:Author: Engelbert Gruber
:Contact: grubert@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

LaTeX2e document tree Writer.
"""

__docformat__ = 'reStructuredText'

# convention deactivate code by two # e.g. ##.

import sys
import time
import re
import string
from types import ListType
from docutils import writers, nodes, languages

# country code by a.schlock.
# partly manually converted from iso and babel stuff, dialects and some
# languages remain missing (austrian, UKEnglish, brazillian etc.)
_ISO639_TO_BABEL = {
    'no': 'norsk',     #XXX added by hand ( forget about nynorsk?)
    'gd': 'scottish',  #XXX added by hand
    'hu': 'magyar',    #XXX added by hand
    'pt': 'portuguese',#XXX added by hand
    'sl': 'slovenian',
    'af': 'afrikaans',
    'bg': 'bulgarian',
    'br': 'breton',
    'ca': 'catalan',
    'cs': 'czech',
    'cy': 'welsh',
    'da': 'danish',

    'de': 'ngerman',  #XXX rather than german
    'el': 'greek',
    'en': 'english',
    'eo': 'esperanto',
    'es': 'spanish',
    'et': 'estonian',
    'eu': 'basque',
    'fi': 'finnish',
    'ga': 'irish',
    'gl': 'galician',
    'he': 'hebrew',
    'hr': 'croatian',
    'hu': 'hungarian',
    'is': 'icelandic',
    'it': 'italian',
    'la': 'latin',
    'nl': 'dutch',
    'pl': 'polish',
    'pt': 'portuguese',
    'ro': 'romanian',
    'ru': 'russian',
    'sk': 'slovak',
    'sr': 'serbian',
    'sv': 'swedish',
    'tr': 'turkish',
    'uk': 'ukrainian'
    }

class Writer(writers.Writer):

    supported = ('latex','latex2e')
    """Formats this writer supports."""

    settings_spec = (
        'LaTeX-Specific Options',
        'The LaTeX "--output-encoding" default is "latin-1".',
        (('Specify documentclass.  Default is "article".',
          ['--documentclass'],
          {'default': 'article', }),
         ('Format for footnote references: one of "superscript" or '
          '"brackets".  Default is "superscript".',
          ['--footnote-references'],
          {'choices': ['superscript', 'brackets'], 'default': 'brackets',
           'metavar': '<FORMAT>'}),
          ))

    settings_default_overrides = {'output_encoding': 'latin-1'}

    output = None
    """Final translated form of `document`."""

    def translate(self):
        visitor = LaTeXTranslator(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()
        self.head_prefix = visitor.head_prefix
        self.head = visitor.head
        self.body_prefix = visitor.body_prefix
        self.body = visitor.body
        self.body_suffix = visitor.body_suffix

"""
Notes on LaTeX
--------------

* Put labels inside environments::
    \chapter[optional]{title}
    \label{lab} % optional, for cross referencing
    text for this unit ...
* unnumbered sections are not written to tableofcontents.
  a. donot print numbers::
        \renewcommand{\thesection}{}
  b. use::
        \addcontentsline{file}{sec_unit}{entry}
     file toc,lof lot
     sec_unit section, subsection , ...
     entry the line::
        \numberline text pagenumber
  X. latex does not support multiple tocs in one document.
     (might be no limitation except for docutils documenttation)

* sectioning::
    \part
    \chapter (report style only) 
    \section
    \subsection
    \subsubsection
    \paragraph
    \subparagraph
    \subsubparagraph (milstd and book-form styles only) 
    \subsubsubparagraph (milstd and book-form styles only)

"""    

class LaTeXTranslator(nodes.NodeVisitor):
    # When options are given to the documentclass, latex will pass them
    # to other packages, as done with babel. 
    # Dummy settings might be taken from document settings
    d_class = 'article'    # document.settings.stylesheet
    d_options = '10pt'  # papersize, fontsize
    d_paper = 'a4paper'
    d_margins = '2cm'
    # for pdflatex some other package. pslatex

    latex_head = '\\documentclass[%s]{%s}\n'
    encoding = '\\usepackage[latin1]{inputenc}\n'
    linking = '\\usepackage[colorlinks]{hyperref}\n'
    geometry = '\\usepackage[%s,margin=%s]{geometry}\n'
    stylesheet = '\\input{%s}\n'
    # add a generated on day , machine by user using docutils versoin.
    generator = '%% generator Docutils: http://docutils.sourceforge.net/\n'

    # use latex tableofcontents or let docutils do it.
    # BUG: not tested.
    latex_toc = 0

    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.settings = settings = document.settings
        # language: labels, bibliographic_fields, and author_separators.
        # to allow writing labes for specific languages.
        self.language = languages.get_language(settings.language_code)
        if _ISO639_TO_BABEL.has_key(settings.language_code):
            self.d_options += ',%s' % \
                    _ISO639_TO_BABEL[settings.language_code]
        self.head_prefix = [
              self.latex_head % (self.d_options,self.d_class),
              '\\usepackage{babel}\n',     # language is in documents settings.
              '\\usepackage{shortvrb}\n',  # allows verb in footnotes.
              self.encoding,
              '\\usepackage{graphicx}\n',
              '\\usepackage{color}\n',
              '\\usepackage{multirow}\n',
              self.linking,
              self.stylesheet % "style.tex",
              # geometry and fonts might go into style.tex.
              self.geometry % (self.d_paper, self.d_margins),
              #
              self.generator,
              # admonition width
              '\\newlength{\\admwidth}\n\\addtolength{\\admwidth}{0.9\\textwidth}\n'
                            ]
        if self.linking: # and maybe check for pdf
            self.pdfinfo = [ ]
            # pdftitle, pdfsubject, pdfauthor, pdfkeywords, pdfcreator, pdfproducer
        else:
            self.pdfinfo = None
        self.head = []
        self.body_prefix = ['\\raggedbottom\n']
        self.body = []
        self.body_suffix = ['\n']
        self.section_level = 0
        self.context = []
        self.topic_class = ''

    def language_label(self, docutil_label):
        if self.language.labels.has_key(docutil_label):
            return self.language.labels[docutil_label]
        return docutil_label

    def encode(self, text):
        """
        Encode special characters in `text` & return.
            # $ % & ~ _ ^ \ { }
        Escaping with a backslash does not help with backslashes, ~ and ^.
        """
        text = text.replace("\\", '{$\\backslash$}')
        text = text.replace("&", '{\\&}')
        text = text.replace("_", '{\\_}')
        text = text.replace("^", '{\verb|^|}')
        text = text.replace("%", '{\\%}')
        text = text.replace("#", '{\\#}')
        text = text.replace("~", '{\\~{ }}')
        # unicode: not needed as long  as output encoding is latin-1.
        # and not supported as long as output encoding is set to latin-1.
        text = text.replace(u'\u2020', '{$\\dagger$}')
        return text

    def attval(self, text,
               whitespace=re.compile('[\n\r\t\v\f]')):
        """Cleanse, HTML encode, and return attribute value text."""
        return self.encode(whitespace.sub(' ', text))

    def astext(self):
        if self.pdfinfo:
            pdfinfo = '\\hypersetup{\n' + ',\n'.join(self.pdfinfo) + '\n}\n'
        else:
            pdfinfo = ''
        return ''.join(self.head_prefix + self.head + [pdfinfo]
                       + self.body_prefix  + self.body + self.body_suffix)

    def visit_Text(self, node):
        self.body.append(self.encode(node.astext()))

    def depart_Text(self, node):
        pass

    def visit_address(self, node):
        self.visit_docinfo_item(node, 'address')

    def depart_address(self, node):
        self.depart_docinfo_item(node)

    def visit_admonition(self, node, name):
        self.body.append('\\begin{center}\n');
        # alternatives: parbox or minipage.
        # minpage has footnotes on the minipage.
        # BUG there is no border.
        self.body.append('\\parbox{\\admwidth}{\\textbf{'
                         + self.language.labels[name] + '}\n')

    def depart_admonition(self):
        self.body.append('}\n')
        self.body.append('\\end{center}\n');

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

    def visit_author(self, node):
        self.visit_docinfo_item(node, 'author')

    def depart_author(self, node):
        self.depart_docinfo_item(node)

    def visit_authors(self, node):
        self.visit_docinfo_item(node, 'author')

    def depart_authors(self, node):
        self.depart_docinfo_item(node)

    def visit_block_quote(self, node):
        self.body.append( '\\begin{quote}\n')

    def depart_block_quote(self, node):
        self.body.append( '\\end{quote}\n')

    def visit_bullet_list(self, node):
        if not self.latex_toc and self.topic_class == 'contents':
            self.body.append( '\\begin{itemize}\n' )
        else:
            self.body.append( '\\begin{itemize}\n' )

    def depart_bullet_list(self, node):
        if not self.latex_toc or not self.topic_class == 'contents':
            self.body.append( '\\end{itemize}\n' )

    def visit_caption(self, node):
        self.body.append( '\\caption{' )

    def depart_caption(self, node):
        self.body.append('}')

    def visit_caution(self, node):
        self.visit_admonition(node, 'caution')

    def depart_caution(self, node):
        self.depart_admonition()

    def visit_citation(self, node):
        notename = node['id']
        self.body.append('\\begin{figure}[b]')
        self.body.append('\\hypertarget{%s}' % notename)

    def depart_citation(self, node):
        self.body.append('\\end{figure}')

    def visit_citation_reference(self, node):
        href = ''
        if node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        self.body.append('[\\href{%s}{' % href)

    def depart_citation_reference(self, node):
        self.body.append('}]')

    def visit_classifier(self, node):
        self.body.append( '(\\textbf{' )

    def depart_classifier(self, node):
        self.body.append( '})\n' )

    def visit_colspec(self, node):
        self.body.append('%[visit_colspec]\n')
        self.context[-1] += 1

    def depart_colspec(self, node):
        self.body.append('%[depart_colspec]\n')

    def visit_comment(self, node,
                      sub=re.compile('\n').sub):
        """Escape end of line by a ne comment start in comment text."""
        self.body.append('%% %s \n' % sub('\n% ', node.astext()))
        raise nodes.SkipNode

    def visit_contact(self, node):
        self.visit_docinfo_item(node, 'contact')

    def depart_contact(self, node):
        self.depart_docinfo_item(node)

    def visit_copyright(self, node):
        self.visit_docinfo_item(node, 'copyright')

    def depart_copyright(self, node):
        self.depart_docinfo_item(node)

    def visit_danger(self, node):
        self.visit_admonition(node, 'danger')

    def depart_danger(self, node):
        self.depart_admonition()

    def visit_date(self, node):
        self.visit_docinfo_item(node, 'date')

    def depart_date(self, node):
        self.depart_docinfo_item(node)

#    def visit_decoration(self, node):
#        pass

#    def depart_decoration(self, node):
#        pass

    def visit_definition(self, node):
        self.body.append('%[visit_definition]\n')

    def depart_definition(self, node):
        self.body.append('%[depart_definition]\n')

    def visit_definition_list(self, node):
        self.body.append( '\\begin{description}\n' )

    def depart_definition_list(self, node):
        self.body.append( '\\end{description}\n' )

    def visit_definition_list_item(self, node):
        self.body.append('%[visit_definition_list_item]\n')

    def depart_definition_list_item(self, node):
        self.body.append('%[depart_definition_list_item]\n')

    def visit_description(self, node):
        self.body.append( ' & ' )

    def depart_description(self, node):
        pass

    def visit_docinfo(self, node):
        self.docinfo = []
        self.docinfo.append('%' + '_'*75 + '\n')
        self.docinfo.append('\\begin{tabular}{ll}\n')

    def depart_docinfo(self, node):
        self.docinfo.append('\\end{tabular}\n')
        self.body = self.docinfo + self.body
        # clear docinfo, so field names are no longer appended.
        self.docinfo = None
        if self.latex_toc:
            self.body.append('\\tableofcontents\n\n\\bigskip\n')

    def visit_docinfo_item(self, node, name):
        # should we stick to latex or docutils.
        # latex article has its own handling of date and author.
        # If we use it we get latexs language handling.
        latex_docinfo = 0
        
        if name == 'abstract':
            # NOTE tableofcontents before or after ?
            # eg after: depart_docinfo
            # NOTE this limits abstract to text.
            self.body.append('\\begin{abstract}\n')
            self.context.append('\\end{abstract}\n')
            self.context.append(self.body)
            self.context.append(len(self.body))
        else:
            if name == 'author':
                if not self.pdfinfo == None:
                    # BUG only the last one survives
                    self.pdfinfo.append( 'pdfauthor={%s}' 
                                        % self.attval(node.astext()) )
                if latex_docinfo:
                    self.head.append('\\author{%s}\n' % self.attval(node.astext()))
                    raise nodes.SkipNode
                else:
                    self.head.append('\\author{}\n')
            elif name == 'date':
                if latex_docinfo:
                    self.head.append('\\date{%s}\n' % self.attval(node.astext()))
                    raise nodes.SkipNode
                else:
                    self.head.append("\\date{}\n")

            self.docinfo.append('\\textbf{%s} &\n\t' % self.language_label(name))
            self.context.append(' \\\\\n')
            self.context.append(self.docinfo)
            self.context.append(len(self.body))
        # \thanks is a footnote to the title.

    def depart_docinfo_item(self, node):
        size = self.context.pop()
        dest = self.context.pop()
        tail = self.context.pop()
        tail = self.body[size:] + [tail]
        del self.body[size:]
        dest.extend(tail)

    def visit_doctest_block(self, node):
        self.body.append( '\\begin{verbatim}' )

    def depart_doctest_block(self, node):
        self.body.append( '\\end{verbatim}\n' )

    def visit_document(self, node):
        self.body_prefix.append('\\begin{document}\n')
        self.body_prefix.append('\\maketitle\n\n')
        # alternative use titlepage environment.
        # \begin{titlepage}

    def depart_document(self, node):
        self.body_suffix.append('\\end{document}\n')

    def visit_emphasis(self, node):
        self.body.append('\\emph{')

    def depart_emphasis(self, node):
        self.body.append('}')

    def visit_entry(self, node):
        # cell separation
        if self.context[-1] > 0:
            self.body.append(' & ')

        # multi{row,column}
        if node.has_key('morerows') and node.has_key('morecols'):
            raise NotImplementedError('LaTeX can\'t handle cells that'
            'span multiple rows *and* columns, sorry.')
        atts = {}
        if node.has_key('morerows'):
            count = node['morerows'] + 1
            self.body.append('\\multirow{%s}*{' % count)
            self.context.append('}')
        elif node.has_key('morecols'):
            count = node['morecols'] + 1
            self.body.append('\\multicolumn{%s}{l}{' % count)
            self.context.append('}')
        else:
            self.context.append('')

        # header / not header
        if isinstance(node.parent.parent, nodes.thead):
            self.body.append('\\textbf{')
            self.context.append('}')
        else:
            self.context.append('')

    def depart_entry(self, node):
        self.body.append(self.context.pop()) # header / not header
        self.body.append(self.context.pop()) # multirow/column
        self.context[-1] += 1

    def visit_enumerated_list(self, node):
        self.body.append('\\begin{enumerate}\n')

    def depart_enumerated_list(self, node):
        self.body.append('\\end{enumerate}\n')

    def visit_error(self, node):
        self.visit_admonition(node, 'error')

    def depart_error(self, node):
        self.depart_admonition()

    def visit_field(self, node):
        self.body.append('%[visit_field]\n')

    def depart_field(self, node):
        self.body.append('\n')
        ##self.body.append('%[depart_field]\n')

    def visit_field_argument(self, node):
        self.body.append('%[visit_field_argument]\n')

    def depart_field_argument(self, node):
        self.body.append('%[depart_field_argument]\n')

    def visit_field_body(self, node):
        # BUG by attach as text we loose references.
        if self.docinfo:
            self.docinfo.append('%s \\\\\n' % node.astext())
            raise nodes.SkipNode
        # what happens if not docinfo

    def depart_field_body(self, node):
        self.body.append('%[depart_field_body]\n')

    def visit_field_list(self, node):
        self.body.append('%[visit_field_list]\n')
        ##self.body.append('\\begin{description}\n')
        pass

    def depart_field_list(self, node):
        ##self.body.append('\\end{description}\n')
        pass

    def visit_field_name(self, node):
        # BUG this duplicates docinfo_item
        self.body.append('%[visit_field_item "'+ node.astext() +'"]\n')
        if self.docinfo:
            s = self.language_label(node.astext())
            self.docinfo.append('\\textbf{%s} &\n\t' % s)
            raise nodes.SkipNode
        else:
            self.body.append('\\textbf{')

    def depart_field_name(self, node):
        if not self.docinfo:
            self.body.append(':}')

    def visit_figure(self, node):
        self.body.append( '\\begin{figure}\n' )

    def depart_figure(self, node):
        self.body.append( '\\end{figure}\n' )

    def visit_footer(self, node):
        self.context.append(len(self.body))

    def depart_footer(self, node):
        start = self.context.pop()
        footer = (['\n\\begin{center}\small\n']
                  + self.body[start:] + ['\n\\end{center}\n'])
        self.body_suffix[:0] = footer
        del self.body[start:]

    def visit_footnote(self, node):
        notename = node['id']
        self.body.append('\\begin{figure}[b]')
        self.body.append('\\hypertarget{%s}' % notename)

    def depart_footnote(self, node):
        self.body.append('\\end{figure}\n')

    def visit_footnote_reference(self, node):
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
            self.context.append('???')
        self.body.append('%s\\href{%s}{' % (suffix,href))

    def depart_footnote_reference(self, node):
        self.body.append('}%s' % self.context.pop())

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

    def visit_header(self, node):
        self.context.append(len(self.body))

    def depart_header(self, node):
        start = self.context.pop()
        self.body_prefix.append('\n\\verb|begin_header|\n')
        ##self.body_prefix.append(self.starttag(node, 'div', CLASS='header'))
        self.body_prefix.extend(self.body[start:])
        self.body_prefix.append('\n\\verb|end_header|\n')
        ##self.body_prefix.append('<hr />\n</div>\n')
        del self.body[start:]

    def visit_hint(self, node):
        self.visit_admonition(node, 'hint')

    def depart_hint(self, node):
        self.depart_admonition()

    def visit_image(self, node):
        atts = node.attributes.copy()
        href = atts['uri']
        ##self.body.append('\\begin{center}\n')
        self.body.append('\\includegraphics{%s}\n' % href)
        ##self.body.append('\\end{center}\n')

    def depart_image(self, node):
        pass

    def visit_important(self, node):
        self.visit_admonition(node, 'important')

    def depart_important(self, node):
        self.depart_admonition()

    def visit_interpreted(self, node):
        # @@@ Incomplete, pending a proper implementation on the
        # Parser/Reader end.
        self.visit_literal(node)

    def depart_interpreted(self, node):
        self.depart_literal(node)

    def visit_label(self, node):
        # footnote label
        self.body.append('[')

    def depart_label(self, node):
        self.body.append(']')

    def visit_legend(self, node):
        self.body.append('{\\small ')

    def depart_legend(self, node):
        self.body.append('}')

    def visit_line_block(self, node):
        self.visit_literal_block(node)

    def depart_line_block(self, node):
        self.depart_literal_block(node)

    def visit_list_item(self, node):
        self.body.append('\\item ')

    def depart_list_item(self, node):
        self.body.append('\n')

    def visit_literal(self, node):
        self.body.append('\\texttt{')

    def depart_literal(self, node):
        self.body.append('}')

    def visit_literal_block(self, node):
        self.use_verbatim_for_literal = 1
        if (self.use_verbatim_for_literal):
            self.body.append('\\begin{verbatim}\n')
            self.body.append(node.astext())
            self.body.append('\n\\end{verbatim}\n')
            raise nodes.SkipNode
        else:
            self.body.append('{\\obeylines\\obeyspaces\\ttfamily\n')

    def depart_literal_block(self, node):
        if not self.use_verbatim_for_literal:
            self.body.append('}\n')

    def visit_meta(self, node):
        self.body.append('[visit_meta]\n')
        self.head.append(self.starttag(node, 'meta', **node.attributes))

    def depart_meta(self, node):
        self.body.append('[depart_meta]\n')

    def visit_note(self, node):
        self.visit_admonition(node, 'note')

    def depart_note(self, node):
        self.depart_admonition()

    def visit_option(self, node):
        if self.context[-1]:
            self.body.append(', ')

    def depart_option(self, node):
        self.context[-1] += 1

    def visit_option_argument(self, node):
        self.body.append(node.get('delimiter', ' '))

    def depart_option_argument(self, node):
        pass

    def visit_option_group(self, node):
        atts = {}
        self.body.append('\\texttt{')
        self.context.append('')
        self.context.append(0)

    def depart_option_group(self, node):
        self.context.pop()
        self.body.append('}')
        self.body.append(self.context.pop())

    def visit_option_list(self, node):
        self.body.append('% option list\n')
        self.body.append('\\begin{center}\n')
        self.body.append('\\begin{tabular}{ll}\n')

    def depart_option_list(self, node):
        self.body.append('\\end{tabular}\n')
        self.body.append('\\end{center}\n')

    def visit_option_list_item(self, node):
        pass

    def depart_option_list_item(self, node):
        self.body.append('\\\\\n')

    def visit_option_string(self, node):
        ##self.body.append(self.starttag(node, 'span', '', CLASS='option'))
        pass

    def depart_option_string(self, node):
        ##self.body.append('</span>')
        pass

    def visit_organization(self, node):
        self.visit_docinfo_item(node, 'organization')

    def depart_organization(self, node):
        self.depart_docinfo_item(node)

    def visit_paragraph(self, node):
        if not self.topic_class == 'contents':
            self.body.append('\n')

    def depart_paragraph(self, node):
        if self.topic_class == 'contents':
            self.body.append('\n')
        else:
            self.body.append('\n')

    def visit_problematic(self, node):
        ##if node.hasattr('refid'):
        ##   self.body.append('<a href="#%s">' % node['refid'])
        ##   self.context.append('</a>')
        ##else:
        ##   self.context.append('')
        ##self.body.append(self.starttag(node, 'span', '', CLASS='problematic'))
        self.body.append('{\\color{red}\\bfseries{}')

    def depart_problematic(self, node):
        ##self.body.append('</span>')
        ##self.body.append(self.context.pop())
        self.body.append('}')

    def visit_raw(self, node):
        if node.has_key('format') and node['format'].lower() == 'latex':
            self.body.append(node.astext())
        raise nodes.SkipNode

    def visit_reference(self, node):
        # for pdflatex hyperrefs might be supported
        if node.has_key('refuri'):
            href = node['refuri']
        elif node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        ##self.body.append('[visit_reference]')
        self.body.append('\\href{%s}{' % href)

    def depart_reference(self, node):
        self.body.append('}')
        ##self.body.append('[depart_reference]')

    def visit_revision(self, node):
        self.visit_docinfo_item(node, 'revision')

    def depart_revision(self, node):
        self.depart_docinfo_item(node)

    def visit_row(self, node):
        ##self.body.append(self.starttag(node, 'tr', ''))
        self.context.append(0)
        pass

    def depart_row(self, node):
        self.context.pop()  # remove cell counter
        self.body.append(' \\\\ \\hline\n')

    def visit_section(self, node):
        self.section_level += 1

    def depart_section(self, node):
        self.section_level -= 1

    def visit_status(self, node):
        self.visit_docinfo_item(node, 'status')

    def depart_status(self, node):
        self.depart_docinfo_item(node)

    def visit_strong(self, node):
        self.body.append('\\textbf{')

    def depart_strong(self, node):
        self.body.append('}')

    def visit_substitution_definition(self, node):
        raise nodes.SkipNode

    def visit_substitution_reference(self, node):
        self.unimplemented_visit(node)

    def visit_subtitle(self, node):
        self.head.append('\\subtitle{%s}\n' %
                self.encode(node.astext()) )
        raise nodes.SkipNode

    def depart_subtitle(self, node):
        pass

    def visit_system_message(self, node):
        if node['level'] < self.document.reporter['writer'].report_level:
            raise nodes.SkipNode


    def depart_system_message(self, node):
        self.body.append('\n')

    def visit_table(self, node):
        self.body.append('\n\\begin{tabular}')
        self.context.append('table_sentinel') # sentinel
        self.context.append(0) # column counter

    def depart_table(self, node):
        self.body.append('\\end{tabular}\n')
        sentinel = self.context.pop()
        if sentinel != 'table_sentinel':
            print 'context:', self.context + [sentinel]
            raise AssertionError

    def table_preamble(self):
        if self.context[-1] != 'table_sentinel':
            self.body.append('{%s}' % ('|l' * self.context.pop() + '|'))
            self.body.append('\n\\hline')

    def visit_target(self, node):
        if not (node.has_key('refuri') or node.has_key('refid')
                or node.has_key('refname')):
            ##self.body.append(self.starttag(node, 'a', '', CLASS='target'))
            ##self.body.append(str(node))
            self.body.append('\\hypertarget{%s}{' % node['name'])
            self.context.append('}')
        else:
            self.context.append('')

    def depart_target(self, node):
        self.body.append(self.context.pop())

    def visit_tbody(self, node):
        self.table_preamble()
        self.body.append('%[visit_tbody]\n')
        pass

    def depart_tbody(self, node):
        self.body.append('%[depart_tbody]\n')
        pass

    def visit_term(self, node):
        ##self.body.append(self.starttag(node, 'dt', ''))
        self.body.append('\\item[')

    def depart_term(self, node):
        self.body.append(']\n')
        pass

    def visit_tgroup(self, node):
        #self.body.append(self.starttag(node, 'colgroup'))
        #self.context.append('</colgroup>\n')
        pass

    def depart_tgroup(self, node):
        pass

    def visit_thead(self, node):
        self.table_preamble()
        self.body.append('%[visit_thead]\n')

    def depart_thead(self, node):
        self.body.append('%[depart_thead]\n')

    def visit_tip(self, node):
        self.visit_admonition(node, 'tip')

    def depart_tip(self, node):
        self.depart_admonition()

    def visit_title(self, node):
        """Only 3 section levels are supported by LaTeX article (AFAIR)."""
        if isinstance(node.parent, nodes.topic):
            self.body.append('% topic title\n')
            if node.parent.hasattr('id'):
                self.body.append('\\hypertarget{%s}{}' % node.parent['id'])
            self.body.append('\\paragraph{')
        elif self.section_level == 0:
            # document title
            self.head.append('\\title{%s}\n' % self.encode(node.astext()))
            if not self.pdfinfo == None:
                self.pdfinfo.append( 'pdftitle={%s}' % self.encode(node.astext()) )
            raise nodes.SkipNode
        else:
            name = None
            if node.parent.hasattr('id'):
                name = node.parent['id']
            self.body.append('\n\n')
            self.body.append('%' + '_' * 75)
            self.body.append('\n\n')
            if name:
                self.body.append( '\\hypertarget{%s}{}\n' % name)
            self.body.append('\\%ssection*{' % ('sub'*(self.section_level-1)))
            # BUG: self.body.append( '\\label{%s}\n' % name)
        self.context.append('}\n')

    def depart_title(self, node):
        self.body.append(self.context.pop())
        # BUG level depends on style.
        if node.parent.hasattr('id'):
            self.body.append('\\pdfbookmark[%d]{%s}{%s}\n' % \
                (self.section_level-1,node.astext(),node.parent['id']))

    def visit_topic(self, node):
        ##self.body.append('% [visit_topic]\n')
        self.topic_class = node.get('class')
        if self.latex_toc:
            ##self.body.append('% [depart_topic]\n')
            self.topic_class = ''
            raise nodes.SkipNode

    def depart_topic(self, node):
        if not self.latex_toc:
            self.body.append('% [depart_topic]\n')
        self.topic_class = ''

    def visit_transition(self, node):
        self.body.append('\n\n')
        self.body.append('%' + '_' * 75)
        self.body.append('\n\\hspace*{\\fill}\\hrulefill\\hspace*{\\fill}')
        self.body.append('\n\n')

    def depart_transition(self, node):
        #self.body.append('[depart_transition]')
        pass

    def visit_version(self, node):
        self.visit_docinfo_item(node, 'version')

    def depart_version(self, node):
        self.depart_docinfo_item(node)

    def visit_warning(self, node):
        self.visit_admonition(node, 'warning')

    def depart_warning(self, node):
        self.depart_admonition()

    def unimplemented_visit(self, node):
        raise NotImplementedError('visiting unimplemented node type: %s'
                                  % node.__class__.__name__)

#    def unknown_visit(self, node):
#    def default_visit(self, node):
    
# vim: set ts=4 et ai :
