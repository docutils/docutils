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


import sys
import time
import re
from types import ListType
from docutils import writers, nodes, languages


class Writer(writers.Writer):

    supported = ('latex','latex2e')
    """Formats this writer supports."""

    cmdline_options = (
        'LaTeX-Specific Options',
        None,
        (('Specify documentclass.  Default is "article".',
          ['--documentclass'],
          {'default': 'article', }),))

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


class LaTeXTranslator(nodes.NodeVisitor):
    # dummy settings might be taken from document options
    d_class = 'article'    # document.options.stylesheet
    d_options = 'twoside'  # papersize, fontsize
    d_paper = 'a4paper'
    d_margins = '2cm'
    # for pdflatex some other package. pslatex

    latex_head = '\\documentclass[%s]{%s}\n'
    encoding = '\\usepackage[latin1]{inputenc}\n'
    linking = '\\usepackage[colorlinks]{hyperref}\n'
    geometry = '\\usepackage[%s,margin=%s]{geometry}\n'
    fonts = '\\usepackage{%s}\n'
    stylesheet = '\\input{%s}\n'
    # content type might be outputenc ?
    # content_type = '<meta http-equiv="Content-Type" content="text/html; ' \
    #               'charset=%s">\n'
    # add a generated on day , machine by user using docutils versoin.
    generator = '%% generator Docutils: http://docutils.sourceforge.net/\n'

    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.language = languages.get_language(document.options.language_code)
        self.head_prefix = [
              self.latex_head % (self.d_options,self.d_class),
              self.encoding,
              self.linking,
              self.geometry % (self.d_paper, self.d_margins),
              self.fonts % "palatino",
              self.stylesheet % "style.tex",
              "%s\n" % '\\usepackage{graphicx}',
              # language for hyphenation , usepackage[]{babel}
              #self.content_type % document.options.output_encoding,
              self.generator,
               ]

        if not document.options.datestamp:
            self.head_prefix.append("\\date{}\n")
        # else date is printed automatically in title.
        # what about author
        self.head = []
        self.body_prefix = ['\n']
        self.body = []
        self.body_suffix = ['\n']
        self.section_level = 0
        self.context = []
        self.topic_class = ''

    def encode(self, text):
        """Encode special characters in `text` & return."""

        text = text.replace("\\", '{\\\\}')
        text = text.replace("&", '{\\&}')
        text = text.replace("_", '{\\_}')
        text = text.replace("#", '{\\#}')
        text = text.replace("$", '{\\$}')
        text = text.replace("^", '{\\^}')
        return text

    def attval(self, text,
               whitespace=re.compile('[\n\r\t\v\f]')):
        """Cleanse, HTML encode, and return attribute value text."""
        return self.encode(whitespace.sub(' ', text))

    def astext(self):
        return ''.join(self.head_prefix + self.head
               + self.body_prefix + self.body + self.body_suffix)

    def visit_Text(self, node):
        self.body.append(self.encode(node.astext()))

    def depart_Text(self, node):
        pass

    def visit_address(self, node):
        self.visit_docinfo_item(node, 'address')

    def depart_address(self, node):
        self.depart_docinfo_item(node)

    def visit_admonition(self, node, name):
        self.body.append('\\paragraph{'
                         + self.language.labels[name] + '}\n{')

    def depart_admonition(self):
        self.body.append('}\n')

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

    def visit_author(self, node):
        self.visit_docinfo_item(node, 'author')

    def depart_author(self, node):
        self.depart_docinfo_item(node)

    def visit_authors(self, node):
        pass

    def depart_authors(self, node):
        pass

    def visit_block_quote(self, node):
        self.body.append( '\\begin{quote}\n')

    def depart_block_quote(self, node):
        self.body.append( '\\end{quote}\n')

    def visit_bullet_list(self, node):
        if self.topic_class == 'contents':
            # should do nothing: contents is handled by latex.
            pass
            #self.body.append( '\\begin{itemize}\n' )
        else:
            self.body.append( '\\begin{itemize}\n' )

    def depart_bullet_list(self, node):
        if self.topic_class != 'contents':
            # should do nothing: contents is handled by latex.
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
        self.body.append( '--begin--citation--\n\n' )
        #self.body.append(self.starttag(node, 'table', CLASS='citation',
        #                               frame="void", rules="none"))
        #self.body.append('<col class="label" />\n'
        #                 '<col />\n'
        #                 '<tbody valign="top">\n'
        #                 '<tr><td>\n')

    def depart_citation(self, node):
        self.body.append( '--end--citation--\n\n' )
        #self.body.append('</td></tr>\n'
        #                 '</tbody>\n</table>\n')

    def visit_citation_reference(self, node):
        href = ''
        if node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        self.body.append( ' [' )
        #self.body.append(self.starttag(node, 'a', '[', href=href,
        #                               CLASS='citation-reference'))

    def depart_citation_reference(self, node):
        self.body.append( '] ' )

    def visit_classifier(self, node):
        self.body.append( '{\\bfseries :' )
        #self.body.append(' <span class="classifier-delimiter">:</span> ')
        #self.body.append(self.starttag(node, 'span', '', CLASS='classifier'))

    def depart_classifier(self, node):
        self.body.append( '}\n' )

    def visit_colspec(self, node):
        #atts = {}
        # @@@ colwidth attributes don't seem to work well in HTML
        #if node.has_key('colwidth'):
        #    atts['width'] = str(node['colwidth']) + '*'
        #self.body.append(self.emptytag(node, 'col', **atts))
        raise NotImplementedError

    def depart_colspec(self, node):
        pass

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

    def visit_decoration(self, node):
        pass

    def depart_decoration(self, node):
        pass

    def visit_definition(self, node):
        #self.body.append('</dt>\n')
        #self.body.append(self.starttag(node, 'dd'))
        pass

    def depart_definition(self, node):
        self.body.append('\n')
        #self.body.append('</dd>\n')

    def visit_definition_list(self, node):
        self.body.append( '\\begin{description}\n' )
        #self.body.append(self.starttag(node, 'dl'))

    def depart_definition_list(self, node):
        self.body.append( '\\end{description}\n' )
        #self.body.append('</dl>\n')

    def visit_definition_list_item(self, node):
        pass

    def depart_definition_list_item(self, node):
        pass

    def visit_description(self, node):
        self.body.append( '\\item[' )
        #self.body.append('<td>\n')

    def depart_description(self, node):
        self.body.append( ']\n\t' )
        #self.body.append('</td>')

    def visit_docinfo(self, node):
        self.docinfo = []
        self.docinfo.append('%' + '_'*75 + '\n')
        self.docinfo.append('\\begin{tabular}{l l}\n')

    def depart_docinfo(self, node):
        self.docinfo.append('\\end{tabular}\n')
        self.body = self.docinfo + self.body

    def visit_docinfo_item(self, node, name):
        # should we stick to latex or docutils.
        # latex article has its own handling of date and author
        latex_docinfo = 0
        if name == 'abstract':
            # NOTE tableofcontents before or after ? 
            # eg after: see visit_topic
            # NOTE this limits abstract to text.
            self.body.append('\\begin{abstract}\n')
            self.context.append('\\end{abstract}\n')
            self.context.append(self.body)
            self.context.append(len(self.body))
        elif latex_docinfo and name == 'date':
            # BUG: time portion is not included in date 
            self.head.append('\\date{%s}\n' % self.attval(node.astext()))
            raise nodes.SkipNode
        elif latex_docinfo and name == 'author':
            self.head.append('\\author{%s}\n' % self.attval(node.astext()))
            raise nodes.SkipNode
        else:
            # BUG: i donot get Web-site in here, no docinfo item
            #self.head.append('\\%s{%s}\n'
            #             % (name, self.attval(node.astext())))
            self.docinfo.append('\\textbf{%s:} &\n\t' % name)
            self.context.append(' \\\\\n')
            self.context.append(self.docinfo)
            self.context.append(len(self.body))
            #self.context.append('')
            #self.context.append(self.body)
            #raise nodes.SkipNode

    def depart_docinfo_item(self, node):
        size = self.context.pop()
        dest = self.context.pop()
        tail = self.context.pop()
        tail = self.body[size:] + [tail]
        del self.body[size:]
        dest.extend(tail)

    def visit_doctest_block(self, node):
        self.body.append( '\\begin{verbatim}' )
        #self.body.append(self.starttag(node, 'pre', suffix='',
        #                               CLASS='doctest-block'))

    def depart_doctest_block(self, node):
        self.body.append( '\\end{verbatim}\n' )
        #self.body.append('</pre>\n')

    def visit_document(self, node):
        self.body_prefix.append('\\begin{document}\n')
        self.body_prefix.append('\\maketitle\n\n')

    def depart_document(self, node):
        self.body_suffix.append('\\end{document}\n')

    def visit_emphasis(self, node):
        self.body.append('\\emph{')

    def depart_emphasis(self, node):
        self.body.append('}')

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
        self.body.append(self.starttag(node, tagname, **atts))
        self.context.append('</%s>' % tagname.lower())
        if len(node) == 0:              # empty cell
            self.body.append('&nbsp;')

    def depart_entry(self, node):
        self.body.append(self.context.pop())

    def visit_enumerated_list(self, node):
        #"""
        #The 'start' attribute does not conform to HTML 4.01's strict.dtd, but
        #CSS1 doesn't help. CSS2 isn't widely enough supported yet to be
        #usable.
        #"""
        #atts = {}
        #if node.has_key('start'):
        #    atts['start'] = node['start']
        #if node.has_key('enumtype'):
        #    atts['class'] = node['enumtype']
        # @@@ To do: prefix, suffix. How? Change prefix/suffix to a
        # single "format" attribute? Use CSS2?
        #self.body.append(self.starttag(node, 'ol', **atts))
        self.body.append('\\begin{enumerate}\n')

    def depart_enumerated_list(self, node):
        self.body.append('\\end{enumerate}\n')
        #self.body.append('</ol>\n')

    def visit_error(self, node):
        self.visit_admonition(node, 'error')

    def depart_error(self, node):
        self.depart_admonition()

    def visit_field(self, node):
        #self.body.append('\n\\verb|visit_field|\n')
        #self.body.append(self.starttag(node, 'tr', CLASS='field'))
        pass

    def depart_field(self, node):
        #self.body.append('\n\\verb|depart_field|\n')
        #self.body.append('</tr>\n')
        pass

    def visit_field_argument(self, node):
        #self.body.append('\n\\verb|visit_field_argument|\n')
        pass

    def depart_field_argument(self, node):
        #self.body.append('\n\\verb|depart_field_argument|\n')
        pass

    def visit_field_body(self, node):
        #self.body.append('\n\\verb|visit_field_body|\n')
        pass

    def depart_field_body(self, node):
        #self.body.append('\n\\verb|depart_field_body|\n')
        pass

    def visit_field_list(self, node):
        #self.body.append('\n\\verb|visit_field_list|\n')
        pass

    def depart_field_list(self, node):
        #self.body.append('\n\\verb|depart_field_list|\n')
        pass

    def visit_field_name(self, node):
        self.body.append('\\textbf{:')
        #self.body.append('\n\\verb|visit_field_name|\n')

    def depart_field_name(self, node):
        self.body.append(':}')
        #self.body.append('\n\\verb|depart_field_name|\n')

    def visit_figure(self, node):
        self.body.append( '\\begin{figure}\n' )
        #self.body.append(self.starttag(node, 'div', CLASS='figure'))

    def depart_figure(self, node):
        self.body.append( '\\end{figure}\n' )
        #self.body.append('</div>\n')

    def visit_footer(self, node):
        self.context.append(len(self.body))

    def depart_footer(self, node):
        start = self.context.pop()
        footer = (['\n\\begin{center}\small\n']
                  + self.body[start:] + ['\n\\end{center}\n'])
        self.body_suffix[:0] = footer
        del self.body[start:]

    def visit_footnote(self, node):
        self.body.append('\\footnote{')

    def depart_footnote(self, node):
        self.body.append('}')

    def visit_footnote_reference(self, node):
        pass

    def depart_footnote_reference(self, node):
        pass
        # self.body.append('}')

    def visit_header(self, node):
        self.context.append(len(self.body))

    def depart_header(self, node):
        start = self.context.pop()
        self.body_prefix.append('\n\\verb|begin_header|\n')
        #self.body_prefix.append(self.starttag(node, 'div', CLASS='header'))
        self.body_prefix.extend(self.body[start:])
        self.body_prefix.append('\n\\verb|end_header|\n')
#        self.body_prefix.append('<hr />\n</div>\n')
        del self.body[start:]

    def visit_hint(self, node):
        self.visit_admonition(node, 'hint')

    def depart_hint(self, node):
        self.depart_admonition()

    def visit_image(self, node):
        atts = node.attributes.copy()
        href = atts['uri']
        self.body.append('\\begin{center}\n')
        self.body.append('\\includegraphics{%s}' % href)
        self.body.append('\\end{center}\n')
        #self.body.append(self.emptytag(node, 'img', '', **atts))

    def depart_image(self, node):
        pass

    def visit_important(self, node):
        self.visit_admonition(node, 'important')

    def depart_important(self, node):
        self.depart_admonition()

    def visit_interpreted(self, node):
        # @@@ Incomplete, pending a proper implementation on the
        # Parser/Reader end.
        #self.body.append('<span class="interpreted">')
        self.visit_literal(node)

    def depart_interpreted(self, node):
        #self.body.append('</span>')
        self.depart_literal(node)

    def visit_label(self, node):
        self.body.append('\n[')
        #self.body.append(self.starttag(node, 'p', '[', CLASS='label'))

    def depart_label(self, node):
        self.body.append(']\n\n')
        #self.body.append(']</p>\n'
        #                 '</td><td>\n')

    def visit_legend(self, node):
        self.body.append('{\\small ')
        #self.body.append(self.starttag(node, 'div', CLASS='legend'))

    def depart_legend(self, node):
        self.body.append('}')
        #self.body.append('</div>\n')

    def visit_list_item(self, node):
        self.body.append('\\item ')
        #self.body.append(self.starttag(node, 'li'))

    def depart_list_item(self, node):
        self.body.append('\n\n')
        #self.body.append('</li>\n')

    def visit_literal(self, node):
        self.body.append('\\texttt{')
        #self.body.append('<code>')

    def depart_literal(self, node):
        self.body.append('}')
        #self.body.append('</code>')

    def visit_literal_block(self, node):
        # BUG verbatim does not work because astext does encode.
        # BUG obeyspaces seams to not care about starting spaces.
        self.body.append('{\\obeylines\\obeyspaces\\ttfamily\n')
        #self.body.append('\\begin{verbatim}')

    def depart_literal_block(self, node):
        self.body.append('}\n')
        #self.body.append('\\end{verbatim}\n')

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
        self.body.append(self.starttag(node, 'span', '',
                                       CLASS='option-argument'))

    def depart_option_argument(self, node):
        self.body.append('</span>')

    def visit_option_group(self, node):
        atts = {}
        if len(node.astext()) > 14:
            atts['colspan'] = 2
            self.context.append('</tr>\n<tr><td>&nbsp;</td>')
        else:
            self.context.append('')
        self.body.append(self.starttag(node, 'td', **atts))
        self.body.append('<p><code>')
        self.context.append(0)

    def depart_option_group(self, node):
        self.context.pop()
        self.body.append('</code></p>\n</td>')
        self.body.append(self.context.pop())

    def visit_option_list(self, node):
        self.body.append(
              self.starttag(node, 'table', CLASS='option-list',
                            frame="void", rules="none", cellspacing=12))
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
        if node.hasattr('refid'):
            self.body.append('<a href="#%s">' % node['refid'])
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
        # for pdflatex hyperrefs might be supported
        if node.has_key('refuri'):
            href = node['refuri']
        elif node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        href = href.replace('#','_')
        #self.body.append('[visit_reference]')
        self.body.append('\\href{%s}{' % href)
        #self.body.append(self.starttag(node, 'a', '', href=href,
        #                               CLASS='reference'))
        pass

    def depart_reference(self, node):
        self.body.append('}')
        #self.body.append('[depart_reference]')
        pass

    def visit_revision(self, node):
        self.visit_docinfo_item(node, 'revision')

    def depart_revision(self, node):
        self.depart_docinfo_item(node)

    def visit_row(self, node):
        self.body.append(self.starttag(node, 'tr', ''))

    def depart_row(self, node):
        self.body.append('</tr>\n')

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
        self.body.append(self.starttag(node, 'div', CLASS='system-message'))
        self.body.append('<p class="system-message-title">')
        if node.hasattr('backrefs'):
            backrefs = node['backrefs']
            if len(backrefs) == 1:
                self.body.append('<a href="#%s">%s</a> '
                                 '(level %s system message)</p>\n'
                                 % (backrefs[0], node['type'], node['level']))
            else:
                i = 1
                backlinks = []
                for backref in backrefs:
                    backlinks.append('<a href="#%s">%s</a>' % (backref, i))
                    i += 1
                self.body.append('%s (%s; level %s system message)</p>\n'
                                 % (node['type'], '|'.join(backlinks),
                                    node['level']))
        else:
            self.body.append('%s (level %s system message)</p>\n'
                             % (node['type'], node['level']))

    def depart_system_message(self, node):
        self.body.append('</div>\n')

    def visit_table(self, node):
        self.body.append('\\begin{verbatim}\n')

    def depart_table(self, node):
        self.body.append('\\end{verbatim}\n')

    def visit_target(self, node):
        if not (node.has_key('refuri') or node.has_key('refid')
                or node.has_key('refname')):
            #self.body.append(self.starttag(node, 'a', '', CLASS='target'))
            #self.body.append(str(node))
            self.body.append('\\hypertarget{%s}{' % node['name'])
            self.context.append('}')
        else:
            self.context.append('')

    def depart_target(self, node):
        self.body.append(self.context.pop())

    def visit_tbody(self, node):
        self.body.append(self.context.pop()) # '</colgroup>\n' or ''
        self.body.append(self.starttag(node, 'tbody', valign='top'))

    def depart_tbody(self, node):
        self.body.append('</tbody>\n')

    def visit_term(self, node):
        self.body.append('\\item[')
        #self.body.append(self.starttag(node, 'dt', ''))

    def depart_term(self, node):
        self.body.append(']\n')
        pass

    def visit_tgroup(self, node):
        self.body.append(self.starttag(node, 'colgroup'))
        self.context.append('</colgroup>\n')

    def depart_tgroup(self, node):
        pass

    def visit_thead(self, node):
        self.body.append(self.context.pop()) # '</colgroup>\n'
        self.context.append('')
        self.body.append(self.starttag(node, 'thead', valign='bottom'))

    def depart_thead(self, node):
        self.body.append('</thead>\n')

    def visit_tip(self, node):
        self.visit_admonition(node, 'tip')

    def depart_tip(self, node):
        self.depart_admonition()

    def visit_title(self, node):
        """Only 3 section levels are supported by LaTeX article (AFAIR)."""
        if isinstance(node.parent, nodes.topic):
            #self.body.append(
            #      self.starttag(node, 'p', '', CLASS='topic-title'))
            self.body.append('\n% topic title\n\\paragraph{')
        elif self.section_level == 0:
            # document title
            self.head.append('\\title{%s}\n' % self.encode(node.astext()))
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
            self.body.append( '\\%ssection{' % ('sub'*(self.section_level-1)) )
        self.context.append('}\n')

    def depart_title(self, node):
        self.body.append(self.context.pop())

    def visit_topic(self, node):
        self.body.append('% [visit_topic]\n')
        self.body.append('\\tableofcontents\n\n\\bigskip\n')
        self.body.append('% [depart_topic]\n')
        self.topic_class = ''
        raise nodes.SkipNode

    def depart_topic(self, node):
        self.body.append('% [depart_topic]\n')
        self.topic_class = ''

    def visit_transition(self, node):
        self.body.append('\n\n')
        self.body.append('%' + '_' * 75)
        self.body.append('\n\\hspace*{\\fill}\\hrulefill\\hspace*{\\fill}')
        self.body.append('\n\n')
        #self.body.append(self.emptytag(node, 'hr'))

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

# vim: set ts=4 et ai:
