#!/usr/bin/env python

"""
:Author: Dave Kuhlman
:Contact: dkuhlman@rexx.com
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
import urllib, urlparse
from docutils import writers, nodes, languages

## from IPython.Shell import IPShellEmbed
## args = ''
## ipshell = IPShellEmbed(args,
##     banner = 'Dropping into IPython',
##     exit_msg = 'Leaving Interpreter, back to program.')


# To turn off debug printing, set to 0.
DEBUG_FLAG = 1

# Constants and globals:

COMMENT_RE_SUB = re.compile('\n').sub
PROGRAMOPT_RE = re.compile('(--[_\-a-zA-Z0-9]+)')
EMAILADDR_RE = re.compile(r'[_\-a-zA-Z0-9.]+@[_\-a-zA-Z0-9.]+')
WEBADDR_RE = re.compile(r'http://[_\-a-zA-Z0-9./~]+')

TABLE_MODE_NONE = 0
TABLE_MODE_HEAD = 1
TABLE_MODE_BODY = 2

class TableSpec:
    def __init__(self):
        self.columnCount = 0
        self.mode = TABLE_MODE_NONE
    def getColumnCount(self): return self.columnCount
    def setColumnCount(self, columnCount): self.columnCount = columnCount
    def getMode(self): return self.mode
    def setMode(self, mode): self.mode = mode


class Writer(writers.Writer):

    supported = ('latex','latex2e')
    """Formats this writer supports."""

    settings_spec = (
        'Documenting Python LaTeX Specific Options',
        'The LaTeX "--output-encoding" default is "latin-1:strict".',
        (('Specify documentclass (one of howto, manual, module).  Default is "howto".',
#        (('Specify documentclass (one of howto, manual).  Default is "howto".',
          ['--documentclass'],
          {'default': 'howto', }),
    ))

    #settings_defaults = {}
    settings_defaults = {'output_encoding': 'latin-1'}

    output = None
    """Final translated form of `document`."""

    def translate(self):
        visitor = DocPyTranslator(self.document)
        self.document.walkabout(visitor)
        #import pdb
        #pdb.run('self.document.walkabout(visitor)', globals(), locals())
        self.output = visitor.astext()


class DocPyTranslator(nodes.NodeVisitor):
    latex_head = '\\documentclass{%s}\n'
    # add a generated on day , machine by user using docutils version.
    generator = '\n%% generator -- Docutils: http://docutils.sourceforge.net/\n' \
        '%% writer -- documenting_python\n' \
        '%% generated on -- %s\n\n' % time.ctime()

    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.docinfo = {}
        self.settings = settings = document.settings
        if self.settings.documentclass == 'module':
            raise NotImplementedError, 'Error: module document type not yet implemented.'
        self.head_prefix = [
              self.latex_head % (self.settings.documentclass, ),
              #self.encoding,
              #self.linking % (self.colorlinks, self.hyperlink_color, self.hyperlink_color),
              # geometry and fonts might go into style.tex.
              #self.geometry % (self.d_paper, self.d_margins),
              #
              self.generator,
              '\\usepackage{html}\n',
              ]

        # NOTE: Latex wants a date and an author, rst puts this into
        #   docinfo, so normally we donot want latex author/date handling.
        # latex article has its own handling of date and author, deactivate.
        self.latex_docinfo = 0
        self.head = [ ]
        self.body_prefix = []
        # separate title, so we can appen subtitle.
        self.title = ""
        self.body = []
        self.body_suffix = ['\n']
        self.section_level = 0
        self.context = []
        self.topic_class = ''
        # Flags to encode
        # ---------------
        # verbatim: to tell encode not to encode.
        self.verbatim = 0
        # insert_newline: to tell encode to replace blanks by "~".
        self.insert_none_breaking_blanks = 0
        # insert_newline: to tell encode to add latex newline.
        self.insert_newline = 0
        # mbox_newline: to tell encode to add mbox and newline.
        self.mbox_newline = 0

        # inside literal block: no quote mangling.
        self.literal = 0
        self.seealso = 0
        self.title_before_section = 0
        self.seen_section = 0

    def pdebug(self, msg):
        if DEBUG_FLAG >= 1:
            self.body.append(msg)
        
    def encode(self, text):
        """
        Encode special characters in `text` & return.
            # $ % & ~ _ ^ \ { }
        Escaping with a backslash does not help with backslashes, ~ and ^.

            < > are only available in math-mode (really ?)
            $ starts math- mode.
        AND quotes:
        
        """
        if self.verbatim:
            return text
        # compile the regexps once. do it here so one can see them.
        #
        # first the braces.
        if not self.__dict__.has_key('encode_re_braces'):
            self.encode_re_braces = re.compile(r'([{}])')
        text = self.encode_re_braces.sub(r'{\\\1}',text)
        if not self.__dict__.has_key('encode_re_bslash'):
            # find backslash: except in the form '{\{}' or '{\}}'.
            self.encode_re_bslash = re.compile(r'(?<!{)(\\)(?![{}]})')
        # then the backslash: except in the form from line above:
        # either '{\{}' or '{\}}'.
        text = self.encode_re_bslash.sub(r'{\\textbackslash}', text)

        # then dollar
        text = text.replace("$", '{\\$}')
        # then all that needs math mode
        text = text.replace("<", '{$<$}')
        text = text.replace(">", '{$>$}')
        # then
        text = text.replace("&", '{\\&}')
        text = text.replace("_", '{\\_}')
        # the ^:
        # * verb|^| does not work in mbox.
        # * mathmode has wedge. hat{~} would also work.
        text = text.replace("^", '{\\ensuremath{^\\wedge}}')
        text = text.replace("%", '{\\%}')
        text = text.replace("#", '{\\#}')
        text = text.replace("~", '{\\~{}}')
        if self.insert_newline:
            # HACK: insert a blank before the newline, to avoid 
            # ! LaTeX Error: There's no line here to end.
            text = text.replace("\n", '~\\\\\n')
        elif self.mbox_newline:
            text = text.replace("\n", '}\\\\\n\\mbox{')
        if self.insert_none_breaking_blanks:
            text = text.replace(' ', '~')
        # unicode !!! 
        text = text.replace(u'\u2020', '{$\\dagger$}')
        return text

    def attval(self, text,
               whitespace=re.compile('[\n\r\t\v\f]')):
        """Cleanse, encode, and return attribute value text."""
        return self.encode(whitespace.sub(' ', text))

    def replace_email_addr(self, mo):
        addr = mo.group(0)
        outtext = '\\ulink{%s}{mailto:%s}' % (addr, addr)
        return outtext

    def replace_web_addr(self, mo):
        addr = mo.group(0)
        outtext = '\\ulink{%s}{%s}' % (addr, addr)
        return outtext

    def linkify(self, intext):
        # If it looks like an email address, convert it to a "mailto" URL.
        text1 = EMAILADDR_RE.sub(self.replace_email_addr, intext)
        # If it looks like a URL, convert it to a ulink.
        text2 = WEBADDR_RE.sub(self.replace_web_addr, text1)
        return text2
    
    def astext(self):
        title = '\\title{%s}\n' % self.title
        if self.docinfo.has_key('revision'):
            self.head.append('\\release{%s}\n' % self.docinfo['revision'])
        if self.docinfo.has_key('date'):
            self.head.append('\\date{%s}\n' % self.docinfo['date'])
        if self.docinfo.has_key('author'):
            self.head.append('\\author{%s}\n' % self.docinfo['author'])
        if self.docinfo.has_key('address'):
            self.pdebug('%% [(astext) text: %s]\n' % self.docinfo['address'])
            self.head.append('\\authoraddress{%s}\n' % \
                #self.linkify(self.cleanHref(self.docinfo['address'])))
                self.cleanHref(self.linkify(
                    self.docinfo['address'])).replace('\n', '\\\\\n'))
        self.body_prefix.append('\\maketitle\n')
        self.body_prefix.append('\\ifhtml\n')
        self.body_prefix.append('\\chapter*{Front Matter\\label{front}}\n')
        self.body_prefix.append('\\fi\n')
        if self.docinfo.has_key('copyright'):
            self.body_prefix.append('\n%s\n' % self.docinfo['copyright'])
        if self.docinfo.has_key('abstract'):
            self.body_prefix.append('\\begin{abstract}\n\\noindent\n')
            self.body_prefix.append('%s\n' % self.docinfo['abstract'])
            self.body_prefix.append('\\end{abstract}\n')
        self.body_prefix.append('\\tableofcontents\n')
        result = ''.join(
            self.head_prefix + 
            [title] +
            self.head +
            self.body_prefix +
            self.body +
            self.body_suffix)
        return result

    def visit_Text(self, node):
        self.body.append(self.encode(node.astext()))

    def depart_Text(self, node):
        pass

    def visit_address(self, node):
        self.visit_docinfo_item(node, 'address')
        raise nodes.SkipNode

    def depart_address(self, node):
        self.depart_docinfo_item(node)

    def visit_admonition(self, node, name):
        self.body.append('\\begin{center}\\begin{sffamily}\n')
        self.body.append('\\fbox{\\parbox{\\admonitionwidth}{\n')
        self.body.append('\\textbf{\\large '+ self.language.labels[name] + '}\n');
        self.body.append('\\vspace{2mm}\n')


    def depart_admonition(self):
        self.body.append('}}\n') # end parbox fbox
        self.body.append('\\end{sffamily}\n\\end{center}\n');

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

    def visit_author(self, node):
        #self.pdebug('%% [(visit_author) node: %s]\n' % str(node))
        self.visit_docinfo_item(node, 'author')
        raise nodes.SkipNode

    def depart_author(self, node):
        self.depart_docinfo_item(node)

    def visit_authors(self, node):
        # ignore. visit_author is called for each one
        # self.visit_docinfo_item(node, 'author')
        pass

    def depart_authors(self, node):
        # self.depart_docinfo_item(node)
        pass

    def visit_block_quote(self, node):
        # If the block quote contains a single object and that object
        #   is a list, then generate a list not a block quote.
        # This lets us indent lists.
        done = 0
        if len(node.children) == 1:
            child = node.children[0]
            if isinstance(child, nodes.bullet_list) or \
                    isinstance(child, nodes.enumerated_list):
                done = 1
        if not done:
            self.body.append('\\begin{quote}\n')

    def depart_block_quote(self, node):
        done = 0
        if len(node.children) == 1:
            child = node.children[0]
            if isinstance(child, nodes.bullet_list) or \
                    isinstance(child, nodes.enumerated_list):
                done = 1
        if not done:
            self.body.append('\\end{quote}\n')

    def visit_bullet_list(self, node):
        self.body.append('\\begin{itemize}\n' )

    def depart_bullet_list(self, node):
        self.body.append('\\end{itemize}\n' )

    def visit_enumerated_list(self, node):
        self.body.append('\\begin{enumerate}\n' )

    def depart_enumerated_list(self, node):
        self.body.append('\\end{enumerate}\n')

    def visit_caption(self, node):
        self.body.append('\\caption{' )

    def depart_caption(self, node):
        self.body.append('}')

    def visit_caution(self, node):
        self.visit_admonition(node, 'caution')

    def depart_caution(self, node):
        self.depart_admonition()

    def visit_citation(self, node):
        self.visit_footnote(node)

    def depart_citation(self, node):
        self.depart_footnote(node)

    def visit_title_reference(self, node):
        # BUG title-references are what?
        pass

    def depart_title_reference(self, node):
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

    def visit_comment(self, node):
        """Escape end of line by a ne comment start in comment text."""
        self.body.append('\n%% %s \n' % COMMENT_RE_SUB('\n% ', node.astext()))
        raise nodes.SkipNode

    def visit_contact(self, node):
        self.visit_docinfo_item(node, 'contact')

    def depart_contact(self, node):
        self.depart_docinfo_item(node)

    def visit_copyright(self, node):
        self.visit_docinfo_item(node, 'copyright')
        raise nodes.SkipNode

    def depart_copyright(self, node):
        self.depart_docinfo_item(node)

    def visit_danger(self, node):
        self.visit_admonition(node, 'danger')

    def depart_danger(self, node):
        self.depart_admonition()

    def visit_date(self, node):
        ## self.pdebug('%% [(visit_date) node: %s]\n' % str(node))
        self.visit_docinfo_item(node, 'date')
        raise nodes.SkipNode

    def depart_date(self, node):
        self.depart_docinfo_item(node)

    def visit_decoration(self, node):
        pass

    def depart_decoration(self, node):
        pass

    def visit_definition(self, node):
        #self.body.append('% [(visit_definition)]\n')
        pass

    def depart_definition(self, node):
        self.body.append('\n')
        #self.body.append('%[(depart_definition)]\n')

    def visit_definition_list(self, node):
        self.body.append( '\n\\begin{description}\n' )

    def depart_definition_list(self, node):
        self.body.append( '\\end{description}\n' )

    def visit_definition_list_item(self, node):
        #self.body.append('%[(visit_definition_list_item)]\n')
        pass

    def depart_definition_list_item(self, node):
        #self.body.append('%[(depart_definition_list_item)]\n')
        pass

    def visit_description(self, node):
        self.body.append( ' ' )

    def depart_description(self, node):
        pass

    def visit_docinfo(self, node):
        self.docinfo = {}

    def depart_docinfo(self, node):
        pass

    def visit_docinfo_item(self, node, name):
        self.docinfo[name] = node.astext()

    def depart_docinfo_item(self, node):
        pass

    def visit_doctest_block(self, node):
        self.body.append('\\begin{verbatim}\n' )
        self.verbatim = 1

    def depart_doctest_block(self, node):
        self.body.append( '\n\\end{verbatim}\n' )
        self.verbatim = 0

    def visit_document(self, node):
        self.body_prefix.append('\\begin{document}\n')

    def depart_document(self, node):
        if self.seealso:
            self.body.append('\\end{seealso}\n')
        self.body_suffix.append('\\end{document}\n')

    def visit_emphasis(self, node):
        self.body.append('\\emph{')

    def depart_emphasis(self, node):
        self.body.append('}')

    def visit_error(self, node):
        self.visit_admonition(node, 'error')

    def depart_error(self, node):
        self.depart_admonition()

    def visit_field(self, node):
        # real output is done in siblings: _argument, _body, _name
        raise nodes.SkipNode
        pass

    def depart_field(self, node):
        self.body.append('\n')
        ##self.body.append('%[depart_field]\n')

    def visit_field_argument(self, node):
        #self.pdebug('%% [(visit_field_argument) node: %s]\n' % str(node))
        #self.pdebug('% [visit_field_argument]\n')
        pass

    def depart_field_argument(self, node):
        #self.pdebug('% [(depart_field_argument)]\n')
        pass

    def visit_field_body(self, node):
        #self.pdebug('%% [(visit_field_body) node: %s]\n' % str(node))
        # BUG by attach as text we loose references.
##         if self.docinfo and self.current_field_name:
##             self.docinfo[self.current_field_name] += '%s \\\\\n' % node.astext()
##             raise nodes.SkipNode
        pass

    def depart_field_body(self, node):
##         self.body.append( '\n' )
        pass

    def visit_field_list(self, node):
##         if not self.docinfo:
##             self.body.append('\\begin{quote}\n')
##             self.body.append('\\begin{description}\n')
        pass

    def depart_field_list(self, node):
##         if not self.docinfo:
##             self.body.append('\\end{description}\n')
##             self.body.append('\\end{quote}\n')
        pass

    def visit_field_name(self, node):
##         self.pdebug('%% [(visit_field_name) content: %s]\n' % node.astext())
##         self.pdebug('%% [(visit_field_name) node: %s\n' % str(node))
##         self.docinfo[node.astext()] = ''
##         self.current_field_name = node.astext()
        pass

    def depart_field_name(self, node):
        pass

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
            href = node['refid']
        elif node.has_key('refname'):
            href = self.document.nameids[node['refname']]
        format = self.settings.footnote_references
        if format == 'brackets':
            suffix = '['
            self.context.append(']')
        elif format == 'superscript':
            suffix = '\\raisebox{.5em}[0em]{\\scriptsize'
            self.context.append('}')
        else:                           # shouldn't happen
            raise AssertionError('Illegal footnote reference format.')
        self.body.append('%s\\hyperlink{%s}{' % (suffix,href))

    def depart_footnote_reference(self, node):
        self.body.append('}%s' % self.context.pop())

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

    def visit_header(self, node):
        self.body.append('%% [(visit_header) node: %s\n' % str(node))
        self.context.append(len(self.body))

    def depart_header(self, node):
        start = self.context.pop()
        self.body_prefix.append('\n\\verb|begin_header|\n')
        self.body_prefix.extend(self.body[start:])
        self.body_prefix.append('\n\\verb|end_header|\n')
        del self.body[start:]

    def visit_hint(self, node):
        self.visit_admonition(node, 'hint')

    def depart_hint(self, node):
        self.depart_admonition()

    def visit_image(self, node):
        atts = node.attributes.copy()
        href = atts['uri']
        ##self.body.append('\\begin{center}\n')
        self.body.append('\n\\includegraphics{%s}\n' % href)
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
        # footnote/citation label
        self.body.append('[')

    def depart_label(self, node):
        self.body.append(']')

    def visit_legend(self, node):
        self.body.append('{\\small ')

    def depart_legend(self, node):
        self.body.append('}')

    def visit_line_block(self, node):
        """line-block: 
        * whitespace (including linebreaks) is significant 
        * inline markup is supported. 
        * serif typeface
        """
        self.body.append('\\begin{flushleft}\n')
        self.insert_none_breaking_blanks = 1
        self.line_block_without_mbox = 1
        if self.line_block_without_mbox:
            self.insert_newline = 1
        else:
            self.mbox_newline = 1
            self.body.append('\\mbox{')

    def depart_line_block(self, node):
        if self.line_block_without_mbox:
            self.insert_newline = 0
        else:
            self.body.append('}')
            self.mbox_newline = 0
        self.insert_none_breaking_blanks = 0
        self.body.append('\n\\end{flushleft}\n')

    def visit_list_item(self, node):
        self.body.append('\\item ')

    def depart_list_item(self, node):
        self.body.append('\n')

    def visit_literal(self, node):
        ## self.literal = 1
        content = node.astext().strip()
        if re.search('[ \t\n]', content):
            self.body.append('\\samp{%s}' % content)
        else:
            self.body.append('\\code{%s}' % content)
        raise nodes.SkipNode

    def depart_literal(self, node):
        pass

    def visit_literal_block(self, node):
        self.verbatim = 1
        self.body.append('\n\\begin{verbatim}\n')

    def depart_literal_block(self, node):
        self.body.append('\n\\end{verbatim}\n')
        self.verbatim = 0

    def visit_meta(self, node):
        self.body.append('[visit_meta]\n')

    def depart_meta(self, node):
        self.body.append('[depart_meta]\n')

    def visit_note(self, node):
        self.visit_admonition(node, 'note')

    def depart_note(self, node):
        self.depart_admonition()


    def visit_option(self, node):
        if self.context[-1]:
            # this is not the first option
            self.body.append(', ')

    def depart_option(self, node):
        # flag tha the first option is done.
        self.context[-1] += 1

    def visit_option_argument(self, node):
        """The delimiter betweeen an option and its argument."""
        self.body.append(node.get('delimiter', ' '))

    def depart_option_argument(self, node):
        pass

    def visit_option_group(self, node):
        self.body.append('\\item [')
        self.context.append(0)

    def depart_option_group(self, node):
        self.context.pop() # the flag
        self.body.append('] ')

    def visit_option_list(self, node):
        self.body.append('\\begin{description}\n')

    def depart_option_list(self, node):
        self.body.append('\\end{description}\n')

    def visit_option_list_item(self, node):
        pass

    def depart_option_list_item(self, node):
        pass

    def re_repl_long(self, mo):
        result = '\\longprogramopt{%s}' % mo.group(0)[2:]
        return result

    def visit_option_string(self, node):
        content = node.astext()
        content = re.sub(PROGRAMOPT_RE, self.re_repl_long, content)
        self.body.append(content)
        raise nodes.SkipNode

    def depart_option_string(self, node):
        pass

        
    def visit_organization(self, node):
        self.visit_docinfo_item(node, 'organization')

    def depart_organization(self, node):
        self.depart_docinfo_item(node)

    def visit_paragraph(self, node):
        if not self.topic_class == 'contents':
            self.body.append('\n')

    def depart_paragraph(self, node):
        self.body.append('\n')

    def visit_problematic(self, node):
        self.body.append('{\\color{red}\\bfseries{}')

    def depart_problematic(self, node):
        self.body.append('}')

    def visit_raw(self, node):
        if node.has_key('format') and node['format'].lower() == 'latex':
            self.body.append(node.astext())
        raise nodes.SkipNode

    def cleanHref(self, href):
        href = href.replace('~', '\\~{}')
        href = href.replace('#', '\\#{}')
        return href

##     def visit_reference(self, node):
##         self.pdebug('%% [(visit_reference) node: %s]\n' % str(node))
##         if node.has_key('refuri'):
##             self.href = node['refuri']
##         elif node.has_key('refid'):
##             self.href = '#' + node['refid']
##         elif node.has_key('refname'):
##             self.href = '#' + self.document.nameids[node['refname']]
##         self.href = self.cleanHref(self.href)
##         if self.seealso:
##             self.body.append('\\seeurl{%s}{' % self.href)
##         else:
##             self.body.append('\\ulink{')
## 
##     def depart_reference(self, node):
##         if self.seealso:
##             self.body.append('}')
##         else:
##             self.body.append('}{%s}' % self.href)

    def visit_reference(self, node):
        #self.pdebug('%% [(visit_reference) node: %s]\n' % str(node))
        if self.seealso:
            if node.has_key('refuri'):
                href = node['refuri']
                href = self.cleanHref(href)
                self.body.append('\\seeurl{%s}{' % href)
        else:
            if node.has_key('refuri'):
                # External hyperlink
                self.body.append('\\ulink{')
            elif node.has_key('refid'):
                # Internal hyperlink
                href = node['refid']
                href = self.cleanHref(href)
                self.body.append('\\ref{%s}' % href)
                raise nodes.SkipNode

    def depart_reference(self, node):
        if self.seealso:
            self.body.append('}')
        else:
            if node.has_key('refuri'):
                # External hyperlink
                href = node['refuri']
                href = self.cleanHref(href)
                self.body.append('}{%s}' % href)
            elif node.has_key('refid'):
                # Internal hyperlink
                pass

    def visit_revision(self, node):
        ## self.pdebug('%% [(visit_revision) node: "%s"]\n' % str(node))
        self.visit_docinfo_item(node, 'revision')
        raise nodes.SkipNode

    def depart_revision(self, node):
        self.depart_docinfo_item(node)

    def visit_section(self, node):
        # If the document title came before the first (outer-most)
        #   section, then the first section is really the first section.
        # If the document title is *in* the first section, then
        #   the first section is a container that has the document title.
        # So, if the document title came before the first section,
        #   then alwasy increment the section_level.
        # But, if the document title is *in* the first section, then
        #   increment the section_level only on the *second* and 
        #   subsequent sections.
        ## self.pdebug('%% [(visit_section) node: %s]\n' % repr(node))
        if self.title_before_section or self.seen_section:
            self.section_level += 1
        self.seen_section = 1

    def depart_section(self, node):
        ## self.pdebug('%% [(depart_section) node: %s]\n' % repr(node))
        if self.title_before_section:
            self.section_level -= 1

    def visit_sidebar(self, node):
        # BUG:  this is just a hack to make sidebars render something 
        self.body.append('\\begin{center}\\begin{sffamily}\n')
        self.body.append('\\fbox{\\colorbox[gray]{0.80}{\\parbox{\\admonitionwidth}{\n')

    def depart_sidebar(self, node):
        self.body.append('}}}\n') # end parbox colorbox fbox
        self.body.append('\\end{sffamily}\n\\end{center}\n');


    attribution_formats = {'dash': ('---', ''),
                           'parentheses': ('(', ')'),
                           'parens': ('(', ')'),
                           'none': ('', '')}

    def visit_attribution(self, node):
        prefix, suffix = self.attribution_formats[self.settings.attribution]
        self.body.append('\n\\begin{flushright}\n')
        self.body.append(prefix)
        self.context.append(suffix)

    def depart_attribution(self, node):
        self.body.append(self.context.pop() + '\n')
        self.body.append('\\end{flushright}\n')

    def visit_status(self, node):
        self.visit_docinfo_item(node, 'status')

    def depart_status(self, node):
        self.depart_docinfo_item(node)

    def visit_strong(self, node):
        self.body.append('\\strong{')

    def depart_strong(self, node):
        self.body.append('}')

    def visit_substitution_definition(self, node):
        raise nodes.SkipNode

    def visit_substitution_reference(self, node):
        self.unimplemented_visit(node)

    def visit_subtitle(self, node):
        self.title = self.title + \
                '\\\\\n\\large{%s}\n' % self.encode(node.astext()) 
        raise nodes.SkipNode

    def depart_subtitle(self, node):
        pass

    def visit_system_message(self, node):
        if node['level'] < self.document.reporter['writer'].report_level:
            raise nodes.SkipNode


    def depart_system_message(self, node):
        self.body.append('\n')

    def visit_target(self, node):
        #self.pdebug('%% [(visit_target) node: %s]\n' % str(node))
        pass
##         if not (node.has_key('refuri') or node.has_key('refid')
##                 or node.has_key('refname')):
##             self.body.append('\\hypertarget{%s}{' % node['name'])
##             self.context.append('}')
##         else:
##             self.context.append('')

    def depart_target(self, node):
        pass
##         self.body.append(self.context.pop())

    def visit_table(self, node):
        self.tableSpec = TableSpec()

    def depart_table(self, node):
        self.tablSpec = None

    def visit_tgroup(self, node):
        columnCount = int(node.get('cols', 0))
        self.tableSpec.setColumnCount(columnCount)
        if columnCount == 2:
            self.body.append('\\begin{tableii}{l|l}{textrm}')
        elif columnCount == 3:
            self.body.append('\\begin{tableiii}{l|l|l}{textrm}')
        elif columnCount == 4:
            self.body.append('\\begin{tableiv}{l|l|l|l}{textrm}')
        elif columnCount == 5:
            self.body.append('\\begin{tablev}{l|l|l|l|l}{textrm}')

    def depart_tgroup(self, node):
        if self.tableSpec.getColumnCount() == 2:
            self.body.append('\n\\end{tableii}\n')
        elif self.tableSpec.getColumnCount() == 3:
            self.body.append('\n\\end{tableiii}\n')
        elif self.tableSpec.getColumnCount() == 4:
            self.body.append('\n\\end{tableiv}\n')
        elif self.tableSpec.getColumnCount() == 5:
            self.body.append('\n\\end{tablev}\n')

    def visit_thead(self, node):
        self.tableSpec.setMode(TABLE_MODE_HEAD)

    def depart_thead(self, node):
        self.tableSpec.setMode(TABLE_MODE_NONE)
            
    def visit_tbody(self, node):
        self.tableSpec.setMode(TABLE_MODE_BODY)

    def depart_tbody(self, node):
        self.tableSpec.setMode(TABLE_MODE_NONE)

    def visit_row(self, node):
        if self.tableSpec.getMode() == TABLE_MODE_HEAD:
            pass
        elif self.tableSpec.getMode() == TABLE_MODE_BODY:
            if self.tableSpec.getColumnCount() == 2:
                self.body.append('\n\\lineii')
            elif self.tableSpec.getColumnCount() == 3:
                self.body.append('\n\\lineiii')
            elif self.tableSpec.getColumnCount() == 4:
                self.body.append('\n\\lineiv')
            elif self.tableSpec.getColumnCount() == 5:
                self.body.append('\n\\linev')

    def depart_row(self, node):
        if self.tableSpec.getMode() == TABLE_MODE_HEAD:
            pass
        elif self.tableSpec.getMode() == TABLE_MODE_BODY:
            pass

    def visit_entry(self, node):
        if self.tableSpec.getMode() == TABLE_MODE_HEAD:
            #self.body.append('%% [(visit_entry) text: +%s+]' % node.astext())
            self.body.append('{%s}' % node.astext().strip(' '))
            raise nodes.SkipNode
        elif self.tableSpec.getMode() == TABLE_MODE_BODY:
            #self.body.append('%% [(visit_entry) text: +%s+]' % node.astext())
            self.body.append('{%s}' % node.astext().strip(' '))
            raise nodes.SkipNode

    def depart_entry(self, node):
        pass
##         if self.tableSpec.getMode() == TABLE_MODE_HEAD:
##             self.body.append('}')
##         elif self.tableSpec.getMode() == TABLE_MODE_BODY:
##             self.body.append('}')

    def visit_term(self, node):
        self.body.append('\\item[')

    def depart_term(self, node):
        # definition list term.
        self.body.append(':]\n')

    def visit_tip(self, node):
        self.visit_admonition(node, 'tip')

    def depart_tip(self, node):
        self.depart_admonition()

##     def string_to_label(self, text):
##         text = text.replace(' ', '-')
##         text = text.replace('_', '-')
##         return text

    def visit_title(self, node):
        #self.pdebug('%% [(visit_title) section_level: %d  node.astext: "%s"]\n' % \
        #    (self.section_level, node.astext()))
        #self.pdebug('%% [(visit_title) section_level: %d  node.astext.encode: "%s"]\n' % \
        #    (self.section_level, self.encode(node.astext())))
        if self.section_level == 0:
            self.title_before_section = 1
        if self.seealso:
            self.body.append('\\end{seealso}\n')
            self.seealso = 0
        if node.astext().lower() == 'see also':
            self.body.append('\\begin{seealso}\n')
            self.seealso = 1
            raise nodes.SkipNode
        else:
            if self.section_level == 0:
                # It's the document title before any section.
                self.title = self.encode(node.astext())
            else:
                # It's a title for a section in-side the document.
                self.body.append('\n\n')
                self.body.append('%' + '_' * 75)
                self.body.append('\n\n')
                s1 = self.encode(node.astext())
                # Remove the non-breaking space character.
                # Not needed.  Using output-encoding=latin-1 fixes this.
                #s1 = s1.replace(u'\xa0', ' ')
                s2 = nodes.make_id(node.astext())
                
                #ipshell('(visit_title) Entering ipshell.\nHit Ctrl-D to exit ipshell.')
                
                if (self.section_level == 1):
                    self.body.append('\\section{%s\\label{%s}}\n' % (s1, s2))
                    #self.body.append(
                    #    '\\section{\htmladdnormallink[%s]{%s}{}}\n' % \
                    #    (s2, s1))
                elif (self.section_level == 2):      
                    self.body.append('\\subsection{%s\\label{%s}}\n' % (s1, s2))
                elif (self.section_level == 3):
                    self.body.append('\\subsubsection{%s\\label{%s}}\n' % (s1, s2))
                elif (self.section_level == 4):
                    self.body.append('\\paragraph{%s\\label{%s}}\n' % (s1, s2))
                else:
                    self.body.append('\\subparagraph{%s\\label{%s}}\n' % (s1, s2))
            raise nodes.SkipNode
        
    def depart_title(self, node):
        pass

    def visit_topic(self, node):
        topic_class = node.get('class')
        if topic_class == 'abstract':
            self.docinfo['abstract'] = node.astext()[9:]
            raise nodes.SkipNode
        else:
            raise nodes.SkipNode

    def depart_topic(self, node):
        pass
    
    def visit_rubric(self, node):
        self.body.append('% [(visit_rubric)]\n')

    def depart_rubric(self, node):
        self.body.append('% [(depart_rubric)]\n')

    def visit_transition(self, node):
        self.body.append('\n\n')
        self.body.append('%' + '_' * 75)
        self.body.append('\n\\hrule{}\n\n')

    def depart_transition(self, node):
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

