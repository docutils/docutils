"""OpenOffice writer

The output is an OpenOffice.org 1.0-compatible document."""

__author__ = "Patrick K. O'Brien <pobrien@orbtech.com>"
__cvsid__ = "$Id$"
__revision__ = "$Revision$"[11:-2]

# Based on work orginally created by:
# Author: Aahz
# Contact: aahz@pythoncraft.com

__docformat__ = 'reStructuredText'

import sys
from warnings import warn
import re

import docutils
from docutils import nodes, utils, writers, languages

import OOtext

import Image  # from the Python Imaging Library

section_styles = [
    '.ch title',
    '.head 1',
    '.head 2',
    '.head 3alone',
    ]


class Writer(writers.Writer):

    supported = ('OpenOffice')
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
        self.para_styles = ['.body']
        self.compact_p = 1
        self.compact_simple = None
        self.context = []
        self.inBulletList = False
        self.inEnumList = False
        self.inTableHead = False
        self.inTableBody = False
        self.bodyOne = False
        self.colspecs = []

    def astext(self):
        """Return the final formatted document as a string."""
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
        self.skip_para_tag = False
        self.para_styles.append('.CALLOUT')

    def depart_admonition(self):
        self.para_styles.pop()
        self.bodyOne = True

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

##     def visit_block_quote(self, node):
##         self.skip_para_tag = True
##         self.body.append(self.start_para % '.quotes')

##     def depart_block_quote(self, node):
##         self.body.append(self.end_para)
##         self.skip_para_tag = False
##         self.bodyOne = True

    def visit_bullet_list(self, node):
        self.inBulletList = True
        self.body.append('\n<text:unordered-list text:style-name="BulletList">\n')

    def depart_bullet_list(self, node):
        self.body.append('</text:unordered-list>\n')
        self.inBulletList = False
        self.bodyOne = True

    def visit_caption(self, node):
        pass

    def depart_caption(self, node):
        pass

    def visit_caution(self, node):
        self.visit_admonition(node, 'caution')

    def depart_caution(self, node):
        self.depart_admonition()

##     def visit_citation(self, node):
##         self.body.append(self.starttag(node, 'table', CLASS='citation',
##                                        frame="void", rules="none"))
##         self.footnote_backrefs(node)

##     def depart_citation(self, node):
##         self.body.append('</td></tr>\n'
##                          '</tbody>\n</table>\n')

##     def visit_citation_reference(self, node):
##         href = ''
##         if node.has_key('refid'):
##             href = '#' + node['refid']
##         elif node.has_key('refname'):
##             href = '#' + self.document.nameids[node['refname']]
##         self.body.append(self.starttag(node, 'a', '[', href=href,
##                                        CLASS='citation-reference'))

##     def depart_citation_reference(self, node):
##         self.body.append(']</a>')

##     def visit_classifier(self, node):
##         self.body.append(' <span class="classifier-delimiter">:</span> ')
##         self.body.append(self.starttag(node, 'span', '', CLASS='classifier'))

##     def depart_classifier(self, node):
##         self.body.append('</span>')

    def visit_colspec(self, node):
        self.colspecs.append(node)

    def depart_colspec(self, node):
        pass

    def write_colspecs(self):
        width = 0
        for node in self.colspecs:
            width += node['colwidth']
        for node in self.colspecs:
            self.body.append('<table:table-column/>')

##             colwidth = int(node['colwidth'] * 100.0 / width + 0.5)
##             self.body.append(self.emptytag(node, 'col',
##                                            colwidth='%i%%' % colwidth))
        self.colspecs = []

    def visit_comment(self, node):
        raise nodes.SkipNode

    def visit_decoration(self, node):
        raise nodes.SkipNode

    def depart_decoration(self, node):
        pass

    def visit_definition(self, node):
        pass

    def depart_definition(self, node):
        pass

    def visit_definition_list(self, node):
        pass

    def depart_definition_list(self, node):
        pass

    def visit_definition_list_item(self, node):
        pass

    def depart_definition_list_item(self, node):
        pass

    def visit_docinfo(self, node):
        raise nodes.SkipNode

    def depart_docinfo(self, node):
        pass

    def visit_doctest_block(self, node):
        self.visit_literal_block(node)

    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass

    def visit_emphasis(self, node):
        self.body.append(self.start_charstyle % 'Emphasis')

    def depart_emphasis(self, node):
        self.body.append(self.end_charstyle)

    def visit_entry(self, node):
        self.body.append('<table:table-cell table:value-type="string">\n')

    def depart_entry(self, node):
        self.body.append('</table:table-cell>\n')

    def visit_enumerated_list(self, node):
        self.inEnumList = True
        self.body.append('\n<text:ordered-list text:style-name="NumberedList">\n')

    def depart_enumerated_list(self, node):
        self.body.append('</text:ordered-list>\n')
        self.inEnumList = False
        self.bodyOne = True

    def visit_error(self, node):
        self.visit_admonition(node, 'error')

    def depart_error(self, node):
        self.depart_admonition()

    def visit_figure(self, node):
        self.body.append(self.start_para % '.figure')

    def depart_figure(self, node):
        self.body.append(self.end_para)
        self.bodyOne = True

    def visit_footer(self, node):
        pass
##         self.context.append(len(self.body))

    def depart_footer(self, node):
        pass
##         start = self.context.pop()
##         footer = (['<hr class="footer" />\n',
##                    self.starttag(node, 'div', CLASS='footer')]
##                   + self.body[start:] + ['</div>\n'])
##         self.body_suffix[:0] = footer
##         del self.body[start:]

    def visit_footnote(self, node):
        raise nodes.SkipNode

##     def footnote_backrefs(self, node):
##         warn("footnote backrefs not available")

    def depart_footnote(self, node):
        pass

##     def visit_footnote_reference(self, node):
##         name = node['refid']
##         id = node['id']
##         number = node['auto']
##         for footnote in self.document.autofootnotes:
##             if name == footnote['name']:
##                 break
##         self.body.append('<text:footnote text:id="%s">\n' % id)
##         self.body.append('<text:footnote-citation text:string-value="%s"/>\n' % number)
##         self.body.append('<text:footnote-body>\n')
##         self.body.append(self.start_para % '.body')
##         for child in footnote.children:
##             if isinstance(child, nodes.paragraph):
##                 self.body.append(child.astext())
##         self.body.append(self.end_para)
##         self.body.append('</text:footnote-body>\n')
##         self.body.append('</text:footnote>')
##         raise nodes.SkipNode

##     def depart_footnote_reference(self, node):
##         pass

    def visit_generated(self, node):
        pass

    def depart_generated(self, node):
        pass

##     def visit_header(self, node):
##         self.context.append(len(self.body))

##     def depart_header(self, node):
##         start = self.context.pop()
##         self.body_prefix.append(self.starttag(node, 'div', CLASS='header'))
##         self.body_prefix.extend(self.body[start:])
##         self.body_prefix.append('<hr />\n</div>\n')
##         del self.body[start:]

    def visit_hint(self, node):
        self.visit_admonition(node, 'hint')

    def depart_hint(self, node):
        self.depart_admonition()

    def visit_image(self, node):
        name = str(node.attributes['uri'])
        image = Image.open(name)
        format = image.format
        dpi = 96.0
        width, height = image.size
        width /= dpi
        height /= dpi
        scale = None
        if 'scale' in node.attributes:
            scale = node.attributes['scale']
        if scale is not None:
            factor = scale / 100.0
            width *= factor
            height *= factor
        # Add to our list so that rest2oo.py can create the manifest.
        if format == 'PNG':
            OOtext.pictures.append((name, OOtext.m_png_format % name))
        elif format == 'TIFF':
            OOtext.pictures.append((name, OOtext.m_tif_format % name))
        else:
            print '*** Image type not recognized ***', repr(name)
        #self.body.append('<text:line-break/>\n')
        self.body.append('<draw:image draw:style-name="image"\n')
        self.body.append('draw:name="%s"\n' % name)
        self.body.append('text:anchor-type="char"\n')
        self.body.append('svg:width="%0.2finch"\n' % width)
        self.body.append('svg:height="%0.2finch"\n' % height)
        self.body.append('draw:z-index="0"\n')
        self.body.append('xlink:href="#Pictures/%s"\n' % name)
        self.body.append('xlink:type="simple"\n') 
        self.body.append('xlink:show="embed"\n')
        self.body.append('xlink:actuate="onLoad"/>')
        self.body.append('Figure X.X\n')

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

##     def visit_line_block(self, node):
##         self.body.append(self.start_para % '.quotes')
##         lines = node.astext()
##         lines = lines.split('\n')
##         lines = self.line_break.join(lines)
##         self.body.append(lines)
##         self.body.append(self.end_para)
##         raise nodes.SkipNode

    def visit_list_item(self, node):
        self.body.append('<text:list-item>')
##         if len(node):
##             node[0].set_class('first')

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
        self.bodyOne = True
        raise nodes.SkipNode

    def visit_note(self, node):
        self.visit_admonition(node, 'note')

    def depart_note(self, node):
        self.depart_admonition()

    def visit_paragraph(self, node):
        style = self.para_styles[-1]
        if self.inBulletList:
            style = '.bullet'
        elif self.inEnumList:
            style = '.numlist'
        elif self.inTableHead:
            style = '.cell head'
        elif self.inTableBody:
            style = '.cell body'
        elif node.astext().startswith('(annotation)'):
            style = '.code NOTATION'
        elif self.bodyOne or node.astext().startswith('#'):
            if style == '.body':
                style = '.body1'
                self.bodyOne = False
        if not self.skip_para_tag:
            self.body.append(self.start_para % style)

    def depart_paragraph(self, node):
        if not self.skip_para_tag:
            self.body.append(self.end_para)

    def visit_reference(self, node):
        pass

    def depart_reference(self, node):
        pass

    def visit_row(self, node):
        self.body.append('<table:table-row>\n')

    def depart_row(self, node):
        self.body.append('</table:table-row>\n')

    def visit_section(self, node):
        self.section_level += 1
        self.bodyOne = True

    def depart_section(self, node):
        self.section_level -= 1

## XX Perhaps these should replace admonition:

##     def visit_sidebar(self, node):
##         self.body.append(self.starttag(node, 'div', CLASS='sidebar'))
##         self.in_sidebar = 1

##     def depart_sidebar(self, node):
##         self.body.append('</div>\n')
##         self.in_sidebar = None

    def visit_strong(self, node):
        self.body.append(self.start_charstyle % 'Strong Emphasis')

    def depart_strong(self, node):
        self.body.append(self.end_charstyle)

    def visit_table(self, node):
        self.body.append('<table:table>\n')

    def depart_table(self, node):
        self.body.append('</table:table>\n')
        self.bodyOne = True

    def visit_tbody(self, node):
        self.write_colspecs()
        self.inTableBody = True

    def depart_tbody(self, node):
        self.inTableBody = False

    def visit_term(self, node):
        self.bodyOne = True
        self.visit_paragraph(node)
        self.body.append(self.start_charstyle % 'Strong Emphasis')

    def depart_term(self, node):
        self.body.append(self.end_charstyle)
        self.depart_paragraph(node)

    def visit_tgroup(self, node):
        pass

    def depart_tgroup(self, node):
        pass

    def visit_thead(self, node):
        self.write_colspecs()
        self.inTableHead = True

    def depart_thead(self, node):
        self.inTableHead = False

    def visit_tip(self, node):
        self.visit_admonition(node, 'tip')

    def depart_tip(self, node):
        self.depart_admonition()

    def visit_title(self, node):
        """Only 4 section levels are supported by this writer."""
        title_tag = self.start_para % section_styles[self.section_level]
        self.body.append(title_tag)

    def depart_title(self, node):
        self.body.append(self.end_para)

    def visit_topic(self, node):
        if node.has_key('class') and node['class'] == 'contents':
            raise nodes.SkipNode
        else:
            pass

    def depart_topic(self, node):
        pass

    def visit_transition(self, node):
        self.visit_paragraph(node)

    def depart_transition(self, node):
        self.depart_paragraph(node)

    def visit_warning(self, node):
        self.visit_admonition(node, 'warning')

    def depart_warning(self, node):
        self.depart_admonition()

    def visit_system_message(self, node):
        print node.astext()

    def depart_system_message(self, node):
        pass

    def unknown_visit(self, node):
        print "=" * 70
        print "Failure due to unknown node type"
        print "-" * 70
        print "Failed node is: %r" % node
        print "Failed line is:", node.line
        print "Failed text is:", node.astext()
        print "=" * 70
        raise NotImplementedError('unimplemented node type: %s'
                                  % node.__class__.__name__)
