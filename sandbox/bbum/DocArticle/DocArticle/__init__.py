# Author: Bill Bumgarner
# Contact: bbum@codefab.com

"""
A writer for DocUtils that spews HTML compliant with O'Reilly's article submission guidelines.
"""

try:
    x = True
except NameError:
    True = 1
    False = 0

__docformat__ = 'reStructuredText'

import sys
from warnings import warn
import re
from types import *

import docutils
from docutils import nodes, utils, writers, languages

from DocArticle import DocArticleText

class DocArticleWriter(writers.Writer):
    supported = ('html')
    """Formats this writer supports."""

    output = None
    """Final translated form of `document`."""

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = HTMLDocArticleTranslator

    def translate(self):
        visitor = self.translator_class(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()

SpewNothing = 0
SpewParagraph = 1
SpewBreak = 2

class HTMLDocArticleTranslator(nodes.NodeVisitor):
    headerContent = []
    bodyContent = []
    metaContent = []
    context = []
    spewTextContext = [True]
    spewParaTag = [SpewParagraph]
    paraFormat = [(None,None)]

    body_pre_docinfo = []
    docinfo = []
    compact_simple = None
    compact_p = 1

    firstFootnoteVisited = False

    named_tags = {'a': 1,
                  'applet': 1,
                  'form': 1,
                  'frame': 1,
                  'iframe': 1,
                  'img': 1,
                  'map': 1}

    words_and_spaces = re.compile(r'\S+| +|\n')

    def __init__(self, document):
        nodes.NodeVisitor.__init__(self, document)
        self.section_level = 0

    def astext(self):
        return ''.join([DocArticleText.contentStart, DocArticleText.headerStart] +
                       self.headerContent +
                       [DocArticleText.headerEnd, DocArticleText.bodyStart] +
                       self.body_pre_docinfo + self.docinfo + self.bodyContent +
                       [DocArticleText.bodyEnd, DocArticleText.contentEnd])

    def encode(self, text):
        """Encode special characters in `text` & return."""
        # @@@ A codec to do these and all other HTML entities would be nice.
        text = text.replace("&", "&amp;")
        text = text.replace("<", "&lt;")
        text = text.replace('"', "&quot;")
        text = text.replace(">", "&gt;")
        return text

    def popAndAppend(self, node):
        possiblePoppedContent = self.context.pop()
        if possiblePoppedContent:
            self.bodyContent.append(possiblePoppedContent)

    def attval(self, text,
               whitespace=re.compile('[\n\r\t\v\f]')):
        """Cleanse, HTML encode, and return attribute value text."""
        return self.encode(whitespace.sub(' ', text))

    def visit_Text(self, node):
        if self.spewTextContext[-1]:
            self.bodyContent.append(self.encode(node.astext()))

    def depart_Text(self, node):
        pass

    def visit_author(self, node):
        self.visit_docinfo_item(node, 'Author')

    def depart_author(self, node):
        self.depart_docinfo_item(node)

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention')

    def depart_attention(self, node):
        self.depart_admonition()

    def visit_bullet_list(self, node):
        self.bodyContent.append(self.starttag(node, 'ul'))
        self.spewParaTag.append(SpewNothing)

    def depart_bullet_list(self, node):
        self.bodyContent.append('</ul>\n')
        self.spewParaTag.pop()

    def visit_copyright(self, node):
        self.visit_docinfo_item(node, 'copyright')

    def depart_copyright(self, node):
        self.depart_docinfo_item()

    def check_simple_list(self, node):
        """Check for a simple list that can be rendered compactly."""
        visitor = SimpleListChecker(self.document)
        try:
            node.walk(visitor)
        except nodes.NodeFound:
            return None
        else:
            return 1

    def visit_contact(self, node):
        self.visit_docinfo_item(node, 'Contact')

    def depart_contact(self, node):
        self.depart_docinfo_item(node)

    def visit_copyright(self, node):
        self.visit_docinfo_item(node, 'copyright')

    def depart_copyright(self, node):
        self.depart_docinfo_item(node)

    def visit_decoration(self, node):
        pass

    def depart_decoration(self, node):
        pass

    def visit_docinfo(self, node):
        self.context.append(len(self.bodyContent))

    def depart_docinfo(self, node):
        start = self.context.pop()
        self.body_pre_docinfo = self.bodyContent[:start]
        self.docinfo = self.bodyContent[start:]
        self.bodyContent = []

    def visit_docinfo_item(self, node, name, meta=1):
        value = self.attval(node.astext())
        if meta:
            self.headerContent.append('<meta name="%s" content="%s" />\n'%(name.lower(), value))
        self.bodyContent.append('<b>%s</b>: ' % name)
        self.context.append('<br />\n')

    def depart_docinfo_item(self, node):
        self.popAndAppend(node)

    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass

    def visit_emphasis(self, node):
        self.bodyContent.append('<i>')

    def depart_emphasis(self, node):
        self.bodyContent.append('</i>')

    def visit_footnote(self, node):
        if not self.firstFootnoteVisited:
            self.bodyContent.append('<hr />')
            self.firstFootnoteVisited = True
        self.bodyContent.append(self.starttag(node, 'table', frame="void", rules="none"))
        self.bodyContent.append('<tbody valign="top">\n<tr>')
        self.spewParaTag.append(SpewBreak)
        self.footnote_backrefs(node)

    def footnote_backrefs(self, node):
        # if self.settings.footnote_backlinks and node.hasattr('backrefs'):
        if node.hasattr('backrefs'):
            backrefs = node['backrefs']
            if len(backrefs) == 1:
                self.context.append('')
                self.context.append('<a href="#%s" name="%s">' % (backrefs[0], node['id']))
            else:
                i = 1
                backlinks = []
                for backref in backrefs:
                    backlinks.append('<a  href="#%s">%s</a>' % (backref, i))
                    i += 1
                self.context.append('<em>(%s)</em> ' % ', '.join(backlinks))
                self.context.append('<a name="%s">' % node['id'])
        else:
            self.context.append('')
            self.context.append('<a name="%s">' % node['id'])

    def depart_footnote(self, node):
        self.spewParaTag.pop()
        self.paraFormat.pop()
        self.bodyContent.append('</td></tr>\n</tbody>\n</table>\n')
        
    def visit_footnote_reference(self, node):
        href = ''
        if node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        # format = self.settings.footnote_references
        format = 'superscript'
        if format == 'brackets':
            suffix = '['
            self.context.append(']')
        elif format == 'superscript':
            suffix = '<sup>'
            self.context.append('</sup>')
        else:                           # shouldn't happen
            suffix = '???'
            self.content.append('???')
        self.bodyContent.append('<b>')
        self.bodyContent.append(suffix)
        self.bodyContent.append(self.starttag(node, 'a', '', href=href))

    def depart_footnote_reference(self, node):
        self.bodyContent.append('</a>')
        self.bodyContent.append(self.context.pop())
        self.bodyContent.append('</b>')

    def visit_label(self, node):
        self.bodyContent.append(self.starttag(node, 'td', '<b>[%s' % self.context.pop()))

    def depart_label(self, node):
        self.paraFormat.append(('<font size=-1><i>', '</i></font>'))
        self.bodyContent.append('</a>]</b></td><td>%s' % self.context.pop())

    def visit_list_item(self, node):
        self.bodyContent.append(self.starttag(node, 'li', ''))

    def depart_list_item(self, node):
        self.bodyContent.append('</li>\n')

    def visit_literal(self, node):
        self.bodyContent.append(self.starttag(node, 'code', ''))
        text = node.astext()
        for token in self.words_and_spaces.findall(text):
            if token.strip():
                # Protect text like "--an-option" from bad line wrapping:
                self.bodyContent.append('<span>%s</span>' % self.encode(token))
            elif token in ('\n', ' '):
                # Allow breaks at whitespace:
                self.bodyContent.append(token)
            else:
                # Protect runs of multiple spaces; the last space can wrap:
                self.bodyContent.append('&nbsp;' * (len(token) - 1) + ' ')
        self.bodyContent.append('</code>')
        # Content already processed:
        raise nodes.SkipNode

    def visit_literal_block(self, node):
        self.bodyContent.append(self.starttag(node, 'pre'))

    def depart_literal_block(self, node):
        self.bodyContent.append('\n</pre>\n')

    def visit_paragraph(self, node):
        if self.spewParaTag[-1] == SpewParagraph:
            self.bodyContent.append(self.starttag(node, 'p', ''))
            self.context.append('</p>\n')
        elif self.spewParaTag[-1] == SpewBreak:
            self.context.append('<br />\n')
        else:
            self.context.append(None)

        start, end = self.paraFormat[-1]
        if start:
            self.bodyContent.append(start)
        if end:
            self.context.append(end)
        else:
            self.context.append(None)

    def depart_paragraph(self, node):
        self.popAndAppend(node) # pop end formatting tag, if any
        self.popAndAppend(node) # pop end paragraph tag, if any

    def visit_problematic(self, node):
        if node.hasattr('refid'):
            self.bodyContent.append('<a href="#%s" name="%s">' % (node['refid'], node['id']))
            self.context.append('</a>')
        else:
            self.context.append('')
        self.bodyContent.append(self.starttag(node, 'span', ''))

    def depart_problematic(self, node):
        self.bodyContent.append('</span>')
        self.bodyContent.append(self.context.pop())

    def visit_reference(self, node):
        if node.has_key('refuri'):
            href = node['refuri']
        elif node.has_key('refid'):
            href = '#' + node['refid']
        elif node.has_key('refname'):
            href = '#' + self.document.nameids[node['refname']]
        self.bodyContent.append(self.starttag(node, 'a', '', href=href))
        self.context.append('</a>')

    depart_reference = popAndAppend

    def visit_section(self, node):
        self.section_level += 1
        #hTag = 'h%s'% self.section_level
        #self.bodyContent.append(self.starttag(node, hTag))
        #self.bodyContent.append('</%s>\n' % hTag)

    def depart_section(self, node):
        self.section_level -= 1

    def visit_strong(self, node):
        self.bodyContent.append('<strong>')

    def depart_strong(self, node):
        self.bodyContent.append('</strong>')

    def visit_target(self, node):
        if not (node.has_key('refuri') or node.has_key('refid') or node.has_key('refname')):
            self.bodyContents.append(self.starttag(node, 'a', ''))
            self.context.append('</a>')
        else:
            self.context.append(None)

    depart_target = popAndAppend

    def visit_title(self, node):
        if isinstance(node.parent, nodes.topic):
            self.bodyContent.append(self.starttag(node, 'p', ''))
            if node.parent.hasattr('id'):
                self.bodyContent.append(self.starttag({},'a','',name=node.parent['id']))
                self.context.append('</a></p>\n')
            else:
                self.context.append('</p>\n')
        elif self.section_level == 0:
            self.headerContent.append(DocArticleText.titleStart)
            self.headerContent.append(self.encode(node.astext()))
            self.headerContent.append(DocArticleText.titleEnd)
            self.bodyContent.append(self.starttag(node, 'h2', ''))
            self.context.append('</h2>\n')
        else:
            ### O'Reilly uses h2 to denote title and h3 for all sections.  In theory,
            ### nothing should hang below h3.  In practice, we leave it up to the
            ### author.
            level = self.section_level + 1
            self.bodyContent.append(self.starttag(node, 'h%s' % level, ''))
            atts = {}
            if node.parent.hasattr('id'):
                atts['name'] = node.parent['id']
            if node.hasattr('refid'):
                atts['href'] = '#' + node['refid']
            self.bodyContent.append(self.starttag({}, 'a', '', **atts))
            self.context.append('</a></h%s>\n' % level)

    def depart_title(self, node):
        self.popAndAppend(node)
    
    def visit_topic(self, node):
        pass

    def depart_topic(self, node):
        pass

    def unknown_visit(self, node):
        print "Failure processing at line (%s) of node:\n %s" % (node.line, node.pformat())
        raise NotImplementedError('visiting unknown node type: %s'
                                  % node.__class__.__name__)

    def visit_system_message(self, node):
        if node['level'] < self.document.reporter['writer'].report_level:
            # Level is too low to display:
            raise nodes.SkipNode
        self.bodyContent.append(self.starttag(node, 'div'))
        self.bodyContent.append('<p>')
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
        self.bodyContent.append('System Message: %s%s/%s%s (<tt>%s</tt>%s)%s</p>\n'
                         % (a_start, node['type'], node['level'], a_end,
                            node['source'], line, backref_text))

    def depart_system_message(self, node):
        self.bodyContent.append('</div>\n')

    def starttag(self, node, tagname, suffix='\n', infix='', **attributes):
        tagname = tagname.lower()
        atts = {}
        for (name, value) in attributes.items():
            atts[name.lower()] = value
        for att in ('id',):             # node attribute overrides
            if node.has_key(att):
                atts[att] = node[att]
        if atts.has_key('id') and self.named_tags.has_key(tagname):
            atts['name'] = atts['id']   # for compatibility with old browsers
        attlist = atts.items()
        attlist.sort()
        parts = [tagname]
        for name, value in attlist:
            if value is None:           # boolean attribute
                # According to the HTML spec, ``<element boolean>`` is good,
                # ``<element boolean="boolean">`` is bad.
                # (But the XHTML (XML) spec says the opposite.  <sigh>)
                parts.append(name.lower())
            elif isinstance(value, ListType):
                values = [str(v) for v in value]
                parts.append('%s="%s"' % (name.lower(),
                                          self.attval(' '.join(values))))
            else:
                parts.append('%s="%s"' % (name.lower(),
                                          self.attval(str(value))))
        return '<%s%s>%s' % (' '.join(parts), infix, suffix)
