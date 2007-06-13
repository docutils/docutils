# Author: Guenter Milde
# Contact: milde@users.berlios.de
# Revision: $Revision$
# Date: $Date: 2005-06-28$
# Copyright: Licensed under the Academic Free License version 1.2

"""
Simple HyperText Markup Language document tree Writer.

Work in progress. The goal is:

The output conforms to the XHTML version 1.0 Transitional DTD and contains
enough formatting information to be viewed without a cascading style sheet by
a lightweight html browser (e.g. Dillo__ or the console browser elinks__).

__ http://www.dillo.org
__ http://elinks.cz

"""

__docformat__ = 'reStructuredText'


import docutils
from docutils import frontend, nodes, utils, writers, languages

from docutils.writers import html4css1
# from html4css1 import SimpleListChecker

class Writer(html4css1.Writer):

    supported = ('html', 'xhtml', 'html4', 'xhtml4',
                 'html4trans', 'xhtml1trans')
    """Formats this writer supports."""
    
    config_section = 'html4trans writer'

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = HTMLTranslator


class HTMLTranslator(html4css1.HTMLTranslator):

    """
    This HTML writer has been customized to produce HTML with direct 
    formatting of objects to be usable without CSS stylesheet
    """
    
    _title_color = '' # ('red' in Warnings, etc)

    def visit_admonition(self, node, name='', title_color=''):
        self.body.append(self.starttag(node, 'blockquote', '',
                                       CLASS=(name or 'admonition')))
        self.body.append(self.starttag(node, 'table', '', 
                                       border=1, width='100%'))
        self.body.append('<tr><td>\n')
        self._title_color = title_color
        if name:
            node.insert(0, nodes.title(name, self.language.labels[name]))
        # self.set_first_last(node)

    def depart_admonition(self, node=None):
        self._title_color=''
        self.body.append('</td></tr>\n'
                         '</table></blockquote>\n')

    def visit_attention(self, node):
        self.visit_admonition(node, 'attention', 'red')

    def visit_caution(self, node):
        self.visit_admonition(node, 'caution', 'red')

    def visit_citation(self, node):
        self.visit_footnote(node, CLASS='docutils citation'),
        
    def visit_danger(self, node):
        self.visit_admonition(node, 'danger', 'red')

    def visit_doctest_block(self, node):
        self.visit_literal_block(node, CLASS='doctest-block')

    def depart_doctest_block(self, node):
        self.depart_literal_block(node)
        
    # def visit_enumerated_list(self, node): 
    #     """TODO: look in the sandbox/bbum/DocArticle/DocArticle/ for emulation
    #     of arabic, roman and alpha list markers """
        
    def visit_error(self, node):
        self.visit_admonition(node, 'error', 'red')

    def visit_field(self, node):
        """top-align field list elements"""
        self.body.append(self.starttag(node, 'tr', '', CLASS='field',
                                       valign='top'))

    def visit_field_name(self, node):
        """left align field name"""
        atts = {}
        if self.in_docinfo:
            atts['class'] = 'docinfo-name'
        else:
            atts['class'] = 'field-name'
        if ( self.settings.field_name_limit
             and len(node.astext()) > self.settings.field_name_limit):
            atts['colspan'] = 2
            self.context.append('</tr>\n<tr><td>&nbsp;</td>')
        else:
            self.context.append('')
        self.body.append(self.starttag(node, 'th', '', align='left', **atts))
    
    def visit_footnote(self, node, CLASS='docutils footnote'):
        self.body.append(self.starttag(node, 'table', CLASS=CLASS,
                                       frame="void", rules="none"))
        self.body.append('<colgroup><col class="label" /><col /></colgroup>\n'
                         '<tbody>\n'
                         '<tr valign="top">')
        self.footnote_backrefs(node)
        
    def visit_line_block(self, node):
        """
        TODO:
        Einrueckungen selbst formatieren, entweder durch geschachtelte Tabellen
        oder indem Sie Zeilenumbrueche durch <nobr> ... </nobr> ("No Break")
        verhindern und die Einrueckungen durch Aneinandersetzen mehrerer
        geschuetzter Leerzeichen ";nbsp" realisieren.
        """
        self.body.append(self.starttag(node, 'div', CLASS='line-block'))

    # def depart_line_block(self, node):
    #     self.body.append('</div>\n')

    def visit_literal_block(self, node, CLASS='literal-block'):
        self.body.append(self.starttag(node, 'blockquote', ''))
        self.body.append(self.starttag(node, 'pre', CLASS=CLASS))

    def depart_literal_block(self, node):
        self.body.append('\n</pre></blockquote>\n')

    def visit_problematic(self, node):
        """make text red"""
        self.body.append(self.starttag(node, 'font', '', CLASS='problematic',
                                      color='red'))
        if node.hasattr('refid'):
            self.body.append('<a href="#%s">' % node['refid'])
            self.context.append('</a>')
        else:
            self.context.append('')

    def depart_problematic(self, node):
        self.body.append(self.context.pop())
        self.body.append('</font>')
    
    def visit_subtitle(self, node):
        """Center sub title"""
        if isinstance(node.parent, nodes.document):
            self.body.append(self.starttag(node, 'h2', '', CLASS='subtitle',
                                           align='center'))
            self.context.append('</h2>\n')
            self.in_document_title = len(self.body)
        else:
            html4css1.HTMLTranslator.visit_subtitle(self, node)

    def visit_system_message(self, node):
        self.visit_admonition(node, '', 'red')
        
        self.body.append('<p class="system-message-title">')
        backref_text = ''
        if len(node['backrefs']):
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
        self.body.append('<font color=red> System Message: %s/%s '
                         '(<tt class="docutils">%s</tt>%s)%s</font></p>\n'
                         % (node['type'], node['level'],
                            self.encode(node['source']), line, backref_text))

    def depart_system_message(self, node):
        self.depart_admonition(node)
        
    def visit_title(self, node):
        """Center title, Admonition title bold, evt. coloured"""
        check_id = 0
        
        if isinstance(node.parent, nodes.Admonition):
            close_tag = '</b></p>\n'
            self.body.append(self.starttag(node, 'p', '', 
                                           CLASS='admonition-title'))
            self.body.append('<b>')
            # if isinstance(node.parent, nodes.Note):
            if self._title_color:
                self.body.append('<font color="%s">'%self._title_color)
                close_tag = '</font>' + close_tag
            self.context.append(close_tag)
            
        elif isinstance(node.parent, nodes.document):
            self.body.append(self.starttag(node, 'h1', '', CLASS='title',
                                           align='center'))
            self.context.append('</h1>\n')
            self.in_document_title = len(self.body)
        elif (isinstance(node.parent, nodes.section)
              and 'system-messages' in node.parent.attributes['classes']):
            # print "System messages node", node.parent.attributes
            self.body.append(self.starttag(node, 'h1', '', 
                                           CLASS='system-messages',
                                           align='center'))
            self.body.append('<font color="red">')
            self.context.append('</font></h1>\n')
        else:
            html4css1.HTMLTranslator.visit_title(self, node)

    def visit_warning(self, node):
        self.visit_admonition(node, 'warning', 'red')



# Testing
# -------

if __name__ == '__main__':
    try:
        import test_html4trans
    except ImportError:
        pass
