# Author: Chris Liechti
# Contact: cliechti@gmx.net
# Revision: $Revision: $
# Date: $Date: $
# Copyright: This module has been placed in the public domain.

"""
S5/HTML Slideshow Writer.
"""

__docformat__ = 'reStructuredText'


import sys
from docutils import nodes
from docutils.writers import html4css1
from docutils.parsers.rst import directives


class Writer(html4css1.Writer):

    settings_spec = html4css1.Writer.settings_spec + (
        'S5 Slideshow Specific Options',
        None,
        (('Specify an S5 theme directory.  Overrides the theme selected '
          'by the document.  The default is "ui" if this option is absent '
          'and the document does not redefine it.',
          ['--theme'],
          {'default': None, 'metavar': '<file>'}),
    ))

    config_section = 's5 writer'
    config_section_dependencies = ('writers', 'html4css1 writer')

    def __init__(self):
        html4css1.Writer.__init__(self)
        self.translator_class = HTMLTranslator


class HTMLTranslator(html4css1.HTMLTranslator):

    def __init__(self, *args):
        html4css1.HTMLTranslator.__init__(self, *args)
        #get theme from document substitutions, or use the default
        if self.document.substitution_defs.has_key('s5 theme'):
            theme = self.document.substitution_defs['s5 theme'].astext()
        else:
            theme = 'ui'
        #the command line overrides the theme setting
        if self.document.settings.theme is not None:
            theme = self.document.settings.theme
        #insert S5 styleshet and script stuff in the HTML header info
        self.body_prefix.insert(0, '''\
<meta name="version" content="S5 1.0" />
<link rel="stylesheet" href="%(s5_theme_dir)s/slides.css" type="text/css" media="projection" id="slideProj" />
<link rel="stylesheet" href="%(s5_theme_dir)s/opera.css" type="text/css" media="projection" id="operaFix" />
<link rel="stylesheet" href="%(s5_theme_dir)s/print.css" type="text/css" media="print" id="slidePrint" />
<script src="%(s5_theme_dir)s/slides.js" type="text/javascript"></script>\n''' % {
's5_theme_dir': theme,
        })

    def visit_document(self, node):
        #~ # empty or untitled document?
        #~ if not len(node) or not isinstance(node[0], nodes.title):
            #~ # for XHTML conformance, modulo IE6 appeasement:
            #~ self.head.insert(0, '<title></title>\n')

        #try to use substitution to set title
        title = ''
        if self.document.substitution_defs.has_key('s5 title'):
            title = self.document.substitution_defs['s5 title'].astext()
        else:
            #try to get the presentation title
            if isinstance(node[0], nodes.title):
                title = node[0].astext()
        #additional footer information can be set trough document substitutions
        if self.document.substitution_defs.has_key('s5 location'):
            location = '<h2>%s</h2>' % self.document.substitution_defs['s5 location'].astext()
        else:
            location = ''
        #insert the slide layout master once in the HTML body
        self.body_prefix.append('''\
<div class="layout">
    <div id="currentSlide"></div>
    <div id="header"></div>
    <div id="footer">
        %s%s
        <div id="controls"></div>
    </div>
</div>\n''' % (title and '<h1>%s</h1>' % title or '', location)
        )
        #set document title. lets hope the user did not use a document title
        #as we would get two of these in the HTML headers...
        self.head.append('<title>%s</title>\n' % title)

    def depart_document(self, node):
        self.fragment.extend(self.body)
        self.body_prefix.append(
            self.starttag(node, 'div', CLASS='presentation'))
        self.body_suffix.insert(0, '</div>\n')

    def visit_section(self, node):
        self.section_level += 1
        if self.section_level > 1:
            # dummy for matching div's
            self.body.append(self.starttag(node, 'div', CLASS='section'))
        else:
            self.body.append(self.starttag(node, 'div', CLASS='slide'))


#extra directive for handouts

def handout_directive(name, arguments, options, content, lineno,
                      content_offset, block_text, state, state_machine):
    text = '\n'.join(content)
    if not text:
        warning = state_machine.reporter.warning(
            'The handout block is empty; content required.',
            '',
            nodes.literal_block(block_text, block_text), line=lineno)
        return [warning]
    node = nodes.section(text)
    #~ if options.has_key('class'):
        #~ node.set_class(options['class'])
    node.set_class('handout')
    state.nested_parse(content, content_offset, node)
    return [node]

handout_directive.content = 1
#~ handout_directive.options = {'class': directives.class_option}
directives.register_directive('handout', handout_directive)
