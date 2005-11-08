# Author: Chris Liechti
# Contact: cliechti@gmx.net
# Revision: $Revision$
# Date: $Date$
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
        'The --compact-lists option (defined in HTML-Specific Options above) '
        'is disabled by default for the S5/HTML writer.',
        (('Specify an S5 theme directory (typically a subdirectory of "ui") '
          'or URL (if it contains a slash).  The default is "default".',
          ['--theme'],
          {'default': 'default', 'metavar': '<path>'}),))

    settings_default_overrides = {'compact_lists': 0}

    config_section = 's5 writer'
    config_section_dependencies = ('writers', 'html4css1 writer')

    def __init__(self):
        html4css1.Writer.__init__(self)
        self.translator_class = HTMLTranslator


class HTMLTranslator(html4css1.HTMLTranslator):

    doctype = (
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"'
        ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">\n')
    s5_stylesheet_template = '''\
<!-- configuration parameters -->
<meta name="defaultView" content="slideshow" />
<meta name="controlVis" content="hidden" />
<!-- style sheet links -->
<link rel="stylesheet" href="%(s5_theme_url)s/slides.css" type="text/css" media="projection" id="slideProj" />
<link rel="stylesheet" href="%(s5_theme_url)s/outline.css" type="text/css" media="screen" id="outlineStyle" />
<link rel="stylesheet" href="%(s5_theme_url)s/print.css" type="text/css" media="print" id="slidePrint" />
<link rel="stylesheet" href="%(s5_theme_url)s/opera.css" type="text/css" media="projection" id="operaFix" />
<script src="%(s5_theme_url)s/slides.js" type="text/javascript"></script>\n'''
    layout_template = '''\
<div class="layout">
<div id="controls"></div>
<div id="currentSlide"></div>
<div id="header">
%(header)s
</div>
<div id="footer">
%(title)s%(footer)s
</div>
<div class="topleft"></div>
<div class="topright"></div>
<div class="bottomleft"></div>
<div class="bottomright"></div>
</div>\n'''
        
    def __init__(self, *args):
        html4css1.HTMLTranslator.__init__(self, *args)
        #insert S5-specific stylesheet and script stuff:
        theme = self.document.settings.theme
        if '/' not in theme:
            theme = 'ui/' + theme
        self.stylesheet.append(self.s5_stylesheet_template
                               % {'s5_theme_url': theme})
        self.add_meta('<meta name="version" content="S5 1.1" />\n')
        self.s5_footer = []
        self.s5_header = []
        self.section_count = 0

    def depart_document(self, node):
        header = ''.join(self.s5_header)
        footer = ''.join(self.s5_footer)
        title = ''.join(self.html_title).replace('<h1 class="title">', '<h1>')
        layout = self.layout_template % {'header': header,
                                         'title': title,
                                         'footer': footer}
        self.fragment.extend(self.body)
        self.body_prefix.extend(layout)
        self.body_prefix.append('<div class="presentation">\n')
        self.body_prefix.append(self.starttag(node, 'div', CLASS='slide'))
        self.body_suffix.insert(0, '</div>\n')
        # skip content-type meta tag with interpolated charset value:
        self.html_head.extend(self.head[1:])
        self.html_body.extend(self.body_prefix[1:] + self.body_pre_docinfo
                              + self.docinfo + self.body
                              + self.body_suffix[:-1])

#     def visit_footer(self, node):
#         # check for one paragraph?
        
    def depart_footer(self, node):
        start = self.context.pop()
        self.s5_footer.append('<h2>')
        self.s5_footer.extend(self.body[start:])
        self.s5_footer.append('</h2>')
        del self.body[start:]

    def depart_header(self, node):
        start = self.context.pop()
        header = ['<div id="header">\n']
        header.extend(self.body[start:])
        header.append('\n</div>\n')
        del self.body[start:]
        self.s5_header.extend(header)

    def visit_section(self, node):
        if not self.section_count:
            self.body.append('\n</div>\n')
        self.section_count += 1
        self.section_level += 1
        if self.section_level > 1:
            # dummy for matching div's
            self.body.append(self.start_tag_with_title(
                node, 'div', CLASS='section'))
        else:
            self.body.append(self.start_tag_with_title(
                node, 'div', CLASS='slide'))
