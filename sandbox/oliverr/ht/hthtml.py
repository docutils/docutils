# Author: Ollie Rutherfurd
# Contact: oliver@rutherfurd.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Simple .ht (HyperText Template) document tree Writer.

.ht tmeplate files are essentially normal HTML, with
an option set of RFC 2822-like headers at the top of
the file. There must be at least one blank line between
the last header and the start of the body HTML.

See http://ht2html.sf.net/ for more information on
.ht files and ht2html..
"""

__docformat__ = 'reStructuredText'

import os
from docutils import nodes
from docutils import writers
from docutils.writers.html4css1 import HTMLTranslator


class Writer(writers.Writer):

    supported = ('ht',)
    """Formats this writer supports."""

    settings_spec = (
        '.ht template-Specific Options',
        None,
        (('Specify a stylesheet URL, used verbatim.  Default is '
          '"default.css".',
          ['--stylesheet'],
          {'default': 'default.css', 'metavar': '<URL>'}),
         ('Specify a stylesheet file, relative to the current working '
          'directory.  The path is adjusted relative to the output HTML '
          'file.  Overrides --stylesheet.',
          ['--stylesheet-path'],
          {'metavar': '<file>'}),
         ('Format for footnote references: one of "superscript" or '
          '"brackets".  Default is "superscript".',
          ['--footnote-references'],
          {'choices': ['superscript', 'brackets'], 'default': 'superscript',
           'metavar': '<FORMAT>'}),
         ('Remove extra vertical whitespace between items of bullet lists '
          'and enumerated lists, when list items are "simple" (i.e., all '
          'items each contain one paragraph and/or one "simple" sublist '
          'only).  Default: enabled.',
          ['--compact-lists'],
          {'default': 1, 'action': 'store_true'}),
         ('Disable compact simple bullet and enumerated lists.',
          ['--no-compact-lists'],
          {'dest': 'compact_lists', 'action': 'store_false'}),))

    relative_path_settings = ('stylesheet_path',)

    output = None

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = HTTranslator

    def translate(self):
        visitor = self.translator_class(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.astext()
        self.stylesheet = visitor.stylesheet
        self.body = visitor.body


class HTTranslator(HTMLTranslator):

    def __init__(self, document):

        # I don't believe we can embed any style content
        # the header, so always link to the stylesheet.
        document.settings.embed_stylesheet = 0

        HTMLTranslator.__init__(self, document)
        self.headers = {}
        stylesheet = self.get_stylesheet_reference(os.getcwd())
        if stylesheet:
            self.headers['stylesheet']= stylesheet
        self.has_author = 0

    def astext(self):
        headers = ''.join(['%s: %s\n' % (k,v) \
            for (k,v) in self.headers.items()])
        title = '<h1 class="title">%(title)s</h1>\n' % self.headers
        body = [title,] + self.docinfo + self.body

        return ''.join([headers + '\n'] + body)

    def visit_author(self, node):
        if not self.headers.has_key('author'):
            self.headers['author'] = self.encode(node.astext())
        HTMLTranslator.visit_author(self, node)

    def visit_contact(self, node):
        if not self.headers.has_key('author-email'):
            self.headers['author-email'] = self.encode(node.astext())
        HTMLTranslator.visit_contact(self, node)

    def visit_title(self, node):
        """Only 6 section levels are supported by HTML."""
        if isinstance(node.parent, nodes.topic):
            HTMLTranslator.visit_title(self, node)
        elif self.section_level == 0:
            # document title
            title = node.astext()
            self.headers['title'] = self.encode(title)
            self.context.append('\n')
        else:
            HTMLTranslator.visit_title(self, node)


# :indentSize=4:lineSeparator=\n:noTabs=true:tabSize=4:
