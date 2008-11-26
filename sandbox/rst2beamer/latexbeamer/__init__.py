"""
LaTeX Beamer document tree Writer.
"""

__docformat__ = 'reStructuredText'

from docutils import nodes
from docutils.writers import latex2e

class Writer(latex2e.Writer):

    supported = ('latexbeamer')
    """Formats this writer supports."""

    def __init__(self):
        latex2e.Writer.__init__(self)
        self.translator_class = BeamerTranslator

class DocumentClass(latex2e.DocumentClass):
    """Details of a LaTeX document class."""

    def __init__(self):
        pass

    def section(self, level):
        """ Return the section name at the given level for the specific
            document class.

            Level is 1,2,3..., as level 0 is the title."""

        # TODO Level where sections become frames must be an option
        sections = [ 'section', "begin{frame}\n\\frametitle", ]
        if level <= len(sections):
            return sections[level-1]
        else:
            return sections[-1]

    def section_end(self, level):
        """ Return the section name at the given level for the specific
            document class.

            Level is 1,2,3..., as level 0 is the title."""

        # TODO Level where sections become frames are must be an option
        sections = [ '', "\n\\end{frame}\n", ]
        if level <= len(sections):
            return sections[level-1]
        else:
            return sections[-1]

class BeamerTranslator(latex2e.LaTeXTranslator):

    def __init__(self, document):
        # TODO Should be done via explicit settings
        document.settings.documentclass = 'beamer'
        # TODO Should be done via explicit settings - should be none but the
        # language
        document.settings.documentoptions = '10pt'
        # TODO Should be done via explicit settings
        document.settings.use_latex_docinfo = 1
        latex2e.LaTeXTranslator.__init__(self, document)
        # TODO Should be done via explicit head_prefix
        self.head_prefix = [ line
                for line in self.head_prefix
                if 'typearea' not in line and 'hyperref' not in line ]
        # TODO Theme must be an option
        # Montpellier, Warsaw, JuanLesPins, Darmstadt, Antibes
        self.head_prefix.extend(('\n',
                                 '\\mode<presentation>\n',
                                 '{\n',
                                 '  \\usetheme{JuanLesPins}\n',
                                 '  \\setbeamercovered{transparent}\n',
                                 '}\n',
                                 '\n',
                                 ))
        # TODO Must be an option including the level where this happens
        self.head_prefix.extend(('\n',
                                 '\\AtBeginSection[]\n',
                                 '{\n',
                                 '  \\begin{frame}<beamer>\n',
                                 '    \\frametitle{Outline}\n',
                                 '    \\tableofcontents[currentsection,currentsubsection]\n',
                                 '  \\end{frame}\n',
                                 '}\n',
                                 ))
        self.d_class = DocumentClass()
        self.subtitle = ''

    def visit_docinfo(self, node):
        self.docinfo = []

    def depart_docinfo(self, node):
        self.docinfo = None
        # Do not output this because relevant data has been gathered

    def visit_topic(self, node):
        self.topic_classes = node['classes']
        if 'contents' in node['classes']:
            # TODO Name of outline slide must be an option and could be the
            # name given for the contents
            self.body.append( '\n\\begin{frame}\n  \\frametitle{Outline}\n  \\tableofcontents\n\\end{frame}')
            self.topic_classes = []
            raise nodes.SkipNode

    def depart_topic(self, node):
        self.topic_classes = []
        self.body.append('\n')

    def visit_title(self, node):
        """Section and other titles."""

        if (isinstance(node.parent, nodes.topic)
            or isinstance(node.parent, nodes.sidebar)
            or isinstance(node.parent, nodes.admonition)
            or isinstance(node.parent, nodes.table)
            or self.section_level == 0):
            latex2e.LaTeXTranslator.visit_title(self, node)
        else:
            self.body.append('\n\n')
            self.body.append('%' + '_' * 75)
            self.body.append('\n\n')
            self.bookmark(node)

            section_name = self.d_class.section(self.section_level)
            self.body.append('\\%s{' % (section_name, ))
            self.context.append('}\n')

    def visit_subtitle(self, node):
        if isinstance(node.parent, nodes.document):
            self.subtitle = self.encode(node.astext())
            raise nodes.SkipNode
        else:
            latex2e.LaTeXTranslator.visit_subtitle(self, node)

    def depart_section(self, node):
        self.body.append(self.d_class.section_end(self.section_level))
        latex2e.LaTeXTranslator.depart_section(self, node)

    def astext(self):
        if self.pdfinfo is not None:
            if self.pdfauthor:
                self.pdfinfo.append('pdfauthor={%s}' % self.pdfauthor)
        if self.pdfinfo:
            pdfinfo = '\\hypersetup{\n' + ',\n'.join(self.pdfinfo) + '\n}\n'
        else:
            pdfinfo = ''
        head = '\\title{%s}\n\\subtitle{%s}\n\\author{%s}\n\\date{%s}\n' % \
               (self.title, self.subtitle,
                ' \\and\n'.join(['~\\\\\n'.join(author_lines)
                                 for author_lines in self.author_stack]),
                self.date)
        return ''.join(self.head_prefix + [head] + self.head + [pdfinfo]
                        + self.body_prefix  + self.body + self.body_suffix)

