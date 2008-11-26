"""
LaTeX Beamer document tree Writer.
"""

__docformat__ = 'reStructuredText'

from docutils import frontend, nodes
from docutils.writers import latex2e

class Writer(latex2e.Writer):

    supported = ('latexbeamer')
    """Formats this writer supports."""

    settings_spec = (
        'LaTeX Beamer Specific Options',
        'The LaTeX "--output-encoding" default is "latin-1:strict".',
        (('Specify document options.  Multiple options can be given, '
          'separated by commas.  Default is no options.',
          ['--documentoptions'],
          {'default': '', }),
         ('Use LaTeX footnotes. LaTeX supports only numbered footnotes (does it?). '
          'Default: no, uses figures.',
          ['--use-latex-footnotes'],
          {'default': 0, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Format for footnote references: one of "superscript" or '
          '"brackets".  Default is "superscript".',
          ['--footnote-references'],
          {'choices': ['superscript', 'brackets'], 'default': 'superscript',
           'metavar': '<format>',
           'overrides': 'trim_footnote_reference_space'}),
         ('Use LaTeX citations. '
          'Default: no, uses figures which might get mixed with images.',
          ['--use-latex-citations'],
          {'default': 0, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Format for block quote attributions: one of "dash" (em-dash '
          'prefix), "parentheses"/"parens", or "none".  Default is "dash".',
          ['--attribution'],
          {'choices': ['dash', 'parentheses', 'parens', 'none'],
           'default': 'dash', 'metavar': '<format>'}),
         ('Specify a stylesheet file. The file will be "input" by latex in '
          'the document header.  Default is no stylesheet ("").  '
          'Overrides --stylesheet-path.',
          ['--stylesheet'],
          {'default': '', 'metavar': '<file>',
           'overrides': 'stylesheet_path'}),
         ('Specify a stylesheet file, relative to the current working '
          'directory.  Overrides --stylesheet.',
          ['--stylesheet-path'],
          {'metavar': '<file>', 'overrides': 'stylesheet'}),
         ('Table of contents by docutils (default) or latex. Latex (writer) '
          'supports only one ToC per document, but docutils does not write '
          'pagenumbers.',
          ['--use-latex-toc'],
          {'default': 0, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Color of any hyperlinks embedded in text '
          '(default: "0" (disabled)).',
          ['--hyperlink-color'], {'default': '0'}),
         ('Enable compound enumerators for nested enumerated lists '
          '(e.g. "1.2.a.ii").  Default: disabled.',
          ['--compound-enumerators'],
          {'default': None, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Disable compound enumerators for nested enumerated lists.  This is '
          'the default.',
          ['--no-compound-enumerators'],
          {'action': 'store_false', 'dest': 'compound_enumerators'}),
         ('Enable section ("." subsection ...) prefixes for compound '
          'enumerators.  This has no effect without --compound-enumerators.  '
          'Default: disabled.',
          ['--section-prefix-for-enumerators'],
          {'default': None, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Disable section prefixes for compound enumerators.  '
          'This is the default.',
          ['--no-section-prefix-for-enumerators'],
          {'action': 'store_false', 'dest': 'section_prefix_for_enumerators'}),
         ('Set the separator between section number and enumerator '
          'for compound enumerated lists.  Default is "-".',
          ['--section-enumerator-separator'],
          {'default': '-', 'metavar': '<char>'}),
         ('When possibile, use verbatim for literal-blocks. '
          'Default is to always use the mbox environment.',
          ['--use-verbatim-when-possible'],
          {'default': 0, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Table style. "standard" with horizontal and vertical lines, '
          '"booktabs" (LaTeX booktabs style) only horizontal lines '
          'above and below the table and below the header or "nolines".  '
          'Default: "standard"',
          ['--table-style'],
          {'choices': ['standard', 'booktabs','nolines'], 'default': 'standard',
           'metavar': '<format>'}),
         ('LaTeX graphicx package option. '
          'Possible values are "dvips", "pdftex". "auto" includes LaTeX code '
          'to use "pdftex" if processing with pdf(la)tex and dvips otherwise. '
          'Default is no option.',
          ['--graphicx-option'],
          {'default': ''}),
         ('LaTeX font encoding. '
          'Possible values are "T1", "OT1", "" or some other fontenc option. '
          'The font encoding influences available symbols, e.g. "<<" as one '
          'character. Default is "" which leads to package "ae" (a T1 '
          'emulation using CM fonts).',
          ['--font-encoding'],
          {'default': ''}),
         ('LaTeX Beamer theme to use. '
          'Default is JuanLesPins.',
          ['--beamer-theme'],
          {'default': 'JuanLesPins'})
          ),)

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

        # TODO Level where sections become frames must be an option - might be
        # determined by a preprocessor, then it is also not fixed - might be
        # driven by the first content under a section
        sections = [ 'section', "begin{frame}\n\\frametitle", ]
        if level <= len(sections):
            return sections[level-1]
        else:
            return sections[-1]

    def section_end(self, level):
        """ Return the section name at the given level for the specific
            document class.

            Level is 1,2,3..., as level 0 is the title."""

        # TODO See above
        sections = [ '', "\n\\end{frame}\n", ]
        if level <= len(sections):
            return sections[level-1]
        else:
            return sections[-1]

class BeamerTranslator(latex2e.LaTeXTranslator):

    def __init__(self, document):
        document.settings.documentclass = 'beamer'
        document.settings.use_latex_docinfo = 1
        latex2e.LaTeXTranslator.__init__(self, document)
        # TODO Should be done via explicit head_prefix
        self.head_prefix = [ line
                for line in self.head_prefix
                if 'typearea' not in line and 'hyperref' not in line ]
        # Montpellier, Warsaw, JuanLesPins, Darmstadt, Antibes
        self.head_prefix.extend(('\n',
                                 '\\mode<presentation>\n',
                                 '{\n',
                                 '  \\usetheme{%s}\n'
                                 % ( document.settings.beamer_theme, ),
                                 '  \\setbeamercovered{transparent}\n',
                                 '}\n',
                                 '\n',
                                 ))
        # TODO Must be an option including the level where this happens - must
        # be present only if there is a toc
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
        # TODO :Organization: should be handled as institute

    def depart_docinfo(self, node):
        self.docinfo = None
        # Do not output this because relevant data has been gathered

    def visit_topic(self, node):
        self.topic_classes = node['classes']
        if 'contents' in node['classes']:
            # TODO Name of outline slide must be an option and could be the
            # name given for the contents (is a `title` element) - might be
            # determined by a preprocessor
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

# TODO Class `handout` must be suppressed - may be by a preprocessor
