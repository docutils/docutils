"""
LaTeX Beamer document tree Writer.
"""

__docformat__ = 'reStructuredText'

from docutils import frontend, nodes
from docutils.writers import latex2e
from docutils.readers import standalone
from docutils.transforms import references, Transform, TransformError

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
         # TODO Themes must be changeable by ``usecolortheme``,
         # ``usefonttheme``, ``useinnertheme``, and ``useoutertheme``
         # separately
         ('LaTeX Beamer theme to use. '
          'Default is JuanLesPins.',
          ['--beamer-theme'],
          {'default': 'JuanLesPins'}),
         ('Insert intermediate outline slides at a certain level. '
          'Possible values are "section" and "subsection". '
          'Default is no intermediate outline slides.',
          ['--intermediate-outlines'],
          {'choices': ['section', 'subsection',],
           'default': ''}),
         # TODO Some themes accept options which must be supported here as
         # plain strings for those who know what they do
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
                                 # TODO Argument to `setbeamercovered` must be
                                 # an option
                                 '  \\setbeamercovered{transparent}\n',
                                 '}\n',
                                 '\n',
                                 ))
        # TODO Name of outline slides should be the name given for the contents
        # (is a `title` element) - might be determined by a preprocessor
        document.settings.toc_title = 'Outline'
        # TODO Must be present only if there is a toc
        if document.settings.intermediate_outlines:
            if document.settings.intermediate_outlines == 'section':
                at_begin = 'AtBeginSection'
                toc_option = 'currentsection'
            else:
                at_begin = 'AtBeginSubsection'
                toc_option = 'currentsection,currentsubsection'
            self.head_prefix.extend(('\n',
                                     '\\%s[]\n' % ( at_begin, ),
                                     '{\n',
                                     '  \\begin{frame}<beamer>\n',
                                     '    \\frametitle{%s}\n' % ( document.settings.toc_title, ),
                                     '    \\tableofcontents[%s]\n' % ( toc_option, ),
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
            self.body.append( '\n\\begin{frame}\n  \\frametitle{%s}\n  \\tableofcontents\n\\end{frame}'
                              % ( self.document.settings.toc_title, ))
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

# Use an own reader to modify transformations done.
class Reader(standalone.Reader):

    def get_transforms(self):
        default = standalone.Reader.get_transforms(self)
        return ([ i
                  for i in default
                  if i is not references.DanglingReferences ]
                + [ RemoveClassHandout, ])

# TODO Must be supported by the right beamer mode (see section 20.2)
class RemoveClassHandout(Transform):

    """
    Remove all elements with a given class attribute.
    """

    # TODO This should be more generic
    classToRemove = 'handout'

    # Must be less than
    # docutils.transforms.misc.ClassAttribute.default_priority
    default_priority = 120

    def apply(self):
        for node in self.document.traverse(nodes.Element):
            if node.has_key('classes'):
                if self.classToRemove in node['classes']:
                    if node.parent:
                        node.parent.remove(node)

# TODO \pause needs to be supported as an inline element; the following works
# but is ugly::
#
#   .. |p| raw:: latex
#
#      \pause
#
# It also is problematic because it ends up in an error if ``raw`` is disabled
# - even when rendering HTML
#
# S5 solves this problem purely by classes and ::
#
#   .. role:: incremental
#   .. default-role:: incremental
#
# in s5defs.txt. Semantics seems to be a \pause before the marked up text.
# Unfortunately then the role is used and things like `*emphasis*` don't work.
#
# If there would be a class ``pause`` implemented by `latexbeamer` then
# something like this could work::
#
#   .. |p| class:: pause
#
#   Pause |p| as much |p| as you like |p|
#
#   and where |p| you like |p|
#
#   and use *any* |p| other role flexibly.
#
# However, any element needs to be checked for the respective class then. A
# construct like ::
#
#   .. class:: pause
#
#      * First point
#
#      * Second point
#
# Unfortunately this doesn't work :-( .

# TODO *emphasis* must translate to bold text and **strong emphasis** to \alert

# TODO \logo picture must be supported as an option

# TODO \appendix must be supported as a directive
