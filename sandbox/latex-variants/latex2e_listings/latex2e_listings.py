# Author: Guenter Milde
# Contact: milde@users.berlios.de
# Revision: $Revision$
# Date: $Date: 2005-06-28$
# Copyright: Licensed under the Academic Free License version 1.2

"""
LaTeX2e document tree Writer 
supporting the `lstlisting` environment for literal blocks
"""

__docformat__ = 'reStructuredText'


import docutils
from docutils import frontend, nodes, utils, writers, languages

from docutils.writers import latex2e

class Writer(latex2e.Writer):

    settings_spec = (
        'LaTeX-Specific Options',
        'The LaTeX "--output-encoding" default is "latin-1:strict".',
        (('Specify documentclass.  Default is "article".',
          ['--documentclass'],
          {'default': 'article', }),
         ('Specify document options.  Multiple options can be given, '
          'separated by commas.  Default is "10pt,a4paper".',
          ['--documentoptions'],
          {'default': '10pt,a4paper', }),
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
         ('Table of contents by docutils (default) or LaTeX. LaTeX (writer) '
          'supports only one ToC per document, but docutils does not know of '
          'pagenumbers. LaTeX table of contents also means LaTeX generates '
          'sectionnumbers.',
          ['--use-latex-toc'],
          {'default': 0, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Add parts on top of the section hierarchy.',
          ['--use-part-section'],
          {'default': 0, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Let LaTeX print author and date, do not show it in docutils '
          'document info.',
          ['--use-latex-docinfo'],
          {'default': 0, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Use LaTeX abstract environment for the documents abstract.'
          'Per default the abstract is an unnumbered section.',
          ['--use-latex-abstract'],
          {'default': 0, 'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Color of any hyperlinks embedded in text '
          '(default: "blue", "0" to disable).',
          ['--hyperlink-color'], {'default': 'blue'}),
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
         ('Overridden in this writer variant.',
          ['--use-verbatim-when-possible'],
          {'default': 1, 'action': 'store_true',
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
         ('Per default the latex-writer puts the reference title into '
          'hyperreferences. Specify "ref*" or "pageref*" to get the section '
          'number or the page number.',
          ['--reference-label'],
          {'default': None, }),
         ('Specify style and database for bibtex, for example '
          '"--use-bibtex=mystyle,mydb1,mydb2".',
          ['--use-bibtex'],
          {'default': None, }),
          ),)

    settings_defaults = {'output_encoding': 'latin-1'}

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = LaTeXTranslator


class LaTeXTranslator(latex2e.LaTeXTranslator):
    """
    This LaTeX writer has been customized to use the `lstlisting` 
    environment for literal blocks.
    
    You almost certainly want to use a style-sheet with it.
    """
    
    # Package loading
    literal_block_package = "\\RequirePackage{listings}\n"
    # argument to the ``\begin{}`` ``\end{}`` pair
    literal_block_environment = "lstlisting"
    # TODO: make this a command line option and a dictionary
    # until then, override in the wrapper script

    def __init__(self, document):
        latex2e.LaTeXTranslator.__init__(self, document)
        self.head_prefix.append(self.literal_block_package)
    
    def visit_doctest_block(self, node):
        self.body.append( '\\begin{%s}\n' % self.literal_block_environment)
        self.verbatim = 1

    def depart_doctest_block(self, node):
        self.body.append( '\\end{%s}\n' % self.literal_block_environment )
        self.verbatim = 0


    def visit_literal_block(self, node):
        """
        Render a literal-block.

        Literal-blocks doctree elements are used for "::"-prefixed 
        indented blocks of text, where the inline markup is not recognized,
        but are also the product of the parsed-literal directive,
        where the markup is respected.
        """
        # In both cases, we want to use a typewriter/monospaced typeface.
        # For "real" literal-blocks, we can use \verbatim, while for all
        # the others we must use the `alltt` environment.
        #
        # We can distinguish between the two kinds by the number of
        # siblings that compose this node: if it is composed by a
        # single element, it's surely either a real one or a
        # parsed-literal that does not contain any markup.
        #
        if ((len(node) == 1)
            # in case of a parsed-literal containing just a "**bold**" word:
            and isinstance(node[0], nodes.Text)):
            self.verbatim = 1
            self.body.append('\\begin{%s}\n' % self.literal_block_environment)
            self.context.append('\\end{%s}\n' % self.literal_block_environment)
        else:
            self.literal_block = 1
            self.insert_none_breaking_blanks = 1
            if self.active_table.is_open():
                # no quote inside tables, to avoid vertical space between
                # table border and literal block.
                # BUG: fails if normal text preceeds the literal block.
                self.body.append('\n')
                self.context.append('\n')
            else:
                self.body.append('\\begin{quote}\n')
                self.context.append('\\end{quote}\n')
            # TODO: use alltt environment
            self.body.append('{\\ttfamily \\raggedright \\noindent\n')
            # * obey..: is from julien and never worked for me (grubert).
            #   self.body.append('{\\obeylines\\obeyspaces\\ttfamily\n')


    def depart_literal_block(self, node):
        self.verbatim = 0
        self.insert_none_breaking_blanks = 0
        self.literal_block = 0 
        self.body.append(self.context.pop())




# Testing
# -------

# if __name__ == '__main__':
#     try:
#         import test_html4trans
#     except ImportError:
#         pass
