#! /usr/bin/env python

# Author: engelbert gruber
# Contact: grubert@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for latex2e writer.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.LatexPublishTestSuite()
    s.generateTests(totest)
    return s


latex_head = """\
\\documentclass[10pt,english]{article}
\\usepackage{babel}
\\usepackage{shortvrb}
\\usepackage[latin1]{inputenc}
\\usepackage{tabularx}
\\usepackage{longtable}
\\setlength{\\extrarowheight}{2pt}
\\usepackage{amsmath}
\\usepackage{graphicx}
\\usepackage{color}
\\usepackage{multirow}
\\usepackage[colorlinks,linkcolor=blue]{hyperref}
\\usepackage[a4paper,margin=2cm,nohead]{geometry}
%% generator Docutils: http://docutils.sourceforge.net/
\\newlength{\\admwidth}
\\addtolength{\\admwidth}{0.9\\textwidth}
\\newcommand{\\optionlistlabel}[1]{\\bf #1 \\hfill}
\\newenvironment{optionlist}[1]
{\\begin{list}{}
  {\\setlength{\\labelwidth}{#1}
   \\setlength{\\rightmargin}{1cm}
   \\setlength{\\leftmargin}{\\rightmargin}
   \\addtolength{\\leftmargin}{\\labelwidth}
   \\addtolength{\\leftmargin}{\\labelsep}
   \\renewcommand{\\makelabel}{\\optionlistlabel}}
}{\\end{list}}
% begin: floats for footnotes tweaking.
\\setlength{\\floatsep}{0.5em}
\\setlength{\\textfloatsep}{\\fill}
\\addtolength{\\textfloatsep}{3em}
\\renewcommand{\\textfraction}{0.5}
\\renewcommand{\\topfraction}{0.5}
\\renewcommand{\\bottomfraction}{0.5}
\\setcounter{totalnumber}{50}
\\setcounter{topnumber}{50}
\\setcounter{bottomnumber}{50}
% end floats for footnotes
\\input{style.tex}
"""

totest = {}

totest['empty file'] = [
[
"""\

""",
latex_head + """\
\\title{}
\\author{}
\\date{}
\\raggedbottom
\\begin{document}
\\maketitle


Document empty; must have contents.


\\end{document}
"""
],
]

totest['tables_of_contents'] = [
["""\
.. contents:: Table of Contents

Title 1
=======
Paragraph 1.

Title 2
-------
Paragraph 2.
""",
latex_head + """\
\\title{Title 1}
\\author{}
\\date{}
\\hypersetup{\npdftitle={Title 1}
}
\\raggedbottom
\\begin{document}
\\maketitle

\\hypertarget{table-of-contents}{}\\begin{center}
\\subsection*{Table of Contents}
\\end{center}
\\pdfbookmark[0]{Table of Contents}{table-of-contents}
\\begin{list}{}{}
\\item \\href{#title-2}{Title 2}

\\end{list}


Paragraph 1.


%___________________________________________________________________________

\\hypertarget{title-2}{}
\\section*{Title 2}
\\pdfbookmark[0]{Title 2}{title-2}

Paragraph 2.

\\end{document}
"""],

]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
