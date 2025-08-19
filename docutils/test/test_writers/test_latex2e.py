#! /usr/bin/env python3
# $Id$
# Author: engelbert gruber <grubert@users.sourceforge.net>
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
Tests for latex2e writer.

This module tests only the "body" part of the output.
For tests of constructs that change the "head", see test-latex2e_parts.py.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.core import publish_parts
from docutils.writers import latex2e


class WriterPublishTestCase(unittest.TestCase):

    maxDiff = None
    settings = {
        '_disable_config': True,
        'strict_visitor': True,
        # avoid latex writer future warnings:
        'use_latex_citations': False,
        'legacy_column_widths': True,
        }

    def test_body(self):
        for name, (settings_overrides, cases) in samples.items():
            for casenum, (rst_input, expected) in enumerate(cases):
                with self.subTest(id=f'samples[{name!r}][{casenum}]'):
                    output = publish_parts(
                        source=rst_input,
                        writer=latex2e.Writer(),
                        settings_overrides=self.settings|settings_overrides
                        )['body']
                    self.assertEqual(expected, output)


samples = {}


samples['default'] = ({}, [
# url chars
['http://nowhere/url_with%28parens%29',
 '\n\\url{http://nowhere/url_with\\%28parens\\%29}\n'
],
# enumerated lists
["""\
1. Item 1.
2. Second to the previous item this one will explain

  a) nothing.
  b) or some other.

3. Third is

  (I) having pre and postfixes
  (II) in roman numerals.
""",
r"""
\begin{enumerate}
\item Item 1.

\item Second to the previous item this one will explain
\end{enumerate}

\begin{quote}
\begin{enumerate}
\renewcommand{\labelenumi}{\alph{enumi})}
\item nothing.

\item or some other.
\end{enumerate}
\end{quote}

\begin{enumerate}
\setcounter{enumi}{2}
\item Third is
\end{enumerate}

\begin{quote}
\begin{enumerate}
\renewcommand{\labelenumi}{(\Roman{enumi})}
\item having pre and postfixes

\item in roman numerals.
\end{enumerate}
\end{quote}
"""],
# bracket protection
["""
* [no option] to this item
""",
r"""
\begin{itemize}
\item {[}no option{]} to this item
\end{itemize}
"""],
# raw directive and role
[r""".. raw:: latex

   $E=mc^2$

A paragraph.

.. |sub| raw:: latex

   (some raw text)

Foo |sub|
same paragraph.
""",
r"""
$E=mc^2$

A paragraph.

Foo (some raw text)
same paragraph.
"""],
# images and figures
["""
.. image:: larch-mini.jpg
   :target: larch.jpg
   :align: center
""",
r"""
\noindent\makebox[\linewidth][c]{\href{larch.jpg}{\includegraphics{larch-mini.jpg}}}
"""],
["""\
.. figure:: larch-mini.jpg
   :target: larch.jpg

   The larch
""",
r"""
\begin{figure}
\noindent\makebox[\linewidth][c]{\href{larch.jpg}{\includegraphics{larch-mini.jpg}}}
\caption{The larch}
\end{figure}
"""],
# tables
# ======
# borderless table
["""\
.. table::
   :class: borderless

   +-----+-----+
   |  1  |  2  |
   +-----+-----+
   |  3  |  4  |
   +-----+-----+
""",
"""
\\setlength{\\DUtablewidth}{\\linewidth}%
\\begin{longtable*}{p{0.075\\DUtablewidth}p{0.075\\DUtablewidth}}

1
 & \n\
2
 \\\\

3
 & \n\
4
 \\\\
\\end{longtable*}
"""],
# collumn widths determined by LaTeX (via class argument)
["""\
.. table::
   :class: colwidths-auto

   +-----+-+
   |  1  |2|
   +-----+-+
""",
r"""
\begin{longtable*}{|l|l|}
\hline
1 & 2 \\
\hline
\end{longtable*}
"""],
# collumn widths determined by LaTeX (via "width" option)
["""\
.. table::
   :widths: auto

   +-----+-+
   |  1  |2|
   +-----+-+
""",
r"""
\begin{longtable*}{|l|l|}
\hline
1 & 2 \\
\hline
\end{longtable*}
"""],
# collumn widths specified via "width" option
["""\
.. table::
   :widths: 15, 30

   +-----+-----+
   |  1  |  2  |
   +-----+-----+
""",
"""
\\setlength{\\DUtablewidth}{\\linewidth}%
\\begin{longtable*}{|p{0.191\\DUtablewidth}|p{0.365\\DUtablewidth}|}
\\hline

1
 & \n\
2
 \\\\
\\hline
\\end{longtable*}
"""],
# table alignment
["""\
.. table::
   :align: right

   +-----+-----+
   |  1  |  2  |
   +-----+-----+
""",
"""
\\setlength{\\DUtablewidth}{\\linewidth}%
\\begin{longtable*}[r]{|p{0.075\\DUtablewidth}|p{0.075\\DUtablewidth}|}
\\hline

1
 & \n\
2
 \\\\
\\hline
\\end{longtable*}
"""],
# table with title row and empty cell
["""\
===== ======
Title
===== ======
entry value1
===== ======
""",
"""
\\setlength{\\DUtablewidth}{\\linewidth}%
\\begin{longtable*}{|p{0.075\\DUtablewidth}|p{0.086\\DUtablewidth}|}
\\hline
\\textbf{%
Title
} &  \\\\
\\hline
\\endfirsthead
\\hline
\\textbf{%
Title
} &  \\\\
\\hline
\\endhead
\\multicolumn{2}{p{0.16\\DUtablewidth}}{\\raggedleft\\ldots continued on next page}\\\\
\\endfoot
\\endlastfoot

entry
 & \n\
value1
 \\\\
\\hline
\\end{longtable*}
"""],
# table with emtpy rowspanning cell
["""\
+----+----+
| c3 | c4 |
+----+----+
|         |
+---------+
""",
"""
\\setlength{\\DUtablewidth}{\\linewidth}%
\\begin{longtable*}{|p{0.063\\DUtablewidth}|p{0.063\\DUtablewidth}|}
\\hline

c3
 & \n\
c4
 \\\\
\\hline
\\multicolumn{2}{|p{0.13\\DUtablewidth}|}{} \\\\
\\hline
\\end{longtable*}
"""],
# Test handling of IDs and custom class values
# --------------------------------------------
# targets with ID
["""\
A paragraph with _`inline target`.

.. _block target:

.. class:: custom paragraph

Next paragraph.
""",
r"""
A paragraph with %
\phantomsection\label{inline-target}inline target.

\phantomsection\label{block-target}
\DUrole{custom}{\DUrole{paragraph}{Next paragraph.}}
"""],
# table with IDs and custom + special class values
["""\
.. class:: cls1
.. _label1:
.. table::
   :name: label2
   :class: cls2 borderless
   :widths: auto

   = =
   Y N
   = =
""",
r"""
\begin{DUclass}{cls2}
\begin{DUclass}{cls1}
\phantomsection\label{label2}\label{label1}
\begin{longtable*}{ll}
Y & N \\
\end{longtable*}
\end{DUclass}
\end{DUclass}
"""],
])

samples['latex_sectnum'] = ({'sectnum_xform': False}, [
["""\
.. sectnum::

some text

first section
-------------
""",
r"""
some text


\section{first section%
  \label{first-section}%
}
"""],
])


samples['latex_citations'] = ({'use_latex_citations': True}, [
["""\
Just a test citation [my_cite2006]_.

.. [my_cite2006]
   Watch the underscore!
""",
r"""
Just a test citation \cite{my_cite2006}.

\begin{thebibliography}{my\_cite2006}
\bibitem[my\_cite2006]{my_cite2006}{
Watch the underscore!
}
\end{thebibliography}
"""],
["""\
Two non-citations: [MeYou2007]_[YouMe2007]_.

Need to be separated for grouping: [MeYou2007]_ [YouMe2007]_.

Two spaces (or anything else) for no grouping: [MeYou2007]_  [YouMe2007]_.

But a line break should work: [MeYou2007]_
[YouMe2007]_.

.. [MeYou2007] not.
.. [YouMe2007] important.
""",
r"""
Two non-citations: {[}MeYou2007{]}\_{[}YouMe2007{]}\_.

Need to be separated for grouping: \cite{MeYou2007,YouMe2007}.

Two spaces (or anything else) for no grouping: \cite{MeYou2007}  \cite{YouMe2007}.

But a line break should work: \cite{MeYou2007,YouMe2007}.

\begin{thebibliography}{MeYou2007}
\bibitem[MeYou2007]{MeYou2007}{
not.
}
\bibitem[YouMe2007]{YouMe2007}{
important.
}
\end{thebibliography}
"""],
])


samples['colwidths_auto'] = ({'table_style': ['colwidths-auto']}, [
# Requires longtable, etc. and setup --- see "test_latex2e_parts".
# borderless table with auto-width columns
["""\
.. table::
   :class: borderless

   +-----+-----+
   |  1  |  2  |
   +-----+-----+
   |  3  |  4  |
   +-----+-----+
""",
r"""
\begin{longtable*}{ll}
1 & 2 \\
3 & 4 \\
\end{longtable*}
"""],
# booktabs style table with auto-width columns
["""\
.. table::
   :class: booktabs

   +-----+-+
   |  1  |2|
   +-----+-+
""",
r"""
\begin{longtable*}{ll}
\toprule
1 & 2 \\
\bottomrule
\end{longtable*}
"""],
# given width overrides "colwidth-auto"
["""\
.. table::
   :widths: 15, 30

   +-----+-----+
   |  1  |  2  |
   +-----+-----+
""",
"""
\\setlength{\\DUtablewidth}{\\linewidth}%
\\begin{longtable*}{|p{0.191\\DUtablewidth}|p{0.365\\DUtablewidth}|}
\\hline

1
 & \n\
2
 \\\\
\\hline
\\end{longtable*}
"""],
])


samples['bibtex'] = ({'use_bibtex': ['alpha', 'xampl']}, [
["""\
Just a test citation [book-full]_.
""",
r"""
Just a test citation \cite{book-full}.

\bibliographystyle{alpha}
\bibliography{xampl}
"""],
["""\
No bibliography if there is no citation.
""",
r"""
No bibliography if there is no citation.
"""],
])

samples['docutils-footnotes'] = ({}, [
# different markup variants
[r"""
Paragraphs contain text and may contain footnote references (manually
numbered [1]_, anonymous auto-numbered [#]_, labeled auto-numbered
[#label]_, or symbolic [*]_).

.. [1] A footnote.

.. [#label] Footnotes may be numbered, either manually or
   automatically using a "#"-prefixed label.  This footnote has a
   label so it can be referred to from multiple places, both as a
   footnote reference ([#label]_) and as a `hyperlink reference`__.

   __ label_

.. [#] This footnote is numbered automatically and anonymously using a
   label of "#" only.

.. [*] Footnotes may also use symbols, specified with a "*" label.
""",
 r"""
Paragraphs contain text and may contain footnote references (manually
numbered\DUfootnotemark{footnote-reference-1}{footnote-1}{1}, anonymous auto-numbered\DUfootnotemark{footnote-reference-2}{footnote-2}{3}, labeled auto-numbered\DUfootnotemark{footnote-reference-3}{label}{2}, or symbolic\DUfootnotemark{footnote-reference-4}{footnote-3}{*}).
%
\DUfootnotetext{footnote-1}{footnote-reference-1}{1}{%
A footnote.
}
%
\DUfootnotetext{label}{footnote-reference-3}{2}{%
Footnotes may be numbered, either manually or
automatically using a \textquotedbl{}\#\textquotedbl{}-prefixed label.  This footnote has a
label so it can be referred to from multiple places, both as a
footnote reference (\DUfootnotemark{footnote-reference-5}{label}{2}) and as a \hyperref[label]{hyperlink reference}.
}
%
\DUfootnotetext{footnote-2}{footnote-reference-2}{3}{%
This footnote is numbered automatically and anonymously using a
label of \textquotedbl{}\#\textquotedbl{} only.
}
%
\DUfootnotetext{footnote-3}{footnote-reference-4}{*}{%
Footnotes may also use symbols, specified with a \textquotedbl{}*\textquotedbl{} label.
}
"""],
# nested footnotes
["""\
It's possible to produce nested footnotes in LaTeX. [#]_

.. [#] It takes some work, though. [#]_
.. [#] And don't even get me started on how tricky recursive footnotes
       would be.
""",
r"""
It's possible to produce nested footnotes in LaTeX.\DUfootnotemark{footnote-reference-1}{footnote-1}{1}
%
\DUfootnotetext{footnote-1}{footnote-reference-1}{1}{%
It takes some work, though.\DUfootnotemark{footnote-reference-2}{footnote-2}{2}
}
%
\DUfootnotetext{footnote-2}{footnote-reference-2}{2}{%
And don't even get me started on how tricky recursive footnotes
would be.
}
"""],
# chained footnotes
["""\
It's possible to produce chained footnotes in LaTeX. [#]_

.. [#] They're just a special case of nested footnotes. [#]_
.. [#] A nested footnote is a footnote on a footnote. [#]_
.. [#] This is a footnote on a footnote on a footnote.
""",
r"""
It's possible to produce chained footnotes in LaTeX.\DUfootnotemark{footnote-reference-1}{footnote-1}{1}
%
\DUfootnotetext{footnote-1}{footnote-reference-1}{1}{%
They're just a special case of nested footnotes.\DUfootnotemark{footnote-reference-2}{footnote-2}{2}
}
%
\DUfootnotetext{footnote-2}{footnote-reference-2}{2}{%
A nested footnote is a footnote on a footnote.\DUfootnotemark{footnote-reference-3}{footnote-3}{3}
}
%
\DUfootnotetext{footnote-3}{footnote-reference-3}{3}{%
This is a footnote on a footnote on a footnote.
}
"""],
# multi-nested footnotes
["""\
A footnote [#multi]_

.. [#multi] This is a footnote with nested [#]_ footnotes. [#]_ [#]_

.. [#] First nested [#]_ footnote. [#]_

.. [#] Second nested footnote. [#]_

.. [#] Third nested footnote.

.. [#] First double-nested footnote.

.. [#] Second double-nested footnote.

.. [#] First triple-nested footnote.

.. [#] Not nested, referenced after footnote text.

Ref to a new footnote [#]_

A second reference to the first one [#multi]_.
We can also write a hyperlink to multi_.
""",
r"""
A footnote\DUfootnotemark{footnote-reference-1}{multi}{1}
%
\DUfootnotetext{multi}{footnote-reference-1}{1}{%
This is a footnote with nested\DUfootnotemark{footnote-reference-2}{footnote-1}{2} footnotes.\DUfootnotemark{footnote-reference-3}{footnote-2}{3}\DUfootnotemark{footnote-reference-4}{footnote-3}{4}
}
%
\DUfootnotetext{footnote-1}{footnote-reference-2}{2}{%
First nested\DUfootnotemark{footnote-reference-5}{footnote-4}{5} footnote.\DUfootnotemark{footnote-reference-6}{footnote-5}{6}
}
%
\DUfootnotetext{footnote-2}{footnote-reference-3}{3}{%
Second nested footnote.\DUfootnotemark{footnote-reference-7}{footnote-6}{7}
}
%
\DUfootnotetext{footnote-3}{footnote-reference-4}{4}{%
Third nested footnote.
}
%
\DUfootnotetext{footnote-4}{footnote-reference-5}{5}{%
First double-nested footnote.
}
%
\DUfootnotetext{footnote-5}{footnote-reference-6}{6}{%
Second double-nested footnote.
}
%
\DUfootnotetext{footnote-6}{footnote-reference-7}{7}{%
First triple-nested footnote.
}
%
\DUfootnotetext{footnote-7}{footnote-reference-8}{8}{%
Not nested, referenced after footnote text.
}

Ref to a new footnote\DUfootnotemark{footnote-reference-8}{footnote-7}{8}

A second reference to the first one\DUfootnotemark{footnote-reference-9}{multi}{1}.
We can also write a hyperlink to \hyperref[multi]{multi}.
"""],
])


if __name__ == '__main__':
    unittest.main()
