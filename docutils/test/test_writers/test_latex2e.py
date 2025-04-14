#! /usr/bin/env python3
# $Id$
# Author: engelbert gruber <grubert@users.sourceforge.net>
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
Tests for latex2e writer.

This module test only the "body" part of the output.
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
   The underscore is mishandled.
""",
r"""
Just a test citation \cite{my_cite2006}.

\begin{thebibliography}{my\_cite2006}
\bibitem[my\_cite2006]{my_cite2006}{
The underscore is mishandled.
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


if __name__ == '__main__':
    unittest.main()
