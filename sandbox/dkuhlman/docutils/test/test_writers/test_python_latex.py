#! /usr/bin/env python

# Author: Dave Kuhlman
# Contact: dkuhlman@rexx.com
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for python_latex writer.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.PythonLatexPublishTestSuite()
    s.generateTests(totest)
    return s


rest_head = """\

==========================
Unit Test for Python LaTeX
==========================


:author: Dave Kuhlman
:address: dkuhlman@rexx.com \\\\
    http://www.rexx.com/~dkuhlman

:revision: 1.0a
:date: Aug. 4, 2003

:copyright: Copyright (c) 2003 Dave Kuhlman. 
    [an Open Source copyright]

:abstract: This document contains input for unit tests for the
    Python LaTeX writer for Docutils.

.. sectnum::    :depth: 2

.. contents::


"""

latex_head = r"""\documentclass{howto}

% generator -- Docutils: http://docutils.sourceforge.net/
% writer -- documenting_python
% generated on -- Wed Aug 13 16:08:04 2003

\usepackage{html}
\title{Unit Test for Python LaTeX}
\release{1.0a}
\date{Aug. 4, 2003}
\author{Dave Kuhlman}
\authoraddress{dkuhlman@rexx.com \
http://www.rexx.com/\~{}dkuhlman}
\begin{document}
\maketitle
\ifhtml
\chapter*{Front Matter\label{front}}
\fi

Copyright (c) 2003 Dave Kuhlman. 
[an Open Source copyright]
\begin{abstract}
\noindent

This document contains input for unit tests for the
Python LaTeX writer for Docutils.
\end{abstract}
\tableofcontents

"""

totest = {}

totest['section_headers'] = [
# input
[rest_head + """
Section #1
==========

Content for section #1.

Subsection #1-1
---------------

Content for sub-section #1-1.

Subsection #1-2
---------------

Content for sub-section #1-2.

Section #2
==========

Content for section #2.

""",

# expected output
latex_head + r"""
%___________________________________________________________________________

\section{1   Section {\#}1\label{section-1}}

Content for section {\#}1.


%___________________________________________________________________________

\subsection{1.1   Subsection {\#}1-1\label{subsection-1-1}}

Content for sub-section {\#}1-1.


%___________________________________________________________________________

\subsection{1.2   Subsection {\#}1-2\label{subsection-1-2}}

Content for sub-section {\#}1-2.


%___________________________________________________________________________

\section{2   Section {\#}2\label{section-2}}

Content for section {\#}2.

\end{document}
"""],
]

# ==============================================================

totest['enumerated_lists'] = [
# input
[rest_head + """
Enumerated List Test Section
============================

Here is an enumerated list:

1. This is an item.

2. This is an item.

3. This is an item.

""",

# expected output
latex_head + r"""
%___________________________________________________________________________

\section{1   Enumerated List Test Section\label{enumerated-list-test-section}}

Here is an enumerated list:
\begin{enumerate}
\item 
This is an item.

\item 
This is an item.

\item 
This is an item.

\end{enumerate}

\end{document}
"""],
]


# ==============================================================

totest['itemized_lists'] = [
# input
[rest_head + """
Itemized List Test Section
==========================

Here is an itemized list:

- This is an item.

- This is an item.

- This is an item.

""",

# expected output
latex_head + r"""
%___________________________________________________________________________

\section{1   Itemized List Test Section\label{itemized-list-test-section}}

Here is an itemized list:
\begin{itemize}
\item 
This is an item.

\item 
This is an item.

\item 
This is an item.

\end{itemize}

\end{document}
"""],
]

# ==============================================================

totest['links'] = [
# input
[rest_head + """
Section #1
==========

Content for section #1.

Subsection #1-1
---------------

Content for sub-section #1-1.

Subsection #1-2
---------------

Content for sub-section #1-2.

Section #2
==========

Content for section #2.

Links Test Section
==================

Here is a link to the section which is the target of this
internal link `Subsection #1-1`_.

""",

# expected output
latex_head + r"""
%___________________________________________________________________________

\section{1   Section {\#}1\label{section-1}}

Content for section {\#}1.


%___________________________________________________________________________

\subsection{1.1   Subsection {\#}1-1\label{subsection-1-1}}

Content for sub-section {\#}1-1.


%___________________________________________________________________________

\subsection{1.2   Subsection {\#}1-2\label{subsection-1-2}}

Content for sub-section {\#}1-2.


%___________________________________________________________________________

\section{2   Section {\#}2\label{section-2}}

Content for section {\#}2.


%___________________________________________________________________________

\section{3   Links Test Section\label{links-test-section}}

Here is a link to the section which is the target of this
internal link \ref{subsection-1-1}.

\end{document}
"""],
]

# ==============================================================

totest['seealso'] = [
# input
[rest_head + """
See Also
========

`Docutils: Python Documentation Utilities`_

.. _`Docutils: Python Documentation Utilities`:
    http://docutils.sourceforge.net/

""",

# expected output
latex_head + r"""
%___________________________________________________________________________

\section{1   See Also\label{see-also}}

\ulink{Docutils: Python Documentation Utilities}{http://docutils.sourceforge.net/}

\end{document}
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')





