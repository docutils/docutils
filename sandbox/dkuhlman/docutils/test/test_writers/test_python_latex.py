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
[rest_head + r"""
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
[rest_head + r"""
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
[rest_head + r"""
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
[rest_head + r"""
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
[rest_head + r"""
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


# ==============================================================

totest['emphasis'] = [
# input
[rest_head + r"""
Section #1
==========

This section contains *some emphaized text*.

It **also has some bold text**.

And ``this is sample text``, or, as it is known in reStructured
Text, an in-line literal.

""",

# expected output
latex_head + r"""
%___________________________________________________________________________

\section{1   Section {\#}1\label{section-1}}

This section contains \emph{some emphaized text}.

It \strong{also has some bold text}.

And \samp{this is sample text}, or, as it is known in reStructured
Text, an in-line literal.

\end{document}
"""],
]


# ==============================================================

totest['blockquote'] = [
# input
[rest_head + r"""
Block Quote Test
================

This section contains a paragraph which is followed by a block
quote.  The block quote is a simple paragraph that is indented.

    This is the block quoted paragraph.  It has several sentences.
    These sentences have empty content because they are only a
    test.  This is a test.  This is a test.  This is a test.  This
    is a test.  This is a test.  This is a test.


Python Iteractive Session Test
==============================

This section has a Python interactive session.  It is a test.
Here is the example session.

>>> import sys
>>> print sys.version
2.3 (#1, Jul 31 2003, 15:26:15)
[GCC 3.2.3 20030415 (Debian prerelease)]
>>> a = [11,22,33]
>>> for x in a:
...   print x
...
11
22
33


Literal Block Test
==================

This section has a literal block.  In Python LaTeX it will be
formatted in a verbatim environment::

    def remove_lines(self, inStr, targetList):
        inLines = inStr.splitlines()
        outLines = []
        for line in inLines:
            remove = False
            for target in targetList:
                if line.find(target) > -1:
                    remove = True
            if not remove:
                outLines.append(line)
        outStr = '\n'.join(outLines)
        return outStr

""",

# expected output
latex_head + r"""
%___________________________________________________________________________

\section{1   Block Quote Test\label{block-quote-test}}

This section contains a paragraph which is followed by a block
quote.  The block quote is a simple paragraph that is indented.
\begin{quote}

This is the block quoted paragraph.  It has several sentences.
These sentences have empty content because they are only a
test.  This is a test.  This is a test.  This is a test.  This
is a test.  This is a test.  This is a test.
\end{quote}


%___________________________________________________________________________

\section{2   Python Iteractive Session Test\label{python-iteractive-session-test}}

This section has a Python interactive session.  It is a test.
Here is the example session.
\begin{verbatim}
>>> import sys
>>> print sys.version
2.3 (#1, Jul 31 2003, 15:26:15)
[GCC 3.2.3 20030415 (Debian prerelease)]
>>> a = [11,22,33]
>>> for x in a:
...   print x
...
11
22
33
\end{verbatim}


%___________________________________________________________________________

\section{3   Literal Block Test\label{literal-block-test}}

This section has a literal block.  In Python LaTeX it will be
formatted in a verbatim environment:

\begin{verbatim}
def remove_lines(self, inStr, targetList):
    inLines = inStr.splitlines()
    outLines = []
    for line in inLines:
        remove = False
        for target in targetList:
            if line.find(target) > -1:
                remove = True
        if not remove:
            outLines.append(line)
    outStr = '\n'.join(outLines)
    return outStr
\end{verbatim}

\end{document}
"""],
]


# ==============================================================

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')





