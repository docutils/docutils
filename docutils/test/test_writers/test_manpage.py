#! /usr/bin/env python3

# Author: engelbert gruber <grubert@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Tests for manpage writer.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.core import publish_string


class WriterPublishTestCase(unittest.TestCase):

    maxDiff = None

    def test_publish(self):
        writer_name = 'manpage'
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    output = publish_string(
                        source=case_input,
                        writer_name=writer_name,
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                            'validate': False,  # allow testing invalid doctree
                        }).decode()
                    self.assertEqual(case_expected, output)


document_start = r""".\" Man page generated from reStructuredText by manpage writer
.\" from docutils 0.22b.dev.
.
"""

indend_macros = r""".
.nr rst2man-indent-level 0
.
.de1 rstReportMargin
\\$1 \\n[an-margin]
level \\n[rst2man-indent-level]
level margin: \\n[rst2man-indent\\n[rst2man-indent-level]]
-
\\n[rst2man-indent0]
\\n[rst2man-indent1]
\\n[rst2man-indent2]
..
.de1 INDENT
.\" .rstReportMargin pre:
. RS \\$1
. nr rst2man-indent\\n[rst2man-indent-level] \\n[an-margin]
. nr rst2man-indent-level +1
.\" .rstReportMargin post:
..
.de UNINDENT
. RE
.\" indent \\n[an-margin]
.\" old: \\n[rst2man-indent\\n[rst2man-indent-level]]
.nr rst2man-indent-level -1
.\" new: \\n[rst2man-indent\\n[rst2man-indent-level]]
.in \\n[rst2man-indent\\n[rst2man-indent-level]]u
..
"""

totest = {}

totest['blank'] = [
        ["",
         document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
.\\" End of generated man page.
"""],
        [r"""Hello, world.
=============

.. WARNING::
   This broke docutils-sphinx.

""",
         document_start + indend_macros +
""".TH "Hello, world." "" "" ""
.SH Name
Hello, world. \\- \n\
.sp
\\fBWarning:\\fP
.INDENT 0.0
.INDENT 3.5
This broke docutils\\-sphinx.
.UNINDENT
.UNINDENT
.\\" End of generated man page.
"""],
]

totest['simple'] = [
        ["""\
========
 simple
========

---------------
 The way to go
---------------

:Author: someone@somewhere.net
:Date:   2009-08-05
:Copyright: public domain
:Version: 0.1
:Manual section: 1
:Manual group: text processing
:Arbitrary field: some text

SYNOPSIS
========

::

  K.I.S.S keep it simple.

DESCRIPTION
===========

General rule of life.

OPTIONS
=======

--config=<file>         Read configuration settings from <file>, if it exists.
--version, -V           Show this program's version number and exit.
--help, -h              Show this help message and exit.

OtHeR SECTION
=============

link to http://docutils.sourceforge.io

With mixed case.

.. Attention::

   Admonition with title

   * bullet list
   * bull and list

.. admonition:: homegrown

   something important

. period at line start.

and . in a line and at line start
.in a paragraph
""",
         document_start + indend_macros +
"""\
.TH "simple" "1" "2009-08-05" "0.1" "text processing"
.SH Name
simple \\- The way to go
.SH SYNOPSIS
.INDENT 0.0
.INDENT 3.5
.sp
.EX
K.I.S.S keep it simple.
.EE
.UNINDENT
.UNINDENT
.SH DESCRIPTION
.sp
General rule of life.
.SH OPTIONS
.INDENT 0.0
.TP
.BI \\-\\-config\\fB= <file>
Read configuration settings from <file>, if it exists.
.TP
.B  \\-\\-version\\fP,\\fB  \\-V
Show this program\\(aqs version number and exit.
.TP
.B  \\-\\-help\\fP,\\fB  \\-h
Show this help message and exit.
.UNINDENT
.SH OtHeR SECTION
.sp
link to  <http://docutils.sourceforge.io> \n\
.sp
With mixed case.
.sp
\\fBAttention!:\\fP
.INDENT 0.0
.INDENT 3.5
Admonition with title
.INDENT 0.0
.IP \\(bu 2
bullet list
.IP \\(bu 2
bull and list
.UNINDENT
.UNINDENT
.UNINDENT
.INDENT 0.0
.INDENT 3.5
.IP "homegrown"
.sp
something important
.UNINDENT
.UNINDENT
.sp
\\&. period at line start.
.sp
and . in a line and at line start
\\&.in a paragraph
.SH Author
someone@somewhere.net

Arbitrary field: some text
.SH Copyright
public domain
.\\" End of generated man page.
"""],
]

totest['table'] = [
        ["""\
        ====== =====
         head   and
        ====== =====
           1     2
          abc   so
        ====== =====
""",
'''\
\'\\" t
''' + document_start + indend_macros + '''.TH "" "" "" ""
.SH Name
 \\- \n\
.INDENT 0.0
.INDENT 3.5
.TS
box center;
l|l.
T{
head
T}\tT{
and
T}
_
T{
1
T}\tT{
2
T}
_
T{
abc
T}\tT{
so
T}
.TE
.UNINDENT
.UNINDENT
.\\" End of generated man page.
''']
]

totest['optiongroup'] = [
        ["""
optin group with dot as group item

$
   bla bla bla

#
   bla bla bla

.
   bla bla bla

[
   bla bla bla

]
   bla bla bla
""",
document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
optin group with dot as group item
.INDENT 0.0
.TP
.B $
bla bla bla
.UNINDENT
.INDENT 0.0
.TP
.B #
bla bla bla
.UNINDENT
.INDENT 0.0
.TP
.B \\&.
bla bla bla
.UNINDENT
.INDENT 0.0
.TP
.B [
bla bla bla
.UNINDENT
.INDENT 0.0
.TP
.B ]
bla bla bla
.UNINDENT
.\\" End of generated man page.
"""],
]

totest['definitionlist'] = [
        ["""
====================
Definition List Test
====================

:Abstract: Docinfo is required.

Section
=======

:term1:

    Description of Term 1 Description of Term 1 Description of Term 1
    Description of Term 1 Description of Term 1

    Description of Term 1 Description of Term 1 Description of Term 1
    Description of Term 1 Description of Term 1

""",
document_start + indend_macros + '''.TH "Definition List Test" "" "" ""
.SH Name
Definition List Test \\- \n\
''' + '''.SS Abstract
.sp
Docinfo is required.
.SH Section
.INDENT 0.0
.TP
.B term1
Description of Term 1 Description of Term 1 Description of Term 1
Description of Term 1 Description of Term 1
.sp
Description of Term 1 Description of Term 1 Description of Term 1
Description of Term 1 Description of Term 1
.UNINDENT
.\\" End of generated man page.
'''],
]

totest['cmdlineoptions'] = [
        ["""optional arguments:
  -h, --help                 show this help
  --output FILE, -o FILE     output filename
  -i DEVICE, --input DEVICE  input device
""",
document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
.INDENT 0.0
.TP
.B optional arguments:
.INDENT 7.0
.TP
.B  \\-h\\fP,\\fB  \\-\\-help
show this help
.TP
.BI \\-\\-output \\ FILE\\fR,\\fB \\ \\-o \\ FILE
output filename
.TP
.BI \\-i \\ DEVICE\\fR,\\fB \\ \\-\\-input \\ DEVICE
input device
.UNINDENT
.UNINDENT
.\\" End of generated man page.
"""],
]

totest['citation'] = [
        [""".. [docutils] blah blah blah
.. [empty_citation]
""",
document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
.IP [docutils] 5
blah blah blah
.IP [empty_citation] 5
.\\" End of generated man page.
"""],
]

totest['rubric'] = [
        [""".. rubric:: some rubric

- followed by
- a list
""",
document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
some rubric
.INDENT 0.0
.IP \\(bu 2
followed by
.IP \\(bu 2
a list
.UNINDENT
.\\" End of generated man page.
"""],
]

totest['double_quote'] = [
        ["""in "defintion list"
    double quotes must be escaped on macro invocations.

They are "escaped" anywhere.
""",
document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
.INDENT 0.0
.TP
.B in \\(dqdefintion list\\(dq
double quotes must be escaped on macro invocations.
.UNINDENT
.sp
They are \\(dqescaped\\(dq anywhere.
.\\" End of generated man page.
"""],
]

totest['man_header'] = [
        ["""
============
 page title
============

in short
--------

:Manual section: 3
:Manual group: the books
:Version: 0.0
:Date: 3/Nov/2022

Test title, docinfo to man page header.
""",
document_start + indend_macros + r""".TH "page title" "3" "3/Nov/2022" "0.0" "the books"
.SH Name
page title \- in short
.sp
Test title, docinfo to man page header.
.\" End of generated man page.
"""],
]


if __name__ == '__main__':
    unittest.main()
