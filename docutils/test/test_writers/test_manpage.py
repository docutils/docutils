#! /usr/bin/env python3

# Author: engelbert gruber <grubert@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Tests for manpage writer.
"""

from pathlib import Path
import sys
import unittest
from io import StringIO

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.core import publish_string
from docutils.writers import manpage

URI_tests = (
        ("///abc.de", r"///\:abc\:.de"),
        ("/abc.de/", r"/\:abc\:.de/"),
        ("http://abc.de", r"http://\:abc\:.de"),
        ("http://abc.de/fg", r"http://\:abc\:.de/fg"),
        ("http://abc.de/fg?q=abc", r"http://\:abc\:.de/\:fg?\:q=abc"),
        ("http://abc.de/fg/?q=abc", r"http://\:abc\:.de/\:fg/?\:q=abc"),
        ("http://abc.de/fg/?q=abc&me#", r"http://\:abc\:.de/\:fg/?\:q=abc&\:me#"),
        ("me@home.here", r"me@\:home\:.here"),
        ("me..dot@home.here..", r"me\:..dot@\:home\:.here.."),
        )


class URIBreakpointsTestCase(unittest.TestCase):

    def test_insert(self):
        for t in URI_tests:
            got = manpage.insert_URI_breakpoints(t[0])
            self.assertEqual(t[1], got)


class WriterPublishTestCase(unittest.TestCase):

    maxDiff = None

    def test_publish(self):
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    output = publish_string(
                        source=case_input,
                        writer=manpage.Writer(),
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                        }).decode()
                    self.assertEqual(case_expected, output)

    def test_reference_macros(self):
        for name, cases in totest_refs.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest_refs[{name!r}][{casenum}]'):
                    output = publish_string(
                        source=case_input,
                        writer=manpage.Writer(),
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                            'text_references': False,
                            'output_encoding': "unicode",
                        })
                    self.assertEqual(case_expected, output)

    def test_system_msgs(self):
        for name, cases in totest_system_msgs.items():
            for casenum, (case_input, case_expected, case_warning) in enumerate(cases):
                with self.subTest(id=f'totest_system_msgs[{name!r}][{casenum}]'):
                    warnings = StringIO("")
                    output = publish_string(
                        source=case_input,
                        writer=manpage.Writer(),
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                            'report_level': 1,
                            'warning_stream': warnings,
                            'output_encoding': "unicode",
                        })
                    self.assertEqual(case_expected, output)
                    warnings.seek(0)
                    self.assertEqual(
                            case_warning,
                            warnings.readlines())


document_start = r""".\" Man page generated from reStructuredText
.\" by the Docutils 0.22rc4 manpage writer.
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

totest_refs = {}

totest_refs['ext hyperlink'] = [
        [r"""External hyperlinks, like Python_.

.. _Python: https://www.python.org/
""",
         document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
External hyperlinks, like \\c\n\
.UR \\%https://\\:www\\:.python\\:.org/
Python
.UE \\c
\\&.
.\\" End of generated man page.
"""],
        ]

totest_refs['emb hyperlink'] = [
        [r"""embedded External hyperlinks, like `Python
<https://www.python.org/>`_.
""",
         document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
embedded External hyperlinks, like \\c\n\
.UR \\%https://\\:www\\:.python\\:.org/
Python
.UE \\c
\\&.
.\\" End of generated man page.
"""],
        ]

totest_refs['email'] = [
        [r"""`Write to me`_ with your questions.

.. _Write to me: jdoe@example.com

whatever.""",
         document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
.MT \\%jdoe@\\:example\\:.com
Write to me
.ME \\c
 with your questions.
.sp
whatever.
.\\" End of generated man page.
"""],
        ]

totest_refs['email trailings'] = [
        [r"""email: (eee@mmm.al).

no spaces before and after the email.""",
         document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
email: (\\c
.MT \\%eee@\\:mmm\\:.al
.ME \\c
).
.sp
no spaces before and after the email.
.\\" End of generated man page.
"""],
        ]

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
link to \\%<http://\\:docutils\\:.sourceforge\\:.io>\n\
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
["""\
Internal hyperlinks_ and targets_ are ignored.

.. _hyperlinks:

(Text content of hyperlinks and _`targets` is printed as normal text.)
""",
f"""{document_start}{indend_macros}.TH "" "" "" ""
.SH Name
 \\- \n\
Internal hyperlinks and targets are ignored.
.sp
(Text content of hyperlinks and targets is printed as normal text.)
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
.. [citation2] Another Source
""",
document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
.IP [docutils] 5
blah blah blah
.IP [citation2] 5
Another Source
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

# test defintion
# [ input, expect, expected_warning ]
totest_system_msgs = {}
# check we get an INFO not a WARNING
totest_system_msgs['image'] = [
        ["""\
text

.. image:: gibsnich.png
   :alt: an image of something

more text
""",
document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
text
.sp
    an image of something
.sp
more text
.\\" End of generated man page.
""",
['<string>:3: (INFO/1) "image" not supported by "manpage" writer.\n']
],

# check we get a WARNING if no alt text
        ["""text

.. image:: gibsnich.png

more text
""",
document_start + indend_macros + """.TH "" "" "" ""
.SH Name
 \\- \n\
text
.sp
    image: gibsnich.png
.sp
more text
.\\" End of generated man page.
""",
['<string>:3: (WARNING/2) "image" not supported by "manpage" writer.\n',
'Please provide an "alt" attribute with textual replacement.\n']
]
]


if __name__ == '__main__':
    unittest.main()
