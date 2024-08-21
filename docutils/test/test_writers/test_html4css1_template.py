#!/usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the HTML writer.
"""

from pathlib import Path
import os
import platform
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

import docutils
from docutils.core import publish_string
from docutils.writers import html4css1

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = Path(__file__).parents[1]


class WriterPublishTestCase(unittest.TestCase):
    # maxDiff = None
    def test_publish(self):
        template_path = TEST_ROOT / 'data/full-template.txt'
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    output = publish_string(
                        source=case_input,
                        writer=html4css1.Writer(),
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                            'template': template_path,
                            'stylesheet_path': '/test.css',
                            'embed_stylesheet': False,
                        }).decode()
                    self.assertEqual(case_expected, output)


if platform.system() == "Windows":
    drive_prefix = os.path.splitdrive(os.getcwd())[0]
else:
    drive_prefix = ""


totest = {}

totest['template'] = [
["""\
================
 Document Title
================
----------
 Subtitle
----------

:Author: Me

.. footer:: footer text

Section
=======

Some text.
""",
fr'''head_prefix = """\
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>"""


head = """\
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />
<meta name="author" content="Me" />
<title>Document Title</title>"""


stylesheet = """\
<link rel="stylesheet" href="{drive_prefix}/test.css" type="text/css" />"""


body_prefix = """\
</head>
<body>
<div class="document" id="document-title">"""


body_pre_docinfo = """\
<h1 class="title">Document Title</h1>
<h2 class="subtitle" id="subtitle">Subtitle</h2>"""


docinfo = """\
<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>Me</td></tr>
</tbody>
</table>"""


body = """\
<div class="section" id="section">
<h1>Section</h1>
<p>Some text.</p>
</div>"""


body_suffix = """\
</div>
<div class="footer">
<hr class="footer" />
footer text
</div>
</body>
</html>"""


head_prefix = """\
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>"""


head = """\
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />
<meta name="author" content="Me" />
<title>Document Title</title>"""


stylesheet = """\
<link rel="stylesheet" href="{drive_prefix}/test.css" type="text/css" />"""


body_prefix = """\
</head>
<body>
<div class="document" id="document-title">"""


body_pre_docinfo = """\
<h1 class="title">Document Title</h1>
<h2 class="subtitle" id="subtitle">Subtitle</h2>"""


docinfo = """\
<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>Me</td></tr>
</tbody>
</table>"""


body = """\
<div class="section" id="section">
<h1>Section</h1>
<p>Some text.</p>
</div>"""


body_suffix = """\
</div>
<div class="footer">
<hr class="footer" />
footer text
</div>
</body>
</html>"""


title = """\
Document Title"""


subtitle = """\
Subtitle"""


header = """\
"""


footer = """\
<div class="footer">
<hr class="footer" />
footer text
</div>"""


meta = """\
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />
<meta name="author" content="Me" />"""


fragment = """\
<div class="section" id="section">
<h1>Section</h1>
<p>Some text.</p>
</div>"""


html_prolog = """\
<?xml version="1.0" encoding="%s"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">"""


html_head = """\
<meta http-equiv="Content-Type" content="text/html; charset=%s" />
<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />
<meta name="author" content="Me" />
<title>Document Title</title>"""


html_title = """\
<h1 class="title">Document Title</h1>"""


html_subtitle = """\
<h2 class="subtitle" id="subtitle">Subtitle</h2>"""


html_body = """\
<div class="document" id="document-title">
<h1 class="title">Document Title</h1>
<h2 class="subtitle" id="subtitle">Subtitle</h2>
<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>Me</td></tr>
</tbody>
</table>
<div class="section" id="section">
<h1>Section</h1>
<p>Some text.</p>
</div>
</div>
<div class="footer">
<hr class="footer" />
footer text
</div>"""
''']
]

if __name__ == '__main__':
    unittest.main()
