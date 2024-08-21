#!/usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the HTML writer.
"""

import os
import platform
from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

import docutils
from docutils.core import publish_string
from docutils.writers import html5_polyglot

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
                        writer=html5_polyglot.Writer(),
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
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>"""


head = """\
<meta charset="utf-8" />
<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="author" content="Me" />
<title>Document Title</title>"""


stylesheet = """\
<link rel="stylesheet" href="{drive_prefix}/test.css" type="text/css" />"""


body_prefix = """\
</head>
<body>
<main id="document-title">"""


body_pre_docinfo = """\
<h1 class="title">Document Title</h1>
<p class="subtitle" id="subtitle">Subtitle</p>"""


docinfo = """\
<dl class="docinfo simple">
<dt class="author">Author<span class="colon">:</span></dt>
<dd class="author"><p>Me</p></dd>
</dl>"""


body = """\
<section id="section">
<h2>Section</h2>
<p>Some text.</p>
</section>"""


body_suffix = """\
</main>
<footer>
<p>footer text</p>
</footer>
</body>
</html>"""


head_prefix = """\
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>"""


head = """\
<meta charset="utf-8" />
<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="author" content="Me" />
<title>Document Title</title>"""


stylesheet = """\
<link rel="stylesheet" href="{drive_prefix}/test.css" type="text/css" />"""


body_prefix = """\
</head>
<body>
<main id="document-title">"""


body_pre_docinfo = """\
<h1 class="title">Document Title</h1>
<p class="subtitle" id="subtitle">Subtitle</p>"""


docinfo = """\
<dl class="docinfo simple">
<dt class="author">Author<span class="colon">:</span></dt>
<dd class="author"><p>Me</p></dd>
</dl>"""


body = """\
<section id="section">
<h2>Section</h2>
<p>Some text.</p>
</section>"""


body_suffix = """\
</main>
<footer>
<p>footer text</p>
</footer>
</body>
</html>"""


title = """\
Document Title"""


subtitle = """\
Subtitle"""


header = """\
"""


footer = """\
<footer>
<p>footer text</p>
</footer>"""


meta = """\
<meta charset="utf-8" />
<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="author" content="Me" />"""


fragment = """\
<section id="section">
<h2>Section</h2>
<p>Some text.</p>
</section>"""


html_prolog = """\
<!DOCTYPE html>"""


html_head = """\
<meta charset="%s" />
<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="author" content="Me" />
<title>Document Title</title>"""


html_title = """\
<h1 class="title">Document Title</h1>"""


html_subtitle = """\
<p class="subtitle" id="subtitle">Subtitle</p>"""


html_body = """\
<main id="document-title">
<h1 class="title">Document Title</h1>
<p class="subtitle" id="subtitle">Subtitle</p>
<dl class="docinfo simple">
<dt class="author">Author<span class="colon">:</span></dt>
<dd class="author"><p>Me</p></dd>
</dl>
<section id="section">
<h2>Section</h2>
<p>Some text.</p>
</section>
</main>
<footer>
<p>footer text</p>
</footer>"""
''']
]

if __name__ == '__main__':
    unittest.main()
