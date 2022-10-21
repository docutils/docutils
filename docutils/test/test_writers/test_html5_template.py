#!/usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the HTML writer.
"""

import os
import platform

if __name__ == '__main__':
    import __init__  # noqa: F401
from test import DocutilsTestSupport


def suite():
    settings = {'template': os.path.join(DocutilsTestSupport.testroot,
                                         'data', 'full-template.txt'),
                'stylesheet_path': '/test.css',
                'embed_stylesheet': 0}
    s = DocutilsTestSupport.PublishTestSuite('html5', suite_settings=settings)
    s.generateTests(totest)
    return s


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
r'''head_prefix = """\
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>"""


head = """\
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="generator" content="Docutils %(version)s: https://docutils.sourceforge.io/" />
<title>Document Title</title>
<meta name="author" content="Me" />"""


stylesheet = """\
<link rel="stylesheet" href="%(drive)s/test.css" type="text/css" />"""


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
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="generator" content="Docutils %(version)s: https://docutils.sourceforge.io/" />
<title>Document Title</title>
<meta name="author" content="Me" />"""


stylesheet = """\
<link rel="stylesheet" href="%(drive)s/test.css" type="text/css" />"""


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
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="generator" content="Docutils %(version)s: https://docutils.sourceforge.io/" />
<meta name="author" content="Me" />"""


fragment = """\
<section id="section">
<h2>Section</h2>
<p>Some text.</p>
</section>"""


html_prolog = """\
<!DOCTYPE html>"""


html_head = """\
<meta charset="%%s" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="generator" content="Docutils %(version)s: https://docutils.sourceforge.io/" />
<title>Document Title</title>
<meta name="author" content="Me" />"""


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
''' % {'version': DocutilsTestSupport.docutils.__version__,
       'drive': drive_prefix,
    }]
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
