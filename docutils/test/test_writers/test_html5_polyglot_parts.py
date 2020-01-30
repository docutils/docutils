#! /usr/bin/env python

# $Id$
# Author: reggie dugard <reggie@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test for fragment code in HTML writer.

Note: the 'body' and 'whole' entries have been removed from the parts
dictionaries (redundant), along with 'meta' and 'stylesheet' entries with
standard values, and any entries with empty values.
"""
from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_transforms import DocutilsTestSupport  # must be imported before docutils
from DocutilsTestSupport import (HtmlWriterPublishPartsTestCase,
                                 HtmlPublishPartsTestSuite)
from docutils import core, __version__


class Html5WriterPublishPartsTestCase(HtmlWriterPublishPartsTestCase):
    """Test case for HTML5 writer via the publish_parts interface."""

    writer_name = 'html5'
    standard_content_type_template = ('<meta charset="%s"/>\n')
    standard_generator_template = (
        '<meta name="generator"'
        ' content="Docutils %s: http://docutils.sourceforge.net/" />\n')
    standard_html_meta_value = (standard_content_type_template
                        + standard_generator_template % __version__)
    standard_meta_value = standard_html_meta_value % 'utf-8'
    standard_html_prolog = '<!DOCTYPE html>\n'

class Html5PublishPartsTestSuite(HtmlPublishPartsTestSuite):

    testcase_class = Html5WriterPublishPartsTestCase


def suite():
    s = Html5PublishPartsTestSuite()
    s.generateTests(totest)
    return s


totest = {}

totest['Title promotion'] = ({'stylesheet_path': '',
                              'embed_stylesheet': 0}, [
["""\
Simple String
""",
"""\
{'fragment': '''<p>Simple String</p>\\n''',
 'html_body': '''<main>
<p>Simple String</p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
Simple String with *markup*
""",
"""\
{'fragment': '''<p>Simple String with <em>markup</em></p>\\n''',
 'html_body': '''<main>
<p>Simple String with <em>markup</em></p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
Simple String with an even simpler ``inline literal``
""",
"""\
{'fragment': '''<p>Simple String with an even simpler <span class="docutils literal">inline literal</span></p>\\n''',
 'html_body': '''<main>
<p>Simple String with an even simpler <span class="docutils literal">inline literal</span></p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
"""\
{'fragment': '''<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>\\n''',
 'html_body': '''<main>
<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
One paragraph.

Two paragraphs.
""",
"""\
{'fragment': '''<p>One paragraph.</p>
<p>Two paragraphs.</p>\\n''',
 'html_body': '''<main>
<p>One paragraph.</p>
<p>Two paragraphs.</p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
"""\
{'fragment': '''<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>\\n''',
 'html_body': '''<main>
<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
+++++
Title
+++++

Subtitle
========

Some stuff

Section
-------

Some more stuff

Another Section
...............

And even more stuff
""",
"""\
{'fragment': '''<p>Some stuff</p>
<section id="section">
<h1>Section</h1>
<p>Some more stuff</p>
<section id="another-section">
<h2>Another Section</h2>
<p>And even more stuff</p>
</section>
</section>\\n''',
 'html_body': '''<main id="title">
<h1 class="title">Title</h1>
<p class="subtitle" id="subtitle">Subtitle</p>
<p>Some stuff</p>
<section id="section">
<h1>Section</h1>
<p>Some more stuff</p>
<section id="another-section">
<h2>Another Section</h2>
<p>And even more stuff</p>
</section>
</section>
</main>\\n''',
 'html_head': '''...<title>Title</title>\\n''',
 'html_subtitle': '''<p class="subtitle" id="subtitle">Subtitle</p>\\n''',
 'html_title': '''<h1 class="title">Title</h1>\\n''',
 'subtitle': '''Subtitle''',
 'title': '''Title'''}
"""],
["""\
+++++
Title
+++++

:author: me

Some stuff
""",
"""\
{'docinfo': '''<dl class="docinfo simple">
<dt class="author">Author</dt>
<dd class="author"><p>me</p></dd>
</dl>\\n''',
 'fragment': '''<p>Some stuff</p>\\n''',
 'html_body': '''<main id="title">
<h1 class="title">Title</h1>
<dl class="docinfo simple">
<dt class="author">Author</dt>
<dd class="author"><p>me</p></dd>
</dl>
<p>Some stuff</p>
</main>\\n''',
 'html_head': '''...<title>Title</title>
<meta name="author" content="me" />\\n''',
 'html_title': '''<h1 class="title">Title</h1>\\n''',
 'meta': '''<meta name="author" content="me" />\\n''',
 'title': '''Title'''}
"""]
])

totest['No title promotion'] = ({'doctitle_xform' : 0,
                                 'stylesheet_path': '',
                                 'embed_stylesheet': 0}, [
["""\
Simple String
""",
"""\
{'fragment': '''<p>Simple String</p>\\n''',
 'html_body': '''<main>
<p>Simple String</p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
Simple String with *markup*
""",
"""\
{'fragment': '''<p>Simple String with <em>markup</em></p>\\n''',
 'html_body': '''<main>
<p>Simple String with <em>markup</em></p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
Simple String with an even simpler ``inline literal``
""",
"""\
{'fragment': '''<p>Simple String with an even simpler <span class="docutils literal">inline literal</span></p>\\n''',
 'html_body': '''<main>
<p>Simple String with an even simpler <span class="docutils literal">inline literal</span></p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
"""\
{'fragment': '''<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>\\n''',
 'html_body': '''<main>
<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
"""\
{'fragment': '''<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>\\n''',
 'html_body': '''<main>
<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
+++++
Title
+++++

Not A Subtitle
==============

Some stuff

Section
-------

Some more stuff

Another Section
...............

And even more stuff
""",
"""\
{'fragment': '''<section id="title">
<h1>Title</h1>
<section id="not-a-subtitle">
<h2>Not A Subtitle</h2>
<p>Some stuff</p>
<section id="section">
<h3>Section</h3>
<p>Some more stuff</p>
<section id="another-section">
<h4>Another Section</h4>
<p>And even more stuff</p>
</section>
</section>
</section>
</section>\\n''',
 'html_body': '''<main>
<section id="title">
<h1>Title</h1>
<section id="not-a-subtitle">
<h2>Not A Subtitle</h2>
<p>Some stuff</p>
<section id="section">
<h3>Section</h3>
<p>Some more stuff</p>
<section id="another-section">
<h4>Another Section</h4>
<p>And even more stuff</p>
</section>
</section>
</section>
</section>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
* bullet
* list
""",
"""\
{'fragment': '''<ul class="simple">
<li><p>bullet</p></li>
<li><p>list</p></li>
</ul>\\n''',
 'html_body': '''<main>
<ul class="simple">
<li><p>bullet</p></li>
<li><p>list</p></li>
</ul>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
.. table::
   :align: right

   +-----+-----+
   |  1  |  2  |
   +-----+-----+
   |  3  |  4  |
   +-----+-----+
""",
"""\
{'fragment': '''<table class="align-right">
<colgroup>
<col style="width: 50%%" />
<col style="width: 50%%" />
</colgroup>
<tbody>
<tr><td><p>1</p></td>
<td><p>2</p></td>
</tr>
<tr><td><p>3</p></td>
<td><p>4</p></td>
</tr>
</tbody>
</table>\\n''',
 'html_body': '''<main>
<table class="align-right">
<colgroup>
<col style="width: 50%%" />
<col style="width: 50%%" />
</colgroup>
<tbody>
<tr><td><p>1</p></td>
<td><p>2</p></td>
</tr>
<tr><td><p>3</p></td>
<td><p>4</p></td>
</tr>
</tbody>
</table>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
Not a docinfo.

:This: .. _target:

       is
:a:
:simple:
:field: list
""",
"""\
{'fragment': '''<p>Not a docinfo.</p>
<dl class="field-list simple">
<dt>This</dt>
<dd><p id="target">is</p>
</dd>
<dt>a</dt>
<dd><p></p></dd>
<dt>simple</dt>
<dd><p></p></dd>
<dt>field</dt>
<dd><p>list</p>
</dd>
</dl>\\n''',
 'html_body': '''<main>
<p>Not a docinfo.</p>
<dl class="field-list simple">
<dt>This</dt>
<dd><p id="target">is</p>
</dd>
<dt>a</dt>
<dd><p></p></dd>
<dt>simple</dt>
<dd><p></p></dd>
<dt>field</dt>
<dd><p>list</p>
</dd>
</dl>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
Not a docinfo.

:This is: a
:simple field list with loooong field: names
""",
"""\
{'fragment': '''\
<p>Not a docinfo.</p>
<dl class="field-list simple">
<dt>This is</dt>
<dd><p>a</p>
</dd>
<dt>simple field list with loooong field</dt>
<dd><p>names</p>
</dd>
</dl>\\n''',
 'html_body': '''\
<main>
<p>Not a docinfo.</p>
<dl class="field-list simple">
<dt>This is</dt>
<dd><p>a</p>
</dd>
<dt>simple field list with loooong field</dt>
<dd><p>names</p>
</dd>
</dl>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
.. figure:: dummy.png

   The figure's caption.

   A legend.

   The legend's second paragraph.
""",
"""\
{'fragment': '''\
<figure>
<img alt="dummy.png" src="dummy.png" />
<figcaption>
<p>The figure's caption.</p>
<div class="legend">
<p>A legend.</p>
<p>The legend's second paragraph.</p>
</div>
</figcaption>
</figure>\\n''',
 'html_body': '''\
<main>
<figure>
<img alt="dummy.png" src="dummy.png" />
<figcaption>
<p>The figure's caption.</p>
<div class="legend">
<p>A legend.</p>
<p>The legend's second paragraph.</p>
</div>
</figcaption>
</figure>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
.. figure:: dummy.png

   The figure's caption, no legend.
""",
"""\
{'fragment': '''\
<figure>
<img alt="dummy.png" src="dummy.png" />
<figcaption>
<p>The figure's caption, no legend.</p>
</figcaption>
</figure>\\n''',
 'html_body': '''\
<main>
<figure>
<img alt="dummy.png" src="dummy.png" />
<figcaption>
<p>The figure's caption, no legend.</p>
</figcaption>
</figure>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
.. figure:: dummy.png

   ..

   A legend without caption.
""",
"""\
{'fragment': '''\
<figure>
<img alt="dummy.png" src="dummy.png" />
<figcaption>
<div class="legend">
<p>A legend without caption.</p>
</div>
</figcaption>
</figure>\\n''',
 'html_body': '''\
<main>
<figure>
<img alt="dummy.png" src="dummy.png" />
<figcaption>
<div class="legend">
<p>A legend without caption.</p>
</div>
</figcaption>
</figure>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
])
["""\
.. figure:: dummy.png

No caption nor legend.
""",
"""\
{'fragment': '''\
<figure>
<img alt="dummy.png" src="dummy.png" />
</figure>
<p>No caption nor legend.</p>\\n''',
 'html_body': '''\
<main>
<figure>
<img alt="dummy.png" src="dummy.png" />
</figure>
<p>No caption nor legend.</p>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
