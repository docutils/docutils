#! /usr/bin/env python3

# $Id$
# Author: reggie dugard <reggie@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test for fragment code in HTML writer.

Note: the 'body' and 'whole' entries have been removed from the parts
dictionaries (redundant), along with 'meta' and 'stylesheet' entries with
standard values, and any entries with empty values.
"""

if __name__ == '__main__':
    import __init__
from test_transforms import DocutilsTestSupport  # before importing docutils!
from DocutilsTestSupport import (HtmlWriterPublishPartsTestCase,
                                 HtmlPublishPartsTestSuite)
from docutils import core, __version__


class Html5WriterPublishPartsTestCase(HtmlWriterPublishPartsTestCase):
    """Test case for HTML5 writer via the publish_parts interface."""

    writer_name = 'html5'
    settings_default_overrides = HtmlWriterPublishPartsTestCase.settings_default_overrides.copy()
    settings_default_overrides['section_self_link'] = True

    standard_content_type_template = '<meta charset="%s"/>\n'
    standard_generator_template = '<meta name="generator"' \
        ' content="Docutils %s: https://docutils.sourceforge.io/" />\n'
    standard_viewport_template = '<meta name="viewport"' \
        ' content="width=device-width, initial-scale=1" />\n'

    standard_html_meta_value = (standard_content_type_template
                                + standard_viewport_template
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
<h2>Section<a class="self-link" title="link to this section" href="#section"></a></h2>
<p>Some more stuff</p>
<section id="another-section">
<h3>Another Section<a class="self-link" title="link to this section" href="#another-section"></a></h3>
<p>And even more stuff</p>
</section>
</section>\\n''',
 'html_body': '''<main id="title">
<h1 class="title">Title</h1>
<p class="subtitle" id="subtitle">Subtitle</p>
<p>Some stuff</p>
<section id="section">
<h2>Section<a class="self-link" title="link to this section" href="#section"></a></h2>
<p>Some more stuff</p>
<section id="another-section">
<h3>Another Section<a class="self-link" title="link to this section" href="#another-section"></a></h3>
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
<dt class="author">Author<span class="colon">:</span></dt>
<dd class="author"><p>me</p></dd>
</dl>\\n''',
 'fragment': '''<p>Some stuff</p>\\n''',
 'html_body': '''<main id="title">
<h1 class="title">Title</h1>
<dl class="docinfo simple">
<dt class="author">Author<span class="colon">:</span></dt>
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

totest['No title promotion'] = ({'doctitle_xform': 0,
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
<h2>Title<a class="self-link" title="link to this section" href="#title"></a></h2>
<section id="not-a-subtitle">
<h3>Not A Subtitle<a class="self-link" title="link to this section" href="#not-a-subtitle"></a></h3>
<p>Some stuff</p>
<section id="section">
<h4>Section<a class="self-link" title="link to this section" href="#section"></a></h4>
<p>Some more stuff</p>
<section id="another-section">
<h5>Another Section<a class="self-link" title="link to this section" href="#another-section"></a></h5>
<p>And even more stuff</p>
</section>
</section>
</section>
</section>\\n''',
 'html_body': '''<main>
<section id="title">
<h2>Title<a class="self-link" title="link to this section" href="#title"></a></h2>
<section id="not-a-subtitle">
<h3>Not A Subtitle<a class="self-link" title="link to this section" href="#not-a-subtitle"></a></h3>
<p>Some stuff</p>
<section id="section">
<h4>Section<a class="self-link" title="link to this section" href="#section"></a></h4>
<p>Some more stuff</p>
<section id="another-section">
<h5>Another Section<a class="self-link" title="link to this section" href="#another-section"></a></h5>
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
<dt>This<span class="colon">:</span></dt>
<dd><p id="target">is</p>
</dd>
<dt>a<span class="colon">:</span></dt>
<dd><p></p></dd>
<dt>simple<span class="colon">:</span></dt>
<dd><p></p></dd>
<dt>field<span class="colon">:</span></dt>
<dd><p>list</p>
</dd>
</dl>\\n''',
 'html_body': '''<main>
<p>Not a docinfo.</p>
<dl class="field-list simple">
<dt>This<span class="colon">:</span></dt>
<dd><p id="target">is</p>
</dd>
<dt>a<span class="colon">:</span></dt>
<dd><p></p></dd>
<dt>simple<span class="colon">:</span></dt>
<dd><p></p></dd>
<dt>field<span class="colon">:</span></dt>
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
<dt>This is<span class="colon">:</span></dt>
<dd><p>a</p>
</dd>
<dt>simple field list with loooong field<span class="colon">:</span></dt>
<dd><p>names</p>
</dd>
</dl>\\n''',
 'html_body': '''\
<main>
<p>Not a docinfo.</p>
<dl class="field-list simple">
<dt>This is<span class="colon">:</span></dt>
<dd><p>a</p>
</dd>
<dt>simple field list with loooong field<span class="colon">:</span></dt>
<dd><p>names</p>
</dd>
</dl>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
Not a docinfo.

.. class:: field-indent-200

:This: is a
:simple: field list with custom indent.
""",
"""\
{'fragment': '''<p>Not a docinfo.</p>
<dl class="field-list simple" style="--field-indent: 200px;">
<dt>This<span class="colon">:</span></dt>
<dd><p>is a</p>
</dd>
<dt>simple<span class="colon">:</span></dt>
<dd><p>field list with custom indent.</p>
</dd>
</dl>\\n''',
 'html_body': '''<main>
<p>Not a docinfo.</p>
<dl class="field-list simple" style="--field-indent: 200px;">
<dt>This<span class="colon">:</span></dt>
<dd><p>is a</p>
</dd>
<dt>simple<span class="colon">:</span></dt>
<dd><p>field list with custom indent.</p>
</dd>
</dl>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
Not a docinfo.

.. class:: field-indent-200uf

:This: is a
:simple: field list without custom indent,
         because the unit "uf" is invalid.
""",
"""\
{'fragment': '''<p>Not a docinfo.</p>
<dl class="field-indent-200uf field-list simple">
<dt>This<span class="colon">:</span></dt>
<dd><p>is a</p>
</dd>
<dt>simple<span class="colon">:</span></dt>
<dd><p>field list without custom indent,
because the unit &quot;uf&quot; is invalid.</p>
</dd>
</dl>\\n''',
 'html_body': '''<main>
<p>Not a docinfo.</p>
<dl class="field-indent-200uf field-list simple">
<dt>This<span class="colon">:</span></dt>
<dd><p>is a</p>
</dd>
<dt>simple<span class="colon">:</span></dt>
<dd><p>field list without custom indent,
because the unit &quot;uf&quot; is invalid.</p>
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
])


totest['lazy loading'] = ({'image_loading': 'lazy',
                           'stylesheet_path': '',
                           'embed_stylesheet': 0}, [

["""\
.. image:: dummy.png
""",
"""\
{'fragment': '''\
<img alt="dummy.png" loading="lazy" src="dummy.png" />\\n''',
 'html_body': '''\
<main>
<img alt="dummy.png" loading="lazy" src="dummy.png" />
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
["""\
.. figure:: dummy.png
""",
"""\
{'fragment': '''\
<figure>
<img alt="dummy.png" loading="lazy" src="dummy.png" />
</figure>\\n''',
 'html_body': '''\
<main>
<figure>
<img alt="dummy.png" loading="lazy" src="dummy.png" />
</figure>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
])


totest['no backlinks'] = ({'footnote_backlinks': False,
                           'stylesheet_path': '',
                           'embed_stylesheet': 0}, [

["""\
Two footnotes [#f1]_ [#f2]_ and two citations [once]_ [twice]_.

The latter are referenced a second time [#f2]_ [twice]_.

.. [#f1] referenced once
.. [#f2] referenced twice
.. [once] citation referenced once
.. [twice] citation referenced twice
""",
"""\
{'fragment': '''\
<p>Two footnotes <a class="footnote-reference brackets" href="#f1" id="footnote-reference-1" role="doc-noteref"><span class="fn-bracket">[</span>1<span class="fn-bracket">]</span></a> <a class="footnote-reference brackets" href="#f2" id="footnote-reference-2" role="doc-noteref"><span class="fn-bracket">[</span>2<span class="fn-bracket">]</span></a> and two citations <a class="citation-reference" href="#once" id="citation-reference-1" role="doc-biblioref">[once]</a> <a class="citation-reference" href="#twice" id="citation-reference-2" role="doc-biblioref">[twice]</a>.</p>
<p>The latter are referenced a second time <a class="footnote-reference brackets" href="#f2" id="footnote-reference-3" role="doc-noteref"><span class="fn-bracket">[</span>2<span class="fn-bracket">]</span></a> <a class="citation-reference" href="#twice" id="citation-reference-3" role="doc-biblioref">[twice]</a>.</p>
<aside class="footnote brackets" id="f1" role="note">
<span class="label"><span class="fn-bracket">[</span>1<span class="fn-bracket">]</span></span>
<p>referenced once</p>
</aside>
<aside class="footnote brackets" id="f2" role="note">
<span class="label"><span class="fn-bracket">[</span>2<span class="fn-bracket">]</span></span>
<p>referenced twice</p>
</aside>
<div role="list" class="citation-list">
<div class="citation" id="once" role="doc-biblioentry">
<span class="label"><span class="fn-bracket">[</span>once<span class="fn-bracket">]</span></span>
<p>citation referenced once</p>
</div>
<div class="citation" id="twice" role="doc-biblioentry">
<span class="label"><span class="fn-bracket">[</span>twice<span class="fn-bracket">]</span></span>
<p>citation referenced twice</p>
</div>
</div>\\n''',
 'html_body': '''\
<main>
<p>Two footnotes <a class="footnote-reference brackets" href="#f1" id="footnote-reference-1" role="doc-noteref"><span class="fn-bracket">[</span>1<span class="fn-bracket">]</span></a> <a class="footnote-reference brackets" href="#f2" id="footnote-reference-2" role="doc-noteref"><span class="fn-bracket">[</span>2<span class="fn-bracket">]</span></a> and two citations <a class="citation-reference" href="#once" id="citation-reference-1" role="doc-biblioref">[once]</a> <a class="citation-reference" href="#twice" id="citation-reference-2" role="doc-biblioref">[twice]</a>.</p>
<p>The latter are referenced a second time <a class="footnote-reference brackets" href="#f2" id="footnote-reference-3" role="doc-noteref"><span class="fn-bracket">[</span>2<span class="fn-bracket">]</span></a> <a class="citation-reference" href="#twice" id="citation-reference-3" role="doc-biblioref">[twice]</a>.</p>
<aside class="footnote brackets" id="f1" role="note">
<span class="label"><span class="fn-bracket">[</span>1<span class="fn-bracket">]</span></span>
<p>referenced once</p>
</aside>
<aside class="footnote brackets" id="f2" role="note">
<span class="label"><span class="fn-bracket">[</span>2<span class="fn-bracket">]</span></span>
<p>referenced twice</p>
</aside>
<div role="list" class="citation-list">
<div class="citation" id="once" role="doc-biblioentry">
<span class="label"><span class="fn-bracket">[</span>once<span class="fn-bracket">]</span></span>
<p>citation referenced once</p>
</div>
<div class="citation" id="twice" role="doc-biblioentry">
<span class="label"><span class="fn-bracket">[</span>twice<span class="fn-bracket">]</span></span>
<p>citation referenced twice</p>
</div>
</div>
</main>\\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\\n'''}
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
