#! /usr/bin/env python3

# $Id$
# Author: reggie dugard <reggie@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""Test HTML4 writer output ("fragment" part).

This is the document body (not HTML <body>).
"""

from pathlib import Path
import re
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

import docutils
import docutils.core
from docutils.parsers.rst.directives.images import PIL
from docutils.utils.code_analyzer import with_pygments
from docutils.writers import html4css1

if with_pygments:
    import pygments
    _pv = re.match(r'^([0-9]+)\.([0-9]*)', pygments.__version__)
    if (int(_pv[1]), int(_pv[2])) >= (2, 14):
        # pygments output changed in version 2.14
        with_pygments = False

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = Path(__file__).parents[1]
DATA_ROOT = TEST_ROOT / 'data'
ROOT_PREFIX = (TEST_ROOT / 'functional/input').as_posix()

# Pillow/PIL is optional:
if PIL:
    SCALING_OUTPUT = 'style="width: 32.0px; height: 32.0px;" '
else:
    SCALING_OUTPUT = ''


class Html5WriterPublishPartsTestCase(unittest.TestCase):
    """Test case for HTML5 writer via the publish_parts() interface."""

    maxDiff = None

    def test_publish(self):
        if not with_pygments:
            del totest['syntax_highlight']
        for name, (settings_overrides, cases) in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    parts = docutils.core.publish_parts(
                        source=case_input,
                        writer=html4css1.Writer(),
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                            'stylesheet_path': '',
                            'section_self_link': True,
                            **settings_overrides,
                        }
                    )
                    self.assertEqual(case_expected, parts['body'])


totest = {}

totest['standard'] = ({}, [
["""\
Simple String
""",
'<p>Simple String</p>\n',
],
["""\
Simple String with *markup*
""",
'<p>Simple String with <em>markup</em></p>\n',
],
["""\
Simple String with an even simpler ``inline literal``
""",
'<p>Simple String with an even simpler <tt class="docutils literal">inline literal</tt></p>\n',
],
["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
'<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>\n',
],
["""\
One paragraph.

Two paragraphs.
""",
"""\
<p>One paragraph.</p>
<p>Two paragraphs.</p>
""",
],
["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
"""\
<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
""",
],
["""\
.. [CIT2022] A citation.
""",
"""\
<table class="docutils citation" frame="void" id="cit2022" rules="none">
<colgroup><col class="label" /><col /></colgroup>
<tbody valign="top">
<tr><td class="label">[CIT2022]</td><td>A citation.</td></tr>
</tbody>
</table>
""",
],
])


totest['no_title_promotion'] = ({'doctitle_xform': False}, [
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
<div class="section" id="title">
<h1>Title</h1>
<div class="section" id="not-a-subtitle">
<h2>Not A Subtitle</h2>
<p>Some stuff</p>
<div class="section" id="section">
<h3>Section</h3>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h4>Another Section</h4>
<p>And even more stuff</p>
</div>
</div>
</div>
</div>
""",
],
["""\
* bullet
* list
""",
"""\
<ul class="simple">
<li>bullet</li>
<li>list</li>
</ul>
""",
],
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
<table border="1" class="docutils align-right">
<colgroup>
<col width="50%" />
<col width="50%" />
</colgroup>
<tbody valign="top">
<tr><td>1</td>
<td>2</td>
</tr>
<tr><td>3</td>
<td>4</td>
</tr>
</tbody>
</table>
""",
],
["""\
Not a docinfo.

:This: .. _target:

       is
:a:
:simple:
:field: list
""",
"""\
<p>Not a docinfo.</p>
<table class="docutils field-list" frame="void" rules="none">
<col class="field-name" />
<col class="field-body" />
<tbody valign="top">
<tr class="field"><th class="field-name">This:</th><td class="field-body"><p class="first last" id="target">is</p>
</td>
</tr>
<tr class="field"><th class="field-name">a:</th><td class="field-body"></td>
</tr>
<tr class="field"><th class="field-name">simple:</th><td class="field-body"></td>
</tr>
<tr class="field"><th class="field-name">field:</th><td class="field-body">list</td>
</tr>
</tbody>
</table>
""",
],
["""\
Not a docinfo.

:This is: a
:simple field list with loooong field: names
""",
"""\
<p>Not a docinfo.</p>
<table class="docutils field-list" frame="void" rules="none">
<col class="field-name" />
<col class="field-body" />
<tbody valign="top">
<tr class="field"><th class="field-name">This is:</th><td class="field-body">a</td>
</tr>
<tr class="field"><th class="field-name" colspan="2">simple field list with loooong field:</th></tr>
<tr class="field"><td>&nbsp;</td><td class="field-body">names</td>
</tr>
</tbody>
</table>
""",
],
["""\
Not a docinfo.

.. class:: field-indent-200

:This: is a
:simple: field list with custom indent.
""",
"""\
<p>Not a docinfo.</p>
<table class="field-indent-200 docutils field-list" frame="void" rules="none">
<col class="field-name" />
<col class="field-body" />
<tbody valign="top">
<tr class="field"><th class="field-name">This:</th><td class="field-body">is a</td>
</tr>
<tr class="field"><th class="field-name">simple:</th><td class="field-body">field list with custom indent.</td>
</tr>
</tbody>
</table>
""",
],
["""\
Not a docinfo.

.. class:: field-indent-200uf

:This: is a
:simple: field list without custom indent,
         because the unit "uf" is invalid.
""",
"""\
<p>Not a docinfo.</p>
<table class="field-indent-200uf docutils field-list" frame="void" rules="none">
<col class="field-name" />
<col class="field-body" />
<tbody valign="top">
<tr class="field"><th class="field-name">This:</th><td class="field-body">is a</td>
</tr>
<tr class="field"><th class="field-name">simple:</th><td class="field-body">field list without custom indent,
because the unit &quot;uf&quot; is invalid.</td>
</tr>
</tbody>
</table>
""",
],
["""\
.. figure:: dummy.png

   The figure's caption.

   A legend.

   The legend's second paragraph.
""",
"""\
<div class="figure">
<img alt="dummy.png" src="dummy.png" />
<p class="caption">The figure's caption.</p>
<div class="legend">
<p>A legend.</p>
<p>The legend's second paragraph.</p>
</div>
</div>
""",
],
["""\
.. figure:: dummy.png

   The figure's caption, no legend.
""",
"""\
<div class="figure">
<img alt="dummy.png" src="dummy.png" />
<p class="caption">The figure's caption, no legend.</p>
</div>
""",
],
["""\
.. figure:: dummy.png

   ..

   A legend without caption.
""",
"""\
<div class="figure">
<img alt="dummy.png" src="dummy.png" />
<div class="legend">
A legend without caption.</div>
</div>
""",
],
["""\
.. figure:: dummy.png

No caption nor legend.
""",
"""\
<div class="figure">
<img alt="dummy.png" src="dummy.png" />
</div>
<p>No caption nor legend.</p>
""",
],
[f"""\
.. include:: {DATA_ROOT}/multiple-term-definition.xml
   :parser: xml
""",
"""\
<dl class="docutils">
<dt>New in Docutils 0.22</dt>
<dd><p class="first">A definition list item may contain several
terms with optional classifier(s).</p>
<p class="last">However, there is currently no corresponding
reStructuredText syntax.</p>
</dd>
<dt>term 2a</dt>
<dt>term 2b</dt>
<dd>definition 2</dd>
<dt>term 3a <span class="classifier-delimiter">:</span> <span class="classifier">classifier 3a</span> <span class="classifier-delimiter">:</span> <span class="classifier">classifier 3aa</span><dt>term 3b <span class="classifier-delimiter">:</span> <span class="classifier">classifier 3b</span></dt>
<dd>definition 3</dd>
</dl>
""",
],
])


totest['lazy_loading'] = ({'image_loading': 'lazy',
                           'report_level': 4}, [
["""\
Lazy loading by default, overridden by :loading: option
("cannot embed" warning ignored).

.. image:: dummy.png
.. image:: dummy.png
   :loading: link
.. figure:: dummy.png
.. figure:: dummy.png
   :loading: embed
""",
"""\
<p>Lazy loading by default, overridden by :loading: option
(&quot;cannot embed&quot; warning ignored).</p>
<img alt="dummy.png" src="dummy.png" />
<img alt="dummy.png" src="dummy.png" />
<div class="figure">
<img alt="dummy.png" src="dummy.png" />
</div>
<div class="figure">
<img alt="dummy.png" src="dummy.png" />
</div>
""",
],
])


totest['root_prefix'] = ({'root_prefix': ROOT_PREFIX,
                          'image_loading': 'embed',
                          'warning_stream': '',
                          }, [
["""\
.. image:: /data/blue%20square.png
   :scale: 100%
.. figure:: /data/blue%20square.png
""",
f"""\
<img alt="/data/blue%20square.png" src="/data/blue%20square.png" {SCALING_OUTPUT}/>
<div class="figure">
<img alt="/data/blue%20square.png" src="/data/blue%20square.png" />
</div>
"""
],
])


totest['no_backlinks'] = ({'footnote_backlinks': False}, [

["""\
Two footnotes [#f1]_ [#f2]_ and two citations [once]_ [twice]_.

The latter are referenced a second time [#f2]_ [twice]_.

.. [#f1] referenced once
.. [#f2] referenced twice
.. [once] citation referenced once
.. [twice] citation referenced twice
""",
"""\
<p>Two footnotes <a class="footnote-reference" href="#f1" id="footnote-reference-1">[1]</a> <a class="footnote-reference" href="#f2" id="footnote-reference-2">[2]</a> and two citations <a class="citation-reference" href="#once" id="citation-reference-1">[once]</a> <a class="citation-reference" href="#twice" id="citation-reference-2">[twice]</a>.</p>
<p>The latter are referenced a second time <a class="footnote-reference" href="#f2" id="footnote-reference-3">[2]</a> <a class="citation-reference" href="#twice" id="citation-reference-3">[twice]</a>.</p>
<table class="docutils footnote" frame="void" id="f1" rules="none">
<colgroup><col class="label" /><col /></colgroup>
<tbody valign="top">
<tr><td class="label">[1]</td><td>referenced once</td></tr>
</tbody>
</table>
<table class="docutils footnote" frame="void" id="f2" rules="none">
<colgroup><col class="label" /><col /></colgroup>
<tbody valign="top">
<tr><td class="label">[2]</td><td>referenced twice</td></tr>
</tbody>
</table>
<table class="docutils citation" frame="void" id="once" rules="none">
<colgroup><col class="label" /><col /></colgroup>
<tbody valign="top">
<tr><td class="label">[once]</td><td>citation referenced once</td></tr>
</tbody>
</table>
<table class="docutils citation" frame="void" id="twice" rules="none">
<colgroup><col class="label" /><col /></colgroup>
<tbody valign="top">
<tr><td class="label">[twice]</td><td>citation referenced twice</td></tr>
</tbody>
</table>
""",
],
])


totest['syntax_highlight'] = ({'syntax_highlight': 'short',
                               }, [
["""\
.. code:: shell

    cat <<EOF
    Hello World
    EOF
""",
"""\
<pre class="code shell literal-block">
cat <span class="s">&lt;&lt;EOF
Hello World
EOF</span>
</pre>
""",
],
["""\
.. role:: shell(code)
   :language: shell

:shell:`cat <<EOF Hello World EOF`
""",
"""\
<p><code class="shell">cat <span class="s">&lt;&lt;EOF Hello World EOF</span></code></p>
""",
],
])


totest['system_messages'] = ({'math_output': 'mathml',
                              'warning_stream': '',
                              }, [
# No warning with HTML4 ("embed" error is silently ignored).
["""\
.. image:: https://dummy.png
   :loading: embed
""",
"""\
<img alt="https://dummy.png" src="https://dummy.png" />
""",
],
# No error with HTML4 (silently ignored)
[f"""\
.. image:: {DATA_ROOT.as_uri()}/circle-broken.svg
   :loading: embed
""",
f"""\
<object data="{DATA_ROOT.as_uri()}/circle-broken.svg" type="image/svg+xml">{DATA_ROOT.as_uri()}/circle-broken.svg</object>
"""
],
[r"""Broken :math:`\sin \my`.
""",
"""\
<p>Broken <span class="math problematic">\\sin \\my</span>.</p>
<div class="system-message">
<p class="system-message-title">System Message: WARNING/2 (<tt class="docutils">&lt;string&gt;</tt>, line 1)</p>
Unknown LaTeX command &quot;\\my&quot;.</div>
"""],
])

totest['system_messages-PIL'] = ({'math_output': 'mathml',
                                  'warning_stream': '',
                                  }, [
["""\
.. image:: dummy.png
   :scale: 100%
   :loading: embed
""",
"""\
<img alt="dummy.png" src="dummy.png" />
""",
],
["""\
.. image:: dummy.mp4
   :scale: 100%
""",
"""\
<object data="dummy.mp4" type="video/mp4">dummy.mp4</object>
""",
],
["""\
.. image:: https://dummy.png
   :scale: 100%
   :loading: embed
""",
"""\
<img alt="https://dummy.png" src="https://dummy.png" />
""",
],
])

totest['no_system_messages'] = ({'math_output': 'mathml',
                                 'report_level': 4,
                                 'warning_stream': '',
                                 }, [
["""\
.. image:: dummy.png
   :scale: 100%
   :loading: embed

.. image:: dummy.mp4
   :scale: 100%
""",
"""\
<img alt="dummy.png" src="dummy.png" />
<object data="dummy.mp4" type="video/mp4">dummy.mp4</object>
""",
],
[f"""\
.. image:: {DATA_ROOT.as_uri()}/circle-broken.svg
   :loading: embed
""",
f"""\
<object data="{DATA_ROOT.as_uri()}/circle-broken.svg" type="image/svg+xml">{DATA_ROOT.as_uri()}/circle-broken.svg</object>
"""],
[r'Broken :math:`\sin \my`.',
'<p>Broken <tt class="math">\\sin \\my</tt>.</p>\n'
],
])


if __name__ == '__main__':
    unittest.main()
