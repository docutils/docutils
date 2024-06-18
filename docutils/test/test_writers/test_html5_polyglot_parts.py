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
    REQUIRES_PIL = ''
    ONLY_LOCAL = 'Can only read local images.'
    DUMMY_PNG_NOT_FOUND = "[Errno 2] No such file or directory: 'dummy.png'"
    # Pillow reports the absolute path since version 10.3.0 (cf. [bugs: 485])
    if (tuple(int(i) for i in PIL.__version__.split('.')) >= (10, 3)):
        DUMMY_PNG_NOT_FOUND = ("[Errno 2] No such file or directory: '%s'"
                               % Path('dummy.png').resolve())
    SCALING_OUTPUT = 'style="width: 32.0px; height: 32.0px;" '
    NO_PIL_SYSTEM_MESSAGE = ''
else:
    REQUIRES_PIL = '\n  Requires Python Imaging Library.'
    ONLY_LOCAL = 'Requires Python Imaging Library.'
    DUMMY_PNG_NOT_FOUND = 'Requires Python Imaging Library.'
    SCALING_OUTPUT = ''
    NO_PIL_SYSTEM_MESSAGE = (
        '<aside class="system-message">\n'
        '<p class="system-message-title">System Message:'
        ' WARNING/2 (<span class="docutils literal">'
        '&lt;string&gt;</span>, line 1)</p>\n'
        '<p>Cannot scale image!\n'
        '  Could not get size from &quot;/data/blue%20square.png&quot;:\n'
        '  Requires Python Imaging Library.</p>\n'
        '</aside>\n')


class Html5WriterPublishPartsTestCase(unittest.TestCase):
    """Test case for HTML writer via the publish_parts interface."""

    maxDiff = None

    def test_publish(self):
        if not with_pygments:
            del totest['syntax_highlight']
        writer = 'html5'
        for name, (settings_overrides, cases) in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    parts = docutils.core.publish_parts(
                        source=case_input,
                        writer=writer,
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                            'stylesheet': '',
                            'section_self_link': True,
                            **settings_overrides,
                        }
                    )
                    self.assertEqual(case_expected, self.format_output(parts))

    standard_content_type_template = '<meta charset="%s" />\n'
    standard_generator_template = (
        '<meta name="generator"'
        f' content="Docutils {docutils.__version__}: '
        'https://docutils.sourceforge.io/" />\n')
    standard_viewport_value = (
        '<meta name="viewport"'
        ' content="width=device-width, initial-scale=1" />\n')
    standard_html_meta_value = (standard_content_type_template
                                + standard_generator_template
                                + standard_viewport_value)
    standard_meta_value = standard_html_meta_value % 'utf-8'
    standard_html_prolog = '<!DOCTYPE html>\n'
    standard_html_body_template = '<main>\n%s</main>\n'

    def format_output(self, parts):
        """Minimize & standardize the output."""
        # remove redundant parts & uninteresting parts:
        del parts['whole']
        assert parts['body'] == parts['fragment']
        del parts['body']
        del parts['body_pre_docinfo']
        del parts['body_prefix']
        del parts['body_suffix']
        del parts['head']
        del parts['head_prefix']
        del parts['encoding']
        del parts['errors']
        del parts['version']
        # remove standard portions:
        parts['meta'] = parts['meta'].replace(self.standard_meta_value, '')
        parts['html_head'] = parts['html_head'].replace(
            self.standard_html_meta_value, '...')
        parts['html_head'] = parts['html_head'].replace(
            '...<title>&lt;string&gt;</title>\n', '')
        parts['html_prolog'] = parts['html_prolog'].replace(
            self.standard_html_prolog, '')
        parts['html_body'] = parts['html_body'].replace(
            self.standard_html_body_template % parts['fragment'], '')
        # remove empty keys and return
        return {k: v for k, v in parts.items() if v}


totest = {}

totest['standard'] = ({'stylesheet_path': '',
                       'embed_stylesheet': False}, [
["""\
Simple String
""",
{'fragment': '<p>Simple String</p>\n',
}],
["""\
Simple String with *markup*
""",
{'fragment': '<p>Simple String with <em>markup</em></p>\n',
}],
["""\
Simple String with an even simpler ``inline literal``
""",
{'fragment': '<p>Simple String with an even simpler <span class="docutils literal">inline literal</span></p>\n',
}],
["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
{'fragment': '<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>\n',
}],
["""\
One paragraph.

Two paragraphs.
""",
{'fragment': """\
<p>One paragraph.</p>
<p>Two paragraphs.</p>
""",
}],
["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
{'fragment': """\
<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
""",
}],
["""\
.. [CIT2022] A citation.
""",
{'fragment': """\
<div role="list" class="citation-list">
<div class="citation" id="cit2022" role="doc-biblioentry">
<span class="label"><span class="fn-bracket">[</span>CIT2022<span class="fn-bracket">]</span></span>
<p>A citation.</p>
</div>
</div>
""",
}],
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
{'fragment': """\
<p>Some stuff</p>
<section id="section">
<h2>Section<a class="self-link" title="link to this section" href="#section"></a></h2>
<p>Some more stuff</p>
<section id="another-section">
<h3>Another Section<a class="self-link" title="link to this section" href="#another-section"></a></h3>
<p>And even more stuff</p>
</section>
</section>
""",
 'html_body': """\
<main id="title">
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
</main>
""",
 'html_head': '...<title>Title</title>\n',
 'html_subtitle': '<p class="subtitle" id="subtitle">Subtitle</p>\n',
 'html_title': '<h1 class="title">Title</h1>\n',
 'subtitle': 'Subtitle',
 'title': 'Title'
}],
["""\
+++++
Title
+++++

:author: me

Some stuff
""",
{'docinfo': """\
<dl class="docinfo simple">
<dt class="author">Author<span class="colon">:</span></dt>
<dd class="author"><p>me</p></dd>
</dl>
""",
 'fragment': '<p>Some stuff</p>\n',
 'html_body': """\
<main id="title">
<h1 class="title">Title</h1>
<dl class="docinfo simple">
<dt class="author">Author<span class="colon">:</span></dt>
<dd class="author"><p>me</p></dd>
</dl>
<p>Some stuff</p>
</main>
""",
 'html_head': """\
...<meta name="author" content="me" />
<title>Title</title>
""",
 'html_title': '<h1 class="title">Title</h1>\n',
 'meta': '<meta name="author" content="me" />\n',
 'title': 'Title'
}],
])

totest['no_title_promotion'] = ({'doctitle_xform': False,
                                 'stylesheet_path': '',
                                 'embed_stylesheet': False}, [
["""\
Simple String
""",
{'fragment': '<p>Simple String</p>\n',
}],
["""\
Simple String with *markup*
""",
{'fragment': '<p>Simple String with <em>markup</em></p>\n',
}],
["""\
Simple String with an even simpler ``inline literal``
""",
{'fragment': '<p>Simple String with an even simpler <span class="docutils literal">inline literal</span></p>\n',
}],
["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
{'fragment': '<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>\n',
}],
["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
{'fragment': """\
<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
""",
}],
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
{'fragment': """\
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
""",
}],
["""\
* bullet
* list
""",
{'fragment': """\
<ul class="simple">
<li><p>bullet</p></li>
<li><p>list</p></li>
</ul>
""",
}],
["""\
.. table::
   :align: right

   +-----+-----+
   |  1  |  2  |
   +-----+-----+
   |  3  |  4  |
   +-----+-----+
""",
{'fragment': """\
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
""",
}],
["""\
Not a docinfo.

:This: .. _target:

       is
:a:
:simple:
:field: list
""",
{'fragment': """\
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
""",
}],
["""\
Not a docinfo.

:This is: a
:simple field list with loooong field: names
""",
{'fragment': """\
<p>Not a docinfo.</p>
<dl class="field-list simple">
<dt>This is<span class="colon">:</span></dt>
<dd><p>a</p>
</dd>
<dt>simple field list with loooong field<span class="colon">:</span></dt>
<dd><p>names</p>
</dd>
</dl>
""",
}],
["""\
Not a docinfo.

.. class:: field-indent-200

:This: is a
:simple: field list with custom indent.
""",
{'fragment': """\
<p>Not a docinfo.</p>
<dl class="field-list simple" style="--field-indent: 200px;">
<dt>This<span class="colon">:</span></dt>
<dd><p>is a</p>
</dd>
<dt>simple<span class="colon">:</span></dt>
<dd><p>field list with custom indent.</p>
</dd>
</dl>
""",
}],
["""\
Not a docinfo.

.. class:: field-indent-200uf

:This: is a
:simple: field list without custom indent,
         because the unit "uf" is invalid.
""",
{'fragment': """\
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
""",
}],
["""\
.. figure:: dummy.png

   The figure's caption.

   A legend.

   The legend's second paragraph.
""",
{'fragment': """\
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
""",
}],
["""\
.. figure:: dummy.png

   The figure's caption, no legend.
""",
{'fragment': """\
<figure>
<img alt="dummy.png" src="dummy.png" />
<figcaption>
<p>The figure's caption, no legend.</p>
</figcaption>
</figure>
""",
}],
["""\
.. figure:: dummy.png

   ..

   A legend without caption.
""",
{'fragment': """\
<figure>
<img alt="dummy.png" src="dummy.png" />
<figcaption>
<div class="legend">
<p>A legend without caption.</p>
</div>
</figcaption>
</figure>
""",
}],
["""\
.. figure:: dummy.png

No caption nor legend.
""",
{'fragment': """\
<figure>
<img alt="dummy.png" src="dummy.png" />
</figure>
<p>No caption nor legend.</p>
""",
}],
[f"""\
.. include:: {DATA_ROOT}/multiple-term-definition.xml
   :parser: xml
""",
{'fragment': """\
<dl>
<dt>New in Docutils 0.22</dt>
<dd><p>A definition list item may contain several
terms with optional classifier(s).</p>
<p>However, there is currently no corresponding
reStructuredText syntax.</p>
</dd>
<dt>term 2a</dt>
<dt>term 2b</dt>
<dd><p>definition 2</p>
</dd>
<dt>term 3a<span class="classifier">classifier 3a</span><span class="classifier">classifier 3aa</span><dt>term 3b<span class="classifier">classifier 3b</span></dt>
<dd><p>definition 3</p>
</dd>
</dl>
""",
}],
])


totest['lazy_loading'] = ({'image_loading': 'lazy',
                           'stylesheet_path': '',
                           'embed_stylesheet': False,
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
{'fragment': """\
<p>Lazy loading by default, overridden by :loading: option
(&quot;cannot embed&quot; warning ignored).</p>
<img alt="dummy.png" loading="lazy" src="dummy.png" />
<img alt="dummy.png" src="dummy.png" />
<figure>
<img alt="dummy.png" loading="lazy" src="dummy.png" />
</figure>
<figure>
<img alt="dummy.png" src="dummy.png" />
</figure>
""",
}],
])


totest['root_prefix'] = ({'root_prefix': ROOT_PREFIX,
                          'image_loading': 'embed',
                          'stylesheet_path': '',
                          'warning_stream': '',
                          'embed_stylesheet': False
                          }, [
["""\
.. image:: /data/blue%20square.png
   :scale: 100%
.. figure:: /data/blue%20square.png
""",
{'fragment': '<img alt="/data/blue%20square.png" src="data:image/png;base64,'
             'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAIAAAD8GO2jAAAALElEQVR4nO3NMQ'
             'EAMAjAsDFjvIhHFCbgSwU0kdXvsn96BwAAAAAAAAAAAIsNnEwBk52VRuMAAAAA'
             'SUVORK5CYII="'
             f' {SCALING_OUTPUT}/>\n{NO_PIL_SYSTEM_MESSAGE}'
             '<figure>\n'
             '<img alt="/data/blue%20square.png" src="data:image/png;base64,'
             'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAIAAAD8GO2jAAAALElEQVR4nO3NMQ'
             'EAMAjAsDFjvIhHFCbgSwU0kdXvsn96BwAAAAAAAAAAAIsNnEwBk52VRuMAAAAA'
             'SUVORK5CYII=" />\n'
             '</figure>\n',
}],
])


totest['no_backlinks'] = ({'footnote_backlinks': False,
                           'stylesheet_path': '',
                           'embed_stylesheet': False}, [

["""\
Two footnotes [#f1]_ [#f2]_ and two citations [once]_ [twice]_.

The latter are referenced a second time [#f2]_ [twice]_.

.. [#f1] referenced once
.. [#f2] referenced twice
.. [once] citation referenced once
.. [twice] citation referenced twice
""",
{'fragment': """\
<p>Two footnotes <a class="brackets" href="#f1" id="footnote-reference-1" role="doc-noteref"><span class="fn-bracket">[</span>1<span class="fn-bracket">]</span></a> <a class="brackets" href="#f2" id="footnote-reference-2" role="doc-noteref"><span class="fn-bracket">[</span>2<span class="fn-bracket">]</span></a> and two citations <a class="citation-reference" href="#once" id="citation-reference-1" role="doc-biblioref">[once]</a> <a class="citation-reference" href="#twice" id="citation-reference-2" role="doc-biblioref">[twice]</a>.</p>
<p>The latter are referenced a second time <a class="brackets" href="#f2" id="footnote-reference-3" role="doc-noteref"><span class="fn-bracket">[</span>2<span class="fn-bracket">]</span></a> <a class="citation-reference" href="#twice" id="citation-reference-3" role="doc-biblioref">[twice]</a>.</p>
<aside class="footnote-list brackets">
<aside class="footnote brackets" id="f1" role="doc-footnote">
<span class="label"><span class="fn-bracket">[</span>1<span class="fn-bracket">]</span></span>
<p>referenced once</p>
</aside>
<aside class="footnote brackets" id="f2" role="doc-footnote">
<span class="label"><span class="fn-bracket">[</span>2<span class="fn-bracket">]</span></span>
<p>referenced twice</p>
</aside>
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
""",
}],
])


totest['syntax_highlight'] = ({'syntax_highlight': 'short',
                               'stylesheet_path': '',
                               }, [
["""\
.. code:: shell

    cat <<EOF
    Hello World
    EOF
""",
{'fragment': """\
<pre class="code shell literal-block"><code>cat <span class="s">&lt;&lt;EOF
Hello World
EOF</span></code></pre>
""",
}],
["""\
.. role:: shell(code)
   :language: shell

:shell:`cat <<EOF Hello World EOF`
""",
{'fragment': """\
<p><code class="shell">cat <span class="s">&lt;&lt;EOF Hello World EOF</span></code></p>
""",
}],
])


totest['system_messages'] = ({'stylesheet_path': '',
                              'embed_stylesheet': False,
                              'math_output': 'mathml',
                              'warning_stream': '',
                              }, [
["""\
.. image:: https://dummy.png
   :loading: embed
""",
{'fragment': """\
<img alt="https://dummy.png" src="https://dummy.png" />
<aside class="system-message">
<p class="system-message-title">System Message: ERROR/3 \
(<span class="docutils literal">&lt;string&gt;</span>, line 1)</p>
<p>Cannot embed image &quot;https://dummy.png&quot;:
  Can only read local images.</p>
</aside>
""",
}],
[f"""\
.. image:: {DATA_ROOT}/circle-broken.svg
   :loading: embed
""",
{'fragment': f"""\
<svg xmlns="http://www.w3.org/2000/svg"
     viewBox="0 0 10 10">
  <circle cx="5" cy="5" r="4" fill="lightblue" x/>
</svg>

<aside class="system-message">
<p class="system-message-title">System Message: ERROR/3 (<span class="docutils literal">&lt;string&gt;</span>, line 1)</p>
<p>Cannot parse SVG image &quot;{DATA_ROOT}/circle-broken.svg&quot;:
  not well-formed (invalid token): line 3, column 48</p>
</aside>
"""
}],
[r"""Broken :math:`\sin \my`.
""",
{'fragment': """\
<p>Broken <span class="math problematic">\\sin \\my</span>.</p>
<aside class="system-message">
<p class="system-message-title">System Message: WARNING/2 (<span class="docutils literal">&lt;string&gt;</span>, line 1)</p>
<p>Unknown LaTeX command &quot;\\my&quot;.</p>
</aside>
"""}],
])

totest['system_messages-PIL'] = ({'stylesheet_path': '',
                                  'embed_stylesheet': False,
                                  'math_output': 'mathml',
                                  'warning_stream': '',
                                  }, [
["""\
.. image:: dummy.png
   :scale: 100%
   :loading: embed
""",
{'fragment': f"""\
<img alt="dummy.png" src="dummy.png" />
<aside class="system-message">
<p class="system-message-title">System Message: WARNING/2 \
(<span class="docutils literal">&lt;string&gt;</span>, line 1)</p>
<p>Cannot scale image!
  Could not get size from &quot;dummy.png&quot;:
  {DUMMY_PNG_NOT_FOUND}</p>
</aside>
<aside class="system-message">
<p class="system-message-title">System Message: ERROR/3 \
(<span class="docutils literal">&lt;string&gt;</span>, line 1)</p>
<p>Cannot embed image &quot;dummy.png&quot;:
  [Errno 2] No such file or directory: 'dummy.png'</p>
</aside>
""",
}],
["""\
.. image:: dummy.mp4
   :scale: 100%
""",
{'fragment': f"""\
<video src="dummy.mp4" title="dummy.mp4">
<a href="dummy.mp4">dummy.mp4</a>
</video>
<aside class="system-message">
<p class="system-message-title">System Message: WARNING/2 \
(<span class="docutils literal">&lt;string&gt;</span>, line 1)</p>
<p>Cannot scale image!
  Could not get size from &quot;dummy.mp4&quot;:{REQUIRES_PIL}
  PIL cannot read video images.</p>
</aside>
""",
}],
["""\
.. image:: https://dummy.png
   :scale: 100%
   :loading: embed
""",
{'fragment': f"""\
<img alt="https://dummy.png" src="https://dummy.png" />
<aside class="system-message">
<p class="system-message-title">System Message: WARNING/2 \
(<span class="docutils literal">&lt;string&gt;</span>, line 1)</p>
<p>Cannot scale image!
  Could not get size from &quot;https://dummy.png&quot;:
  {ONLY_LOCAL}</p>
</aside>
<aside class="system-message">
<p class="system-message-title">System Message: ERROR/3 \
(<span class="docutils literal">&lt;string&gt;</span>, line 1)</p>
<p>Cannot embed image &quot;https://dummy.png&quot;:
  Can only read local images.</p>
</aside>
""",
}],
])

totest['no_system_messages'] = ({'stylesheet_path': '',
                                 'embed_stylesheet': False,
                                 'math_output': 'mathml',
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
{'fragment': """\
<img alt="dummy.png" src="dummy.png" />
<video src="dummy.mp4" title="dummy.mp4">
<a href="dummy.mp4">dummy.mp4</a>
</video>
""",
}],
[f"""\
.. image:: {DATA_ROOT}/circle-broken.svg
   :loading: embed
""",
{'fragment': """\
<svg xmlns="http://www.w3.org/2000/svg"
     viewBox="0 0 10 10">
  <circle cx="5" cy="5" r="4" fill="lightblue" x/>
</svg>

"""}],
[r'Broken :math:`\sin \my`.',
{'fragment': '<p>Broken <tt class="math">\\sin \\my</tt>.</p>\n'
}],
])


if __name__ == '__main__':
    unittest.main()
