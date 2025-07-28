.. include:: ../header.rst

=====================
Docutils HTML writers
=====================

.. contents::

This document describes the HTML writers provided by Docutils.

The default `length unit`_ in HTML is "px" (pixels, 1 px = 1/96 in).

.. _length unit: ../ref/rst/restructuredtext.html#length-units

html
----

:front-end: rst2html_

The writer name `html` is an alias for the default Docutils HTML writer.

The default may change with the development of HTML, browsers, Docutils,
and the web.
Currently, `html` is mapped to html4css1_, it will become an alias for
html5_ in Docutils 2.0.

* Use ``get_writer_by_name('html')`` or the rst2html_ front end, if you
  want the output to be up-to-date automatically.

* Use a specific writer name or front end, if you depend on stability of the
  generated HTML code, e.g. because you use a custom style sheet or
  post-processing that may break otherwise.


html5
-----

:aliases:   _`html5_polyglot`, xhtml
:front-end: rst2html5_
:config:    `[html5 writer]`_

The *html5* writer generates valid XML that conforms to the
`HTML standard`_ (`polyglot HTML`_). [#safetext]_
New features and elements are used if they are widely supported.
See the `HTML5 test page`_ (and the sources `html5-features.rst`_ and
`html5-text-level-tags.rst`_) for differences to the html4css1_ writer.

There is no hard-coded formatting information in the HTML document.
Correct rendering of elements not directly supported by HTML depends on a
CSS_ style sheet. The provided style sheet minimal.css_ defines required
styling rules; plain.css_ and responsive.css_ add optional rules for
better legibility. Adaption of the layout is possible with `custom style
sheets`_. [#safetext]_

Image_ size values with unit are converted to "style" rules,
values without unit are rounded to the nearest integer and
written as "width" and "height" attributes instead.
This allows the specification of the image's aspect ratio
(to `prevent jank when loading images`__) without overwriting
size declarations in a CSS stylesheet.

.. [#safetext] The validity of raw HTML and custom stylesheets must be
   ensured by the author.

.. _HTML5 test page: https://docutils.sourceforge.io/test/functional/
    expected/standalone_rst_html5.html#differences-to-the-html4css1-writer
.. _html5-features.rst: https://docutils.sourceforge.io/test/functional/
    input/data/html5-features.rst
.. _html5-text-level-tags.rst: https://docutils.sourceforge.io/test/functional/
    input/data/html5-text-level-tags.rst
.. _rst2html5: tools.html#rst2html5
.. _[html5 writer]: config.html#html5-writer
.. _minimal.css: ../../docutils/writers/html5_polyglot/minimal.css
.. _plain.css: ../../docutils/writers/html5_polyglot/plain.css
.. _responsive.css: ../../docutils/writers/html5_polyglot/responsive.css
.. _custom style sheets: ../howto/html-stylesheets.html
.. _viewable with any browser: http://www.anybrowser.org/campaign
.. _Benefits of polyglot XHTML5: http://xmlplease.com/xhtml/xhtml5polyglot/
.. _image: ../ref/rst/directives.html#image
__ https://developer.mozilla.org/en-US/docs/Learn/Performance/Multimedia
   #rendering_strategy_preventing_jank_when_loading_images


html4css1
---------

:aliases:   html4, html_, xhtml10
:front-end: rst2html4_
:config:    `[html4css1 writer]`_

The HTML Writer module, ``docutils/writers/html4css1.py``, was the first
Docutils writer and up to release 0.13 the only official HTML writer.

The output conforms to the `XHTML 1 Transitional`_ specification. It does
not validate as `HTML 4.01 Transitional`_ due to the closing of empty tags
required in XML but not allowed in HTML 4. However, the output follows the
`HTML Compatibility Guidelines`_ for proper rendering on most HTML user
agents.

Correct rendering depends on a CSS_ style sheet. A reference style sheet,
`html4css1.css`_, is provided and used by default.

To support the `Internet Explorer` (with a market share of about 90%
around 2002, the time this writer was written), documents contain some
hard-coded formatting hints and are tagged as "text/html" (instead of
"application/xhtml+xml"). Additional class values serve as surrogate for
the "first"/"last" pseudo-classes introduced in CSS 2.1. [#IE]_

Tables are used for description lists, field lists, docinfo, footnotes,
and option lists. Videos and SVG images are wrapped in <object> elements
and cannot be embedded.

.. [#IE] Conformance to `CSS 2.1`_ has been added in IE 8 (2009), support
   for XHTML in IE 9 (2011).

.. _rst2html: tools.html#rst2html
.. _rst2html4: tools.html#rst2html4
.. _[html4css1 writer]: config.html#html4css1-writer
.. _html4css1.css: ../../docutils/writers/html4css1/html4css1.css

pep_html
~~~~~~~~

:front-end: ``docutils  --reader=pep --writer=pep_html``
:config:    `[pep_html writer]`_, `[html4css1 writer]`_

This is a special writer for the generation of `Python Enhancement
Proposals`_ (PEPs). It inherits from html4css1_ and adds some `PEP-specific
options`_, a style sheet and template. It works best in combination with
the specialised "pep_html" reader.

.. _PEP-specific options:
.. _[pep_html writer]: config.html#pep-html-writer
.. _Python Enhancement Proposals: https://peps.python.org/

s5_html
~~~~~~~

:alias:     s5
:front-end: rst2s5_
:config:    `[s5_html writer]`_, `[html4css1 writer]`_

The `s5` writer inherits from html4css1_. It produces XHTML for use with
S5_, the “Simple Standards-based Slide Show System” by Eric Meyer.  See
`Easy Slide Shows With reST & S5`_ for details.

.. _rst2s5: tools.html#rst2s5
.. _[s5_html writer]: config.html#s5-html-writer
.. _Easy Slide Shows With reST & S5: slide-shows.html
.. _S5: http://meyerweb.com/eric/tools/s5/
.. _theme: tools.html#themes


3rd-party HTML writers
----------------------

For additional HTML writers, see the `Docutils link list`__
and the sandbox_.

__ https://docutils.sourceforge.io/docs/user/links.html
   #website-generators-and-html-variants
.. _sandbox: ../dev/policies.html#the-sandbox


References
----------

_`HTML Standard`
   `HTML Living Standard`.
   https://html.spec.whatwg.org/multipage/

_`XHTML 1 Transitional`
   `Transitional version`_ of:
   `XHTML™ 1.0 The Extensible HyperText Markup Language (Second
   Edition)`, `A Reformulation of HTML 4 in XML 1.0`,
   W3C Recommendation, 26 January 2000, revised 1 August 2002.
   https://www.w3.org/TR/xhtml1/

_`HTML 4.01 Transitional`
  Transitional version of:
  `HTML 4.01 Specification`, W3C Recommendation 24 December 1999.
  https://www.w3.org/TR/html4/

.. _`CSS 1`:

_`CSS Level 1`:
  The features defined in the `CSS1 specification`_, but using the syntax
  and definitions in the `CSS 2.1`_ specification.

_`CSS 2.1`
  `Cascading Style Sheets Level 2 Revision 1 (CSS 2.1) Specification`,
  W3C Recommendation 07 June 2011.
  https://www.w3.org/TR/CSS21/

_`CSS 3`:
  CSS Level 3 builds on CSS Level 2 module by module, using the CSS 2.1
  specification as its core.

  | Specifications: https://www.w3.org/Style/CSS/specs.en.html
  | Validator: http://jigsaw.w3.org/css-validator/

.. other references
   ----------------

.. _HTML Compatibility Guidelines: https://www.w3.org/TR/xhtml1/#guidelines
.. _transitional version:
    https://www.w3.org/TR/xhtml1/#a_dtd_XHTML-1.0-Transitional

.. _polyglot HTML: https://www.w3.org/TR/html-polyglot/

   .. Beware. This specification is no longer in active maintenance and the
      HTML Working Group does not intend to maintain it further.

.. _CSS: https://www.w3.org/TR/CSS/
.. _CSS1 specification: https://www.w3.org/TR/2008/REC-CSS1-20080411/

.. Appendix


      On the question of Polyglot markup, there seems to be little
      consensus. One line of argument suggests that, to the extent that it
      is practical to obey the Robustness principle, it makes sense to do
      so. That is, if you're generating HTML markup for the web, and you can
      generate Polyglot markup that is also directly consumable as XML, you
      should do so. Another line of argument suggests that even under the
      most optimistic of projections, so tiny a fraction of the web will
      ever be written in Polyglot that there's no practical benefit to
      pursuing it as a general strategy for consuming documents from the
      web. If you want to consume HTML content, use an HTML parser that
      produces an XML-compatible DOM or event stream.

      -- https://www.w3.org/TR/html-xml-tf-report/#conclusions

  Further development

  On 2016-05-25, David Goodger wrote:

  > In addition, I'd actually like to see the HTML writer(s) with
  > fully-parameterized classes, i.e. removing hard-coded *classes* as well as
  > formatting. This way, any user who wants to (e.g.) write reST for use with
  > Bootstrap can easily work around any naming conflicts.

.. _front-end: tools.html
