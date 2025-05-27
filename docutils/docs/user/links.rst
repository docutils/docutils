.. include:: ../header.rst

=====================
 Docutils_ Link List
=====================

:Author: Lea Wiemann, the Docutils team
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$ [#]_
:Copyright: This document has been placed in the public domain.
:Abstract: Links that users of Docutils_ and reStructuredText_ may find useful.

.. title:: Docutils Links

.. contents::
   :depth: 2

.. [#] The most current version of this link list can always be found at
   https://docutils.sourceforge.io/docs/user/links.html. If you find
   outdated or broken links or want to suggest additions, please `let us
   know`__ and we'll update the list here.

.. _Docutils: https://docutils.sourceforge.io/
.. _reStructuredText: https://docutils.sourceforge.io/rst.html
__ mailing-lists.html#docutils-users


Extensions
==========

Drop-in components and front-end tools for Docutils that extend the
reStructuredText syntax or feature set.

Import
------

Markdown
````````

* `myst-docutils`_ --- the MyST_ Markdown parser for `single page builds`_.

  .. _myst-docutils: https://pypi.org/project/myst-docutils/
  .. _MyST: https://mystmd.org/guide/quickstart-myst-markdown
  .. _single page builds:
     https://myst-parser.readthedocs.io/en/latest/docutils.html

* pycmark_ --- small, customizable drop-in parser for CommonMark_ Markdown.

  .. _pycmark: https://pypi.org/project/pycmark/
  .. _CommonMark: https://spec.commonmark.org/current/


.. Python 2 only

  PySource_, by Tony Ibbs, is an experimental Python source Reader.
  There is some related code in David Goodger's sandbox
  (pysource_reader_) and a `Python Source Reader`_ document.

  .. _PySource: https://docutils.sourceforge.io/sandbox/tibs/pysource/
  .. _pysource_reader: https://docutils.sourceforge.io/sandbox/davidg/pysource_reader/
  .. _Python Source Reader: https://docutils.sourceforge.io/docs/dev/pysource.html



.. Python 2 only

  The `ASCII art to SVG converter`_ (aafigure_) developed by
  Chris Liechti can parse ASCII art images, embedded in reST documents and
  output an image. This would mean that simple illustrations could be
  embedded as ASCII art in the reST source and still look nice when
  converted to e.g. HTML

  .. _ASCII art to SVG converter:
     https://docutils.sourceforge.io/sandbox/aafigure/
  .. _aafigure: https://pypi.org/project/docutils-aafigure/

.. only Python 2

  restxsl_ by Michael Alyn Miller, lets you transform reStructuredText
  documents into XML/XHTML files using XSLT stylesheets.

  .. _restxsl: http://www.strangeGizmo.com/products/restxsl/


Export
------

PDF
```

* RinohType_ --- pure Python PDF writer

  .. _RinohType: https://pypi.python.org/pypi/RinohType

* `rst2pdf (reportlab)`__ --- PDF writer based on ReportLab_.

  __ https://pypi.org/project/rst2pdf/
  .. _ReportLab: https://pypi.org/project/reportlab/

.. only Python 2

  `rst2pdf (rubber)`__ --- front end for PDF generation via LaTeX
  using the rubber_ building system.

  __ https://docutils.sourceforge.io/sandbox/rst2pdf_rubber/README.html
  .. _rubber: https://gitlab.com/latex-rubber/rubber/

 `rst2pdf (pdflatex)`__ --- minimal front end for PDF generation via LaTeX.

  __ https://docutils.sourceforge.io/sandbox/blais/rst2pdf/

.. Python 2 only (or broken)

  rlpdf_ --- another PDF writer based on ReportLab_.

  .. _rlpdf: https://docutils.sourceforge.io/sandbox/dreamcatcher/rlpdf/

  >>> from rlpdf import Writer
  ImportError: cannot import name 'Writer' from partially initialized
  module 'rlpdf' (most likely due to a circular import)


Others
``````

* `DocBook Writer`_ (only Python 2)

  .. _DocBook Writer: https://docutils.sourceforge.io/sandbox/oliverr/docbook/

* `pickle writer`_ --- "pickle__" the document tree to a binary string.

  .. _pickle writer:
      https://docutils.sourceforge.io/sandbox/blais/pickle_writer/
  __ https://docs.python.org/3/library/pickle.html

* rst2confluence__ converts reStructuredText to Confluence__ CMS markup.

  __ https://github.com/netresearch/rst2confluence
  __ https://www.atlassian.com/software/confluence

* rst2epub2_ (only Python 2)

  .. _rst2epub2: https://github.com/mattharrison/rst2epub2

----------------------------------------------------------

There are more extensions in the `Docutils Sandbox`_.

.. _Docutils Sandbox: https://docutils.sourceforge.io/sandbox/README.html


Tools
=====

* a Makefile_ for driving Docutils,

  .. _Makefile: https://docutils.sourceforge.io/sandbox/cben/make/

* rstcheck_ checks syntax of reStructuredText and code blocks nested
  within it.

  .. _rstcheck: https://pypi.python.org/pypi/rstcheck

* restview_ --- live preview for reStructuredText documents,

  .. _restview: https://mg.pov.lt/restview/


Editors
-------

Editors and IDEs with reStructuredText support.

* Eclipse_ IDE with `ReST Editor`__ plug-in.

  .. _Eclipse: https://eclipseide.org/
  __  http://resteditor.sourceforge.net/

* Emacs__ extensible text editor with `rst mode`__.

  __ https://www.gnu.org/software/emacs/
  __ https://docutils.sourceforge.io/tools/editors/emacs/

* Geany_ lightweight IDE.

  .. _geany: http://www.geany.org/

* gedit_ with the gedit-reST-plugin__.

  .. _gedit: https://gedit-text-editor.org/
  __ https://github.com/bittner/gedit-reST-plugin

* JED__ programmers editor with `rst mode`__.

  __ https://www.jedsoft.org/jed/
  __ https://jedmodes.sourceforge.io/mode/rst/

* Leo_ PIM, IDE and outliner (see `creating documents from outlines`__).

  .. _Leo: https://leo-editor.github.io/leo-editor/
  __ https://leo-editor.github.io/leo-editor/tutorial-rst3.html

* `Notepad++`_ with reStructuredText_NPP__ (basic syntax highlighting).

  .. _Notepad++: https://www.notepad-plus-plus.org/
  __ https://github.com/steenhulthin/reStructuredText_NPP

* ReSTedit_ --- Docutils GUI for Mac OS X.

  .. _ReSTedit: https://svn.red-bean.com/restedit/trunk/README.html

* ReText_ --- editor for markup languages with live preview.

  .. _ReText: https://pypi.org/project/ReText/

* RSTPad_ --- reStructuredText editor with live preview.

  .. _RSTPad: https://github.com/shira-374/rstpad?tab=readme-ov-file#rstpad

* `Vim <https://www.vim.org/>`__ with

  - `rest.vim <https://www.vim.org/scripts/script.php?script_id=973>`__
    reStructuredText syntax mode,
  - `VST <https://www.vim.org/scripts/script.php?script_id=1334>`__
    (Vim reStructuredText) plugin,
  - `VOoM <https://www.vim.org/scripts/script.php?script_id=2657>`__
    two-pane text outliner, or
  - `Riv: <https://github.com/Rykka/riv.vim>`__
    Notes and wiki in rST.

* `Visual Studio Code`__ with `vscode-restructuredtext`__
  Language Support extension.

  __ https://code.visualstudio.com
  __ https://github.com/vscode-restructuredtext/vscode-restructuredtext


.. \http://rst.ninjs.org/ is down, the repository marked as UNMAINTAINED:

  `rsted <https://github.com/anru/rsted>`_: a simple online editor for
  reStructuredText.

.. last update 2004, Python 2 -> incompatible with Docutils > 0.18

  DocFactory_ is a wxPython GUI application for Docutils.
  The last version (from 2004) still requires Python 2.

  .. _DocFactory:
   https://docutils.sourceforge.io/sandbox/gschwant/docfactory/doc/


Related Applications
====================

Applications using Docutils/reStructuredText and helper applications.


Converters
----------

Alternative implementations to convert between reStructuredText
and other formats.

* Pandoc_ --- universal document converter written in Haskell.
  Can read/write reStructuredText, DocBook, EPUB, HTML, LaTeX,
  docx, Markdown, ODT, and more.

  .. _Pandoc: https://pandoc.org/

* Laika_ --- Site and E-book Generator and Customizable Text Markup
  Transformer for sbt, Scala and Scala.js.

  .. _Laika: https://typelevel.org/Laika/

* Nim_ --- system programming language.  Supports `Nim-flavoured
  reStructuredText`__ with the rst__, rstast__, and rstgen__ modules.
  The Nim compiler provides the  ``nim rst2html`` and ``nim rst2latex``
  sub-commands.

  .. _Nim: https://nim-lang.org/
  __ https://nim-lang.org/docs/markdown_rst.html
  __ https://nim-lang.org/docs/rst.html
  __ http://nim-lang.org/docs/rstast.html
  __ http://nim-lang.org/docs/rstgen.html

* RST__ --- PHP library to parse reStructuredText documents.

  __ https://github.com/Gregwar/RST

* Text-Restructured_ --- a set of Perl_ modules to parse reStructuredText
  documents and output them in various formats.

  .. _Text-Restructured: https://metacpan.org/dist/Text-Restructured
  .. _Perl: https://www.perl.org

* xml2rst_ --- an XSLT stylesheet that converts `Docutils XML`_
  back to reStructuredText.

  .. _xml2rst: http://www.merten-home.de/FreeSoftware/xml2rst/index.html
  .. _Docutils XML: ../ref/doctree.html

Website Generators
------------------

* Sphinx_ Documentation Generator.

  .. _Sphinx: https://www.sphinx-doc.org

* ipsumgenera_ --- static blog generator written in Nim.

  .. _ipsumgenera: https://github.com/dom96/ipsumgenera

* Nikola_ --- static site generator.

  .. _nikola:  https://getnikola.com/

* Pelican_ --- another static site generator.

  .. _pelican: https://docs.getpelican.com

* rest2web_ --- simple tool for creating web sites with reStructuredText.

  .. _rest2web: https://gitlab.com/wavexx/rest2web

* Yozuch_ --- reStructuredText based static blog generator written in Python.

  .. _Yozuch: https://github.com/akrylysov/yozuch


Wikis
-----

* MoinMoin_ includes a `ReStructuredText parser`__.

  .. _MoinMoin: http://moinmo.in/
  __ http://moinmo.in/HelpOnParsers/ReStructuredText

* Trac_ supports  `reStructuredText as alternative wiki markup`__.

  __ https://trac.edgewall.org/wiki/WikiRestructuredText


Development
-----------

* Sphinx_ extends the ReStructuredText syntax to better support the
  documentation of Software projects.

  `Sphinx Extensions`_ allow automatic testing of code snippets,
  inclusion of docstrings from Python modules (API docs), and more.

  .. _Sphinx extensions: https://www.sphinx-doc.org/en/master/usage/extensions/

* Trac_, a wiki and issue tracking system, supports
  `reStructuredText as alternative wiki markup`__.
  This includes support for TracLinks__ via a `:trac: role`__.

  .. _Trac: https://trac.edgewall.org
  __ https://trac.edgewall.org/wiki/WikiRestructuredText
  __ https://trac.edgewall.org/wiki/TracLinks
  __ https://trac.edgewall.org/wiki/WikiRestructuredTextLinks

* PyLit_ provides a bidirectional text ↔ code converter for Literate
  Programming with reStructuredText.

  .. _PyLit: https://codeberg.org/milde/pylit


CMS Systems
-----------

* Plone_ supports `reStructuredText content`__.

  __ https://6.docs.plone.org/backend/fields.html
     #restructuredtext-transformation

* Zope_ (Zope 4 `dropped the ReST shim`__).

  __ https://github.com/zopefoundation/Products.CMFDefault/issues/5

.. _Plone: https://plone.org/
.. _Zope: https://www.zope.dev/


Presentations
-------------

There is native support for `slide shows with S5`__.

__ https://docutils.sourceforge.io/docs/user/slide-shows.s5.html

* InkSlide_ quick and easy presentations using Inkscape_. InkSlide uses
  reStructuredText for markup, although it renders only a subset of rST.

  .. _InkSlide: http://wiki.inkscape.org/wiki/index.php/InkSlide
  .. _Inkscape: http://inkscape.org/

* landslide_ generates HTML5 slideshows from markdown, ReST, or textile.

  .. _landslide: https://github.com/adamzap/landslide

* `marianoguerra/rst2html5`__ can produce slides using
  HTML5 + deck.js, impress.js, or reveal.js.

  __ https://github.com/marianoguerra/rst2html5

* Pandoc_ can export to various slide show formats
  (LaTeX Beamer, PowerPoint, Slidy, S5, ...).

* rst2odp_ --- rst to LibreOffice impress.

  .. _rst2odp: https://github.com/mattharrison/rst2odp


Help / Q&A
==========

* Stackoverflow tags `[restructuredtext]`__ and `[docutils]`__.

  __ https://stackoverflow.com/questions/tagged/restructuredtext
  __ https://stackoverflow.com/questions/tagged/docutils
