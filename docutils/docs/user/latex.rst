.. include:: ../header.rst

================================
 Generating LaTeX with Docutils
================================

:Author: Engelbert Gruber, Günter Milde
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.
:Abstract: This document covers topics specific to Docutils' LaTeX_ export.

.. contents::
.. sectnum::


LaTeX
=====

LaTeX__, is a document preparation system for high-quality typesetting. It
is most often used for medium-to-large technical or scientific documents but
it can be used for almost any form of publishing. There exists a wide
selection of `LaTeX Documentation on the net`_ and `books on LaTeX and
related topics`_. For an introduction to LaTeX see, e.g., `LaTeX2e for
authors`_.

__ https://www.latex-project.org/
.. _LaTeX Documentation on the net: https://www.latex-project.org/guides/
.. _books on LaTeX and related topics:
    https://www.latex-project.org/guides/books.html
.. _LaTeX2e for authors: https://www.latex-project.org/guides/usrguide.pdf


.. _LaTeX packages:

LaTeX document classes and packages
-----------------------------------

Unlike HTML with CSS, LaTeX uses one common language for markup and style
definitions. Separation of content and style is realized by collecting style
definitions in LaTeX classes and packages, or the
`document preamble <LaTeX preamble_>`_.

LaTeX document classes and packages (similar to Python modules or C
libraries) provide means to extend or modify the LaTeX language by
redefining macros or providing new ones.

Using the `document class`_ and `style sheet`_ configuration settings, you
can select from a *huge* selection of classes and packages (standard as well
as user contributed) coming with your TeX distribution or available at
CTAN_ as well as custom style sheets.

.. _CTAN: https://www.ctan.org


Docutils specific LaTeX macros
------------------------------

Some Docutils objects have no LaTeX counterpart, they will be typeset
using a Docutils specific LaTeX *macro* (command, environment, or length)
to allow customization.  By convention, these macros use the prefix
``\DU``\ [#]_.
Fallback definitions are included after the `custom style sheets`_,
for all macros required in the document.  Alternatively, you may list
"docutils" in the `stylesheet`_ setting to use the `"docutils" package`_
instead.

* Custom `style sheets`_ can define alternative implementations with
  ``\newcommand``, ``\newenvironment``, and ``\newlength`` followed by
  ``\setlength``.

* Definitions with `raw LaTeX`_ are part of the document body.  Use
  ``\def``, ``\renewcommand`` or ``\renewenvironment``, and ``\setlength``.

See docutils.sty_ and the test output `standalone_rst_latex.tex`_ for
an example of the fallback definitions and their use in the document.

.. [#] DU for Documentation Utilities = Docutils

.. _"docutils" package: https://ctan.org/pkg/docutils
.. _docutils.sty:
    https://mirrors.ctan.org/macros/latex/contrib/docutils/docutils.sty.html


.. _length unit:

Length units
------------

The LaTeX writer supports all `reStructuredText length units`_
with the following peculiarities:

* In LaTeX, the size of the *pixel unit* defaults to **1 px = 1/72 in**
  while the `CSS3 pixel unit`_ is defined as 1 px = 1/96 in.
  The LaTeX conversion factor can be configured,
  see below for the `size of a "px"`_.

* LaTeX uses "pt" for the `American point`_ (*TeX point*), 1 pt = 1/72.25 in.
  The `DTP point`_ (*Postscript point*) used in CSS is available in LaTeX
  as *big point*, 1 bp = 1/72 in.

  Lengths specified in the source with unit "pt" are written with unit
  "bp" by the LaTeX writer.  In `raw LaTeX`_ and `custom style sheets`_,
  the `DTP point` must be specified as "bp", while "pt" is interpreted as
  `TeX point`.

* The **default length unit** (added by the latex writer to length values
  without unit) is the `DTP point` "**bp**".  It will change to "px" in
  Docutils 1.0.

The TeX units:

.. class:: align-center

====  =========================  ===================
 bp   "big" point (`DTP point`)  1 bp  = 1/72 in
 cc   cîcero                     1 cc = 12 dd
 dd   didôt                      1 dd = 1238/1157 pt
 sp   scaled point               1sp = 1/65536pt
====  =========================  ===================

can be used in `raw LaTeX`_ and `custom style sheets`_ but not in
reStructuredText.

.. _CSS3 pixel unit: https://www.w3.org/TR/css-values-3/#px
.. _reStructuredText length units:
    ../ref/rst/restructuredtext.html#length-units
.. _American point:
    https://en.wikipedia.org/wiki/Point_(typography)#American_points
.. _DTP point:
    https://en.wikipedia.org/wiki/Point_(typography)#Desktop_publishing_point


PDF generation
==============

In most cases, LaTeX code is not the desired end-format of the document.
There are several programs to generate PDF documents from the LaTeX source,
including:

_`pdflatex`
  Generates a PDF document directly from the LaTeX file.
  Export your document with the _`LaTeX2e writer` (writer
  name "``latex``", frontend tool rst2latex_).

_`xelatex` or _`lualatex`
  The `XeTeX`_ and LuaTeX_ engines work with input files in UTF-8 encoding
  and system fonts. Export your document with the _`XeLaTeX writer` (writer
  name "``xetex``", frontend tool rst2xetex_).

You may need to call the respective command two or three times
to get internal references correct.

.. _xetex: https://tug.org/xetex/
.. _luatex: https://www.luatex.org/
.. _rst2latex: tools.html#rst2latex
.. _rst2xetex: tools.html#rst2xetex

_`rubber`
  The Rubber__ wrapper for LaTeX and friends can be used to automatically
  run all programs the required number of times and delete "spurious" files.
  This includes processing bibliographic references or indices, as well as
  compilation or conversion of figures.

__ https://gitlab.com/latex-rubber/rubber/


Configuration
=============

.. contents:: :local:

.. _option:
.. _setting:
.. _settings:

Options/Settings
----------------

Docutils configuration settings can be specified as

* command-line options, or

* configuration settings.

Run ``rst2latex --help`` to get a list of available options;
see `Docutils Configuration`_ for details.

.. _Docutils Configuration: config.html

Classes
-------

The `"classes" attribute`_ is one of the common attributes, shared by all
Docutils elements.
In HTML, the common use is to provide selection criteria for style rules in
CSS stylesheets. As there is no comparable framework for LaTeX, Docutils
emulates some of this behaviour via `Docutils specific LaTeX macros`_.
Due to LaTeX limitations, class arguments are ignored for
some elements (e.g. a rubric_).

*Inline elements*
  are handled via the ``\DUrole{}`` macro that calls the optional styling
  command ``\DUrole«classargument»`` with one argument (the role content).
  See `custom interpreted text roles`_.

*Block level elements*
  are wrapped in "class environments":
  ``\begin{DUclass}`` calls the optional styling command
  ``\DUCLASS«classargument»{}``, ``\end{DUclass}`` tries
  ``\endDUCLASS«classargument»``.

Customization is done by defining matching macros or environments.

Example 1:
  Use small caps font inside elements with class value "custom".

  *Inline elements*
    The LaTeX function ``\textsc`` sets the argument in small caps::

      \newcommand{\DUrolecustom}[1]{\textsc{#1}}

  *Block-level elements*
    The LaTeX directive (macro without argument) ``\scshape`` switches to
    the small caps font. Its effect is confined to the wrapper ``DUclass``
    environment::

      \newcommand*{\DUCLASScustom}{\scshape}

Example 2:
  It is even possible to locally redefine other LaTeX macros, e.g. to
  turn bullet lists with class value "enumerateitems" into enumerated
  lists::

    \newcommand*{\DUCLASSenumerateitems}{%
      \renewenvironment{itemize}{\begin{enumerate}}%
                                {\end{enumerate}}%
    }

.. rubric:: Notes

* Class arguments may contain numbers and hyphens, which need special
  treatment in LaTeX command names (see `class directive`_). The commands
  ``\csname`` and ``\endcsname`` or the special command ``\@namedef`` can
  help with the definition of corresponding macros or environments, e.g.::

    \expandafter\newcommand\csname gg1\endcsname{Definition of gg1.}

  or ::

    \makeatletter
    \@namedef{DUCLASSadmonition-test}{…}
    \makeatother

* Elements can have multiple class arguments. In contrast to HTML/CSS, the
  order of the class arguments cannot be ignored in LaTeX

* Class handling differs for some elements and class values:

  * Class argument values starting with ``align-`` are transformed to
    "align" argument values. Class argument values starting with
    ``language-`` set the elements language property.

  * The table element recognizes some special class values. See section
    `table style`_.

  * If the legacy-class-functions_ setting is True, the special macros
    ``\DUadmonition`` and ``\DUtitle`` are written with a comma separated
    list of class values as optional argument.

.. _"classes" attribute: ../ref/doctree.html#classes
.. _legacy-class-functions: config.html#legacy-class-functions

LaTeX code
----------

Custom LaTeX code can be placed in `style sheets`_, the
`LaTeX preamble`_, the document body (`raw LaTeX`_), or custom templates_.

The functional tests that come with Docutils, can serve as example.

input:
  standalone_rst_latex.rst_ (includes files from `tests/functional/input/data`_)
expected output:
  standalone_rst_latex.tex_

.. _standalone_rst_latex.rst:
  https://sourceforge.net/p/docutils/code/HEAD/tree/trunk/docutils/test/functional/input/standalone_rst_latex.rst
.. _tests/functional/input/data:
  https://sourceforge.net/p/docutils/code/HEAD/tree/trunk/docutils/test/functional/input/data
.. _standalone_rst_latex.tex:
   https://sourceforge.net/p/docutils/code/HEAD/tree/trunk/docutils/test/functional/expected/standalone_rst_latex.tex


.. _style sheet:
.. _custom style sheets:

Style sheets
````````````

A common way of LaTeX customization is the preparation of custom style
sheets, either as simple files with LaTeX code snippets or as home-made
`LaTeX packages`_ (see the clsguide_ for an introduction on LaTeX
package writing).

Setting:
  stylesheet_

  It is possible to specify multiple style sheets and mix `LaTeX
  packages`_ with custom style sheets.

You cannot specify package options with the stylesheet_ setting. If
you need to pass options to the package, use the ``\usepackage``
command in the `LaTeX preamble`_ or a custom style sheet.

Example 1:
  Select Latin Modern fonts with the `lmodern` package::

    --stylesheet=lmodern

Example 2:
  Use the `preamble.tex` home-made custom style sheet together with
  the package `kerkis` (Bookman fonts)::

    --stylesheet=kerkis,preamble.tex

Example 3:
  Select Palatino fonts with old-style numbers and true small-caps
  with the LaTeX command ::

    \usepackage[osf,sc]{mathpazo}

  in the `LaTeX preamble`_ or `custom style sheets`_.

Stylesheet Repository
  There is a `repository of user-contributed style sheets`_ in the
  Docutils Sandbox_.

.. _clsguide: https://mirrors.ctan.org/macros/latex/base/clsguide.pdf
.. _stylesheet: config.html#stylesheet-latex-writers
.. _embed-stylesheet: config.html#embed-stylesheet-latex-writers
.. _repository of user-contributed style sheets:
   ../../../sandbox/stylesheets/
.. _sandbox: ../../../sandbox/


LaTeX preamble
``````````````

Configuration by LaTeX code in the document preamble is also possible
without a separate stylesheet. This way, packages can be loaded with
options or commands re-defined without the need to create a separate
file (new in Docutils 0.7).

Setting:
  latex-preamble_

Default:
  used for `font setup`_

Example:
  To use the better looking ``txtt`` font for monospaced text define the
  latex-preamble_ setting in a configuration file::

     latex-preamble: \renewcommand{\ttdefault}{txtt}
                     \usepackage{mathptmx}          % Times
                     \usepackage[scaled=.92]{helvet}  % Helvetica

.. _latex-preamble: config.html#latex-preamble
.. _PDF standard fonts: https://en.wikipedia.org/wiki/PDF#Standard_Type_1_Fonts
.. _Linux Libertine: https://www.linuxlibertine.org


Templates
`````````

Some customizations require commands at places other than the insertion
point of stylesheets or depend on the deletion/replacement of parts of the
document. This can be done via a custom template. See the `publisher
documentation`_ for a description of the `document parts`_ available in a
template file.

Setting:
  template_

In addition to the 'default.tex' template, the latex writer directory
contains the alternatives 'titlepage.tex' (separate title page) and
'titlingpage.tex'" (separate title page with the `memoir`_
`document class`_).

Example:
  Print a title page including docinfo, dedication, and abstract::

    --template=titlepage.tex

.. _publisher documentation: ../api/publisher.html
.. _document parts:
    ../api/publisher.html#parts-provided-by-the-xe-latex-writers
.. _template: config.html#template-latex-writers


Raw LaTeX
`````````

By means of the `raw directive`_ or a derived `custom role`_, one can
give commands directly to LaTeX. These can be both, styling as well as
printing commands.

Example:
  Math formula::

    .. raw:: latex

       \[x^3 + 3x^2a + 3xa^2 + a^3,\]

  (Drawback: the formula will be invisible in other output formats. Better
  use the `math directive`_)

Most LaTeX code examples also work as raw LaTeX inside the document.
An exception are commands that need to be given in the document
preamble (e.g. package loading with ``\usepackage``, which can be
achieved with the ``--style-sheet`` or ``--latex-preamble`` command
line options instead). Remember to use *re-defining* commands for
customizing `Docutils specific LaTeX macros`_ with raw LaTeX.

Example:
  Define the transition command as page break::

    .. raw:: latex

      \renewcommand*{\DUtransition}{\pagebreak[4]}

See also:
  * Defining a macro for a `custom role`_.
  * Forcing `page breaks`_.

.. _raw directive: ../ref/rst/directives.html#raw
.. _math directive: ../ref/rst/directives.html#math


How to configure the ...
========================

admonitions
-----------

Admonitions__ are specially marked "topics" that can appear anywhere an
ordinary body element can.

__ ../ref/rst/directives.html#admonitions

Environment:
  ``DUadmonition``

  (Command ``\DUadmonition`` with legacy-class-functions_.)

Default:
  Typeset in a frame (90 % of text width).

The admonition title is typeset with the ``\DUtitle`` command (see `titles`_).

Example 1:
  A lighter layout without the frame::

    \newenvironment{DUadmonition}%
      {\begin{quote}}
      {\end{quote}}

Example 2:
  Print all admonitions in the margin::

    \usepackage{environ}
    \NewEnviron{DUadmonition}{\marginpar{\BODY}}

Example 3:
  Use the ``.. note::`` admonition for a margin note::

    \usepackage{environ}
    \newcommand{\DUCLASSnote}{%
      \RenewEnviron{DUadmonition}{\marginpar{\BODY}}%
      \renewcommand{\DUtitle}[1]{}% suppress title ("Note")
    }

.. caution:: Make sure there is enough space in the margin.
   ``\marginpar`` fails in some places or with some content. See also the
   environ_ and marginnote_ packages.

.. _environ: https://ctan.org/pkg/environ
.. _marginnote: https://ctan.org/pkg/marginnote


.. _custom role:

custom interpreted text roles
-----------------------------

The rst `role directive`_ allows defining custom `text roles`_ that mark
parts of inline text (spans) with class arguments (see section classes_).

Commands:
  ``\DUrole``: dispatcher command

  ``\DUrole«classargument»``: optional styling command with 1 argument (the
  role content).

Default:
  The default definition of ``\DUrole{«classargument»}{}`` calls the macro
  named ``\DUrole«classargument»{}`` if it is defined and silently ignores
  this class argument if not.

Example 1:
  Typeset text in small caps::

    .. role:: smallcaps

    :smallcaps:`Fourier` transformation

  This is transformed to the LaTeX code::

    \DUrole{smallcaps}{Fourier} transformation

  The definition ::

    \newcommand{\DUrolesmallcaps}{\textsc}

  as `raw LaTeX`_ or in the custom `style sheet`_ will give the expected
  result (if the text font_ supports small caps).

Example 2:
  Subscript text in normal size and *italic* shape::

  .. role:: sub(subscript)

  As "sub" inherits from the standard "subscript" role, the LaTeX macro
  only needs to set the size and shape::

    \newcommand{\DUrolesub}{\normalsize\itshape}

Example 3:
  A role with several classes and a converted class name::

    .. role:: custom4
       :class: argI argII arg_3

  is translated to the nested commands::

    \DUrole{argi}{\DUrole{argii}{\DUrole{arg-3}{<content>}}}

  With the definitions::

    \newcommand{\DUroleargi}[1]{\textsc}
    \newcommand{\DUroleargii}[1]{{\large #1}}
    \makeatletter
    \@namedef{DUrolearg-3}{\textbf}
    \makeatother

  in a `style sheet`_\ [#]_ or as `raw LaTeX`_ in the document source,
  text styled with ``:custom4:`large bold small-caps``` will be typeset
  accordingly.

.. [#] Leave out the ``\makeatletter`` - ``\makeatother`` pair if the style
   sheet is a LaTeX package (``*.sty``).

.. _role directive: ../ref/rst/directives.html#role
.. _text roles: ../ref/rst/roles.html
.. _class directive: ../ref/rst/directives.html#class

definition lists
----------------

ReStructuredText `definition lists`__ correspond to HTML ``<dl>`` list
objects.

Environment:
  ``description``: LaTeX standard environment

Command:
  ``\descriptionlabel``: styling macro for the description term

Default:
  bold label text, hanging indent

Example:
  A non-bold label can be achieved with::

    \renewcommand\descriptionlabel[1]{\hspace\labelsep \normalfont #1}

__ ../ref/rst/restructuredtext.html#definition-lists


document class
--------------

There are hundreds of `LaTeX document classes`_ installed by modern
LaTeX distributions, provided by publishers, or available at CTAN_.

Setting:
  documentclass_

Popular document classes:
  * `Standard document classes`_: article, report, book
  * KOMA-script_ classes: scrartcl, scrrprt, scrbook
  * memoir_: a highly configurable document class for larger documents

.. _LaTeX document classes: https://texfaq.org/FAQ-clsvpkg
.. _standard document classes:
    http://mirrors.ctan.org/macros/latex/base/classes.pdf
.. _KOMA-script: https://ctan.org/pkg/koma-script
.. _memoir: https://ctan.org/pkg/memoir
.. _documentclass: config.html#documentclass


document info
-------------

The `bibliographic fields`_ at the top of a document specify
document information like author name(s) and copyright info.
By default, they are typeset as a table.

Settings:
  use-latex-docinfo_, use-latex-abstract_

  If use-latex-docinfo_ is True, the content of the "Author", "Authors",
  "Address", "Contact", "Organization", and "Date" fields is attached to
  the `document title`_ (see below).

  If use-latex-abstract_ is True, the "Abstract" field content is placed
  in an "abstract" environment. The "abstract" environment can be
  customized or `set up`__ (for the "book" documentclass) with the
  "abstract_" LaTeX package.

Length:
  | ``\DUdocinfowidth`` (width of the `docinfo` table).
  | Default: ``0.9\textwidth`` (90 % of text width)

Example:
  Change the width of the docinfo table to 70 % of text width::

    \newlength{\DUdocinfowidth}
    \setlength{\DUdocinfowidth}{0.7\textwidth}

.. _bibliographic fields: ../ref/rst/restructuredtext.html#bibliographic-fields
.. _use-latex-docinfo: config.html#use-latex-docinfo
.. _use-latex-abstract: config.html#use-latex-abstract
__ https://tex.stackexchange.com/a/539606/288060
.. _abstract: https://ctan.org/pkg/abstract


document title
--------------

A lone top-level section title is (usually) transformed to the document title
(see `section structure`_).

Settings:
  doctitle_xform_, documentclass_, use-latex-docinfo_, template_

The format of the document title is defined by the `document class`_
and can be modified by `latex packages`_.
The "article" document class uses an in-page title while the "report"
and "book" classes use a separate title page.
See section Templates_ and the `TeX FAQ`_ on how to customize the
`style of document titles`_.

Example:
  With `use-latex-docinfo`_, author information and date are typeset
  as part of the document title instead of the `document info`_ table.

  Paper by three authors, two of them from the same organisation::

    Shop Sketches
    =============

    :authors: * Mr. Mousebender
              * Mr. Arthur Wensleydale
    :organization: Ye National Cheese Emporium

    :author: Hr. Hallmackenreuther
    :organization: Bettengeschäft

  With ``--use-latex-docinfo``, authors and their affiliations are
  typeset below the title using the standard LaTeX command ``\author``
  with a `simple hack for the common affiliation`__. [#]_

.. [#] Unfortunately, author names don't wrap if there are more
   authors than fit on one line in one "authors" field.

__ https://tex.stackexchange.com/a/11656/288060from


.. _section structure: rst/quickref.html#section-structure
.. _doctitle_xform: config.html#doctitle-xform
.. _TeX FAQ: https://texfaq.org/
.. _style of document titles: https://texfaq.org/FAQ-titlsty


field lists
-----------

`Field lists`__ may be used as generic two-column table constructs in
documents.

Environment:
   ``DUfieldlist``

Default:
   Indented description list.

Example:
   Use a description list customized with enumitem_::

     \usepackage{enumitem}
     \newenvironment{DUfieldlist}%
       {\description[font=,style=sameline,leftmargin=8em]}
       {\enddescription}
     }

   The `KOMA-script`_ classes provide a similar environment under the name
   `labeling`.

.. _enumitem: https://ctan.org/pkg/enumitem
__ ../ref/rst/restructuredtext.html#field-lists


figure and table captions
-------------------------

The caption_ package provides many ways to customise the captions in
floating environments like figure and table.

The chngcntr_ package helps to configure the numbering of figure and table
caption numberings.

Some document classes (e.g. KOMA-script_) provide additional configuration.
See also `The style of captions`_ in the LaTeX FAQ.

Example
  ::

    \usepackage{caption}
    \captionsetup{justification=raggedleft,singlelinecheck=false}

.. _caption: https://ctan.org/pkg/caption
.. _chngcntr: https://ctan.org/pkg/chngcntr
.. _The style of captions: https://texfaq.org/FAQ-captsty

figure placement
----------------

Figures_ might be typeset at the place of definition (default) or "float"
to a suitable place at the top or bottom of a page. This is implemented
using the float_ package.

Command:
  ``\floatplacement``

The placement setting is valid from the point of definition until the next
``\floatplacement`` command or the end of the document. See float.pdf_ for
details.

Default:
  ``\floatplacement{figure}{H}`` (here definitely). This corresponds most
  closely to the source and HTML placement (principle of least surprise).

Example 1:
  In a custom `style sheet`_, set the default to let LaTeX find a suitable
  place for figure floats::

    \usepackage{float}
    \floatplacement{figure}{htbp} % here, top, bottom, extra-page

Example 2:
  To move all following figures to the top or bottom of the page write in
  the document source::

    .. raw:: latex

        \floatplacement{figure}{tb}

.. _figures: ../ref/rst/directives.html#figure
.. _float: https://ctan.org/pkg/float
.. _float.pdf: https://mirrors.ctan.org/macros/latex/contrib/float/float.pdf


.. _font setup:

font
----

The selected text font influences the *look*, the *feel*,
and the *readability* of the document
(cf. `Typography on the information highway`_).
Selecting a suitable font also solves the problem with
`bad looking PDF output`_.

Font selection is one of the main differences between pdflatex_
and xelatex_/lualatex_:

pdflatex_
  requires specially installed fonts in a set of legacy `font encoding`_\ s.

xelatex_ and lualatex_
  can use system fonts and provide access to the full feature set of
  modern OpenType_ fonts.

The default font setup is done in the latex-preamble_:

pdflatex_
  `PDF standard fonts`_ (Times, Helvetica, Courier)

xelatex_/lualatex_
  `Linux Libertine`_, a free, high quality alternative to Times with a
  wide coverage of glyphs, styles, and OpenType features.

  Despite its name, Linux Libertine can be used on any operating
  system that can handle OpenType fonts.

Alternative fonts can be selected by

pdflatex_
  a) specifying the corresponding LaTeX package(s) as argument to the
     stylesheet_ setting or with the ``\usepackage`` LaTeX command.

     * packages can be combined,
     * passing options to a package is only possible in a `style sheet`_
       or the `LaTeX preamble`_.

  b) changing the font-default macros ``\rmdefault``, ``\sfdefault``
     and/or ``\ttdefault`` in a custom `style sheet`_, the `LaTeX
     preamble`_ or `raw LaTeX`_.

  Example 1:
    Use `Latin Modern`_. `LaTeX code`_::

      \usepackage{lmodern}

    Command line argument::

      --stylesheet=lmodern

  Example 2:
    The _`Times/Helvetica/Courier` `PDF standard fonts`_ are
    selected by the LaTeX code [#]_::

      \usepackage{mathptmx}            % Times for serif and math
      \usepackage[scaled=.90]{helvet}  % downscaled Helvetica for sans serif
      \usepackage{courier}             % Courier for teletype (mono-space)

    Since Docutils 0.7, this is the default value of the
    `latex-preamble`_ setting.

  .. [#] When generating PDF-files from LaTeX, the `PDF standard
     fonts`_ do not need to be embedded in the document. While this
     results in smaller files, the actually used fonts on screen and in
     print might differ! (For details see, e.g., the testflow_ package
     documentation.)


  Example 3:
    Use the teletype font from the txfonts_ package. As there is no
    package for this, we re-define the font macro with the `LaTeX code`_::

      \renewcommand{\ttdefault}{txtt}


xelatex_/lualatex_
  using the macros of the fontspec_ package. Use some font-viewer or
  -manager (e.g. fontmatrix_) to find out the correct names of the
  fonts on your system.

  Example:
    DejaVu_, very wide coverage, screen optimized. As this font
    runs wide, add ``DIV=10`` to the `documentoptions`_::

      \setmainfont{DejaVu Serif}
      \setsansfont{DejaVu Sans}
      \setmonofont[HyphenChar=None]{DejaVu Sans Mono}

.. _Typography on the information highway: https://csarven.ca/web-typography
.. _OpenType: https://en.wikipedia.org/wiki/OpenType
.. _fontspec: https://ctan.org/pkg/fontspec
.. _fontmatrix: https://fontmatrix.net/
.. _DejaVu: https://dejavu-fonts.org/
.. _documentoptions: config.html#documentoptions


choice of suitable fonts
````````````````````````

With xelatex_/lualatex_, you may use any of the system fonts.

The `LaTeX Font Catalogue`_ provides information and examples for a wide
range of fonts available for use with PdfLaTeX. Here is just a selection:

a) The `Latin Modern`_ (LM) fonts are extended outline versions of the
   standard TeX font Computer Modern (CM).

   +1  simple invocation:  ``--stylesheet=lmodern``

   +1  keeps the traditional TeX "look and feel":

       +1  generally accepted as high quality CM replacement,
       +1  comprehensive math support,
       +1  including optical sizes,
       +1  compatible with extensions made to match CM,
       -1  modern types are hard to read at low (screen) resolutions.

   -1  not part of a minimal standard TeX installation

b) CM-Super_ is another outline CM replacement.

   +1  simple invocation: modern LaTeX distributions use CM-Super
       automatically instead of CM if it is installed.

   -1  said to be of inferior quality compared to LM.

   -1  not part of a minimal standard TeX installation,
       bigger download size than Latin Modern (64 MB).

c) `Bera`_ (Bitstream Vera)

   +1  simple invocation:  ``--stylesheet=bera``

   +1  optimized for on-screen viewing with goot hinting

   -1  not part of a minimal standard TeX installation

d) PSNFSS_ Postscript fonts

   +1  part of every standard TeX installation

   +1  smaller PDF/Postscript document size if standard fonts are not
       embedded

   -1  restricted set of glyphs in the free versions [#]_

   -1  different fonts for roman, sans-serif and typewriter fonts.

   -1  invocation somewhat more complex, as several packages are
       required for a complete font set, sometimes including package
       options.

   Roman (serif) PSNFSS fonts:

   Bookman
     good legibility but very wide.

   Charter
     bread-and-butter type optimized for printing on low-resolution
     printers

   New Century Schoolbook
     good legibility but very wide.

   Palatino
     +1  recommended by font experts
     +1  good LaTeX support including matching math fonts, small caps,
           old-style figures
     -1  bad rendering in xpdf viewer (auto-hinting leads to different
         x-hight for different characters at some magnifications)
         (this is fixed in recent versions).

   Times
     +1  the serif `PDF Standard Font`_,
     -1  overused and quite narrow (devised for multi-column layouts).

   Utopia
     recommended by font experts


   .. table:: Font packages for standard Postscript fonts
              (cf. `Using common Postscript fonts with LaTeX`_)

     ========= ============ ============= ============= =========
     Package   Roman        Sans Serif    Typewriter    Math
     ========= ============ ============= ============= =========
     (none)    CM Roman     CM Sans Serif CM Typewriter CM Math

     mathpazo  Palatino                                 Palatino

     mathptmx  Times                                    Times

     helvet                 Helvetica

     avant                  Avant Garde

     courier                              Courier

     chancery  Zapf
               Chancery

     bookman   Bookman      Avant Garde   Courier

     newcent   New Century  Avant Garde   Courier
               Schoolbook

     charter   Charter

     utopia    Utopia

     fourier   Utopia                                   Fourier
     ========= ============ ============= ============= =========

.. [#] Extended versions of the standard Postscript fonts including
       accented chars, as well as real small-caps
       and old-style numbers are available with the `TeX Gyre`_ bundle
       which is part of, e.g., `TeX Live`_.


.. _LaTeX Font Catalogue: https://www.tug.org/FontCatalogue/
.. _Latin Modern: https://ctan.org/pkg/lm
.. _CM-Super: https://ctan.org/pkg/cm-super
.. _bera: https://ctan.org/pkg/bera
.. _TeX Gyre: https://www.gust.org.pl/projects/e-foundry/tex-gyre
.. _PSNFSS: https://ctan.org/pkg/psnfss
.. _Using common PostScript fonts with LaTeX:
   https://mirrors.ctan.org/macros/latex/required/psnfss/psnfss2e.pdf
.. _TeX Live: https://www.tug.org/texlive/
.. _txfonts: https://ctan.org/pkg/txfonts
.. _PDF Standard Font:
   https://en.wikipedia.org/wiki/PDF#Standard_Type_1_Fonts
.. _testflow: https://ctan.org/pkg/testflow


font encoding
-------------

LaTeX font encodings are described in detail in the encguide_ which is
part of the LaTeX base documentation.

Setting:
  font-encoding_

Default:
  "T1"

Example 1:
  Use the (obsolete) LaTeX default encoding "OT1"::

    --font-encoding=OT1

  or (without loading the fontenc_ package)::

    --font-encoding=""

  This will improve the look on screen with the default Computer Modern
  fonts at the expense of problems with `search and text extraction`_
  The recommended way is to select a T1-encoded "Type 1" (vector)
  font, for example `Latin Modern`_

Example 2:
  Support for characters in the Unicode blocks Latin, Latin-1 Supplement,
  and Greek together with a T1-encoded "Type 1" (vector) font, for example
  `Latin Modern`_::

    --font-encoding=LGR,T1 --stylesheet=lmodern

.. _encguide: https://mirrors.ctan.org/macros/latex/base/encguide.pdf
.. _font-encoding: config.html#font-encoding
.. _fontenc: https://ctan.org/pkg/fontenc


font size
---------

Add font size in points to the document options, e.g.
``--documentoptions=12``, use e.g. the document classes provided by
extsizes_ for values other than [10,11,12].

.. _extsizes: https://ctan.org/pkg/extsizes


footnotes
---------

By default, footnotes are set with Docutils-specific wrappers around
the standard ``\footnotemark`` and ``\footnotetext`` commands.  You
can configure the footnote layout similar to standard LaTeX footnotes
in a custom `style sheet`_ or the `LaTeX preamble`_.

Further configuration is possible by alternative definitions of
``\DUfootnotemark`` and ``\DUfootnotetext``

Example 1:
  Set footnote text with a hanging indent.

  * This is the default with KOMA-script_ classes, e.g::

      --documentclass=scrartcl

    (for further configuration, see the `KOMA-script Guide`_),

  * with package footmisc_::

      \usepackage[hang]{footmisc}
      \setlength{\footnotemargin}{0em}

    (play with the ``\footnotemargin`` setting),

  * redefine ``\DUfootnotetext`` inserting `\hangindent`::

      \newcommand{\DUfootnotetext}[4]{%
        \begingroup%
        \renewcommand{\thefootnote}{%
          \protect\raisebox{1em}{\protect\hypertarget{#1}{}}%
          \protect\hyperlink{#2}{#3}}%
          \footnotetext{\hangindent=2em #4}%
        \endgroup%
      }

    (adapt the ``\hangindent`` value).

Example 2:
  Footnote marks in normal font size, not superscript::

    \usepackage{scrextend} % not required with KOMA-script document classes
    \deffootnote{1em}{1em}{\thefootnotemark\ }

  (See the `KOMA-script Guide`_ for details and other options.)

Example 3:
  Place the footnote text where it appears in the source document (instead
  of at the page bottom). This can be used to get the effect of endnotes
  (needs the hanging_ package)::

     \usepackage{hanging}
     \newcommand{\DUfootnotetext}[4]{%
       \par\noindent\raisebox{1em}{\hypertarget{#1}{}}%
       \hyperlink{#2}{#3}%
       \hangpara{\parindent}{1}#4%
     }

.. _footmisc: https://ctan.org/pkg/footmisc
.. _hanging: https://ctan.org/pkg/hanging


hyphenation
-----------

The amount of hyphenation is influenced by ``\hyphenpenalty``, setting it to
10000 almost prevents hyphenation. As this produces lines with more space
between words one should increase Latex's ``\tolerance`` for this.

Example:
  ::

    \hyphenpenalty=5000
    \tolerance=1000


hyperlinks
----------

Settings:
  hyperlink-color_, hyperref-options_

Hyperlinks are realized using the hyperref_ package. As it re-defines many
standard LaTeX macros, this package is loaded last, *after* the style
sheets.

However, you can load hyperref before a package that requires its
presence in a `style sheet`_ or the `LaTeX preamble`_ (see example
below). This will ignore options set with hyperlink-color_ and
hyperref-options_.

URLs are typeset with the "url" package (loaded implicitly by "hyperref").
The font of URLs can be defined with the ``\urlstyle`` command. Valid
arguments are

.. class:: field-indent-4em

:same:  normal text font, Docutils default,
:tt:    teletype (monospaced), LaTeX default,
:rm:    roman,
:sf:    sans serif

Example:
  Custom loading of the hyperref package also switches to
  the LaTeX default (monospaced fonts for URLs). Reset to use the text
  font::

   \usepackage[unicode,colorlinks=true,linkcolor=green]{hyperref}
   \urlstyle{same}

See also `non-breaking hyperlinks`_.

.. _hyperlink-color: config.html#hyperlink-color
.. _hyperref-options: config.html#hyperref-options


disable hyperlinks
``````````````````

To suppress the hyper-linking completely (e.g. for printing or to
avoid clashes with other packages), set hyperref-options_ to "draft"
or load the "nohyperref" package that comes with the "hyperref"
bundle.

Setting:
  hyperref-options_

`LaTeX code`_::

  \usepackage{nohyperref,url}
  \urlstyle{same}

.. _hyperref: https://ctan.org/pkg/hyperref


language
--------

The global document language can be set with the language-code_
configuration setting. The language of text parts can be set adding the
language tag prefixed by "language-" to an element's classes_
attribute, e.g. ``language-el`` for a Greek text part.

.. _language-code: config.html#language-code


line blocks
-----------

In `line blocks`__, newlines and leading whitespace are respected.

Environment:
  ``DUlineblock``: special list environment for line blocks

Length:
  ``\DUlineblockindent``: indentation of indented lineblock parts.

Default:
   2.5 times the font height: ``2.5em``

Example:
  set to the paragraph indentation::

    \newlength{\DUlineblockindent}
    \setlength{\DUlineblockindent}{\parindent}

__ ../ref/rst/restructuredtext.html#line-blocks

line spacing
------------

Commands:
  ``\linespread``: for small adjustments

  ``\singlespacing``, ``\onehalfspacing``, and ``\doublespacing``: from
  package `setspace`

Example 1:
  Get document wide double spacing::

    \usepackage{setspace}
    \doublespacing

Example 2:
  Increase line spacing by five percent for better readability::

    \linespread{1.05}


literal blocks
--------------

No markup processing is done within a `literal block`__. It is left as-is,
and is typically rendered in a monospaced typeface

Setting:
  literal-block-env_

Example:

  ``--literal-block-env=lstlisting``

  The ``lstlisting`` environment is highly configurable (as documented in
  listings.pdf_) and provides syntax highlight for many programming languages,
  for instance ::

    \renewcommand{\ttdefault}{txtt}
    \lstset{language=Python, morekeywords=[1]{yield}}
    \lstloadlanguages{Python}
    \lstset{
      basicstyle=\ttfamily,
      keywordstyle=\bfseries,
      commentstyle=\rmfamily\itshape,
      stringstyle=\slshape,
    }
    \lstset{showstringspaces=false}
    \lstset{columns=fullflexible,
         basewidth={0.5em,0.4em}}

  and to get LaTeX syntax highlight for a code block with "listings"::

    \lstloadlanguages{[LaTeX]TeX} %  comma separated list of languages
    \newcommand{\DUCLASSlatex}{\lstset{language=[LaTeX]TeX}}

  The indentation of literal blocks can be reset with ::

    \lstset{resetmargins=true}

  and/or configured with e. g.::

    \lstset{xleftmargin=-2em}

__ ../ref/rst/restructuredtext.html#literal-blocks
.. _literal-block-env: config.html#literal-block-env
.. _listings.pdf:
   https://mirrors.ctan.org/macros/latex/contrib/listings/listings.pdf


lists
-----

Remove extra vertical whitespace between items of bullet lists and
enumerated lists.

Example:
  Pass the class argument "compact" to the list::

    .. class:: compact

    * first item
    * second item

  The following lines for the `LaTeX preamble`_ use the enumitem_ package to
  remove spacing from all lists with class argument "compact"::

    \usepackage{enumitem}
    \newcommand*{\DUCLASScompact}{\setlist{noitemsep}}


list of figures/tables
----------------------

Docutils does not support lists of figures or tables.

However, with LaTeX, they can be generated using `raw LaTeX`_ in the
document source.

Commands:
  ``\listoffigures``: a list of figures

  ``\listoftables``: a list of tables

Example:
  ::

    .. raw:: latex

       \listoffigures


option list
-----------

`Option lists`__ are two-column lists of command-line options and
descriptions, documenting a program's options.

Environment:
  ``DUoptionlist``: environment for option lists,

Command:
  ``\DUoptionlistlabel``: set appearance of the options

Example:
  set command options with a bold monospace font::

    \newcommand{\DUoptionlistlabel}{\texttt{\textbf{#1}} \hfill}

__ ../ref/rst/restructuredtext.html#option-lists


page breaks
-----------

* Page breaks before top-level sections are the default with a
  documentclass that provides "chapters", e.g.  "book", "memoir" or
  "scrbook".

* Redefining the \section or \section* command in a
  style sheet is possible too.

* `Raw LaTeX`_ or a `custom role`_ can be used.

* The transition element can be re-defined to produce a page break,

Commands
  ``\newpage``:  hard pagebreak at exactly this position

  ``\pagebreak[2]``: recommended page break after line end (precedence 1...4)

Example:
  Define the transition command as page break with the `LaTeX code`_::

    \newcommand*{\DUtransition}{\pagebreak[4]}

  (use ``\renewcommand`` with `raw LaTeX`_).

page layout
-----------

By default, paper size and margin settings are determined by the document
class.

The following packages help to configure the page layout:

a) The `typearea`_ package (part of the `KOMA-script`_ bundle) calculates a
   *good* page layout (based on rules and recommendations of typography
   experts).

   See the `KOMA-Script Guide`_ for details on what is a *good* layout and
   how this is achieved.

b) The `geometry`_ package is recommended if you have to follow guidelines
   with fixed values for the margins.
   For details see the `geometry manual`_.

Example 1:
  Let `typearea` determine the type area with ``DIV=calc`` in the
  documentoptions::

    --documentoptions='a4paper,DIV=calc'

  The ``DIV`` option can also be specified, like ``DIV=10``. It defines how
  "crowded" a page will be: larger values mean larger text area (at the
  expense of readability).

Example 2:
  `LaTeX code`_ to set margins with the geometry_ package::

    \usepackage{geometry}
    \geometry{hmargin={3cm,0.8in},height=8in}
    \geometry{height=10in}.

.. _typearea: https://ctan.org/pkg/typearea
.. _geometry: https://ctan.org/pkg/geometry
.. _KOMA-Script Guide: https://mirrors.ctan.org/
    macros/latex/contrib/koma-script/doc/scrguide-en.pdf
.. _geometry manual:
   https://mirrors.ctan.org/macros/latex/contrib/geometry/geometry.pdf


page headers and footers
------------------------

With the fancyhdr_ package or the `KOMA-script`_ classes, you can define
custom page head- and foot-lines.

The `"header" and "footer" directives`_ save their content in the macros
``\DUheader`` rsp. ``\DUfooter``. The macros can be used in LaTeX code and
will be replaced by LaTeX with the content of the directives.

Example:
  `LaTeX code`_ to place left-aligned "header" and "footer" on every
  page with fancyhdr_::

    \usepackage{fancyhdr}
    \fancyhead[L]{\DUheader}
    \fancyfoot{} % reset
    \fancyfoot[L]{\DUfooter}
    \pagestyle{fancy}


.. _fancyhdr: https://ctan.org/pkg/fancyhdr
.. _"header" and "footer" directives: ../ref/rst/directives.html#header


page numbering
--------------

Example:
  Number pages by chapter (using the chappg_ package)::

    \usepackage{chappg}

  See the `chappg documentation`_ for details.

.. _chappg: https://ctan.org/pkg/chappg
.. _chappg documentation:
   https://mirrors.ctan.org/macros/latex/contrib/chappg/chappg.pdf


paper size
----------

Paper geometry can be changed using documentoptions_ or with the
`geometry`_ package.

`LaTeX code`_::

  \usepackage{geometry}
  \geometry{OPTIONLIST}

Default:
  a4paper

Some possibilities:

* a4paper, b3paper, letterpaper, executivepaper, legalpaper
* landscape, portrait, twoside.

Example:
  Choose A5 pager in landscape orientation with command line argument::

    --documentoptions=a5paper,landscape

  The same with LaTeX commands in the `style sheet`_::

    \usepackage{geometry}
    \geometry{a5paper,landscape}

  For details see the `geometry manual`_.

paragraph indent
----------------

Default (in most document classes):
  Indent the first line in a paragraph unless it is the first line of a
  chapter, section, subsection, or subsubsection.

Example 1:
  To set paragraph indentation to zero but add a vertical space between
  load the `parskip` package with the command line argument::

    --stylesheet=parskip

  or in a custom `style sheet`_ with::

    \usepackage{parskip}

Example 2:
  To suppress the indentation of a specific paragraph, you may give it the
  class "noindent" with, e.g. ::

    .. class:: noindent

    This paragraph should not be indented.

  and define the `custom role`_ command::

    \newcommand{\DUrolenoindent}[1]{\noindent #1}

rubric
------

A rubric__ is like an informal heading that doesn't correspond to the
document's structure.

Command:
  ``\DUrubric``

Default:
  subsubsection style (unnumbered), italic

Example1:
  Set centred and red::

    \newcommand*{\DUrubric}[1]{%
       \subsubsection*{\centerline{\color{red}#1}}}

.. note::
  Class attribute values are ignored because the "classes_ wrapper"
  interferes with LaTeX's formatting (spacing/indentation) of text following
  a section heading. Consider using a `topic element`_ or a container_.

__ ../ref/rst/directives.html#rubric
.. _container: ../ref/rst/directives.html#container

section headings
----------------

Settings: documentclass_, use-part-section_

Section headings are converted into LaTeX macros according to their level,
the document class and the value of the use-part-section_ setting:

=====  =============  ================== =============  ==============
Level  article        article with part  book [#]_      book with part
=====  =============  ================== =============  ==============
  1    section        part               chapter        part
  2    subsection     section            section        chapter
  3    subsubsection  subsection         subsection     section
  4    paragraph      subsubsection      subsubsection  subsection
  5    subparagraph   paragraph          paragraph      subsubsection
  6    DUtitle        subparagraph       subparagraph   paragraph
  7    DUtitle        DUtitle            DUtitle        subparagraph
=====  =============  ================== =============  ==============


.. [#] One of the document classes 'book', 'memoir', 'report 'scrbook',
       or 'scrreprt'.

.. _use-part-section: config.html#use-part-section

section numbering
-----------------

Sections are numbered if there is a `sectnum directive`_ in the document.

Setting: sectnum_xform_

If sectnum_xform_ is False, section numbers are generated by LaTeX. In this
case the "prefix" and "suffix" arguments of the `sectnum directive`_ are
ignored. The section number style is determined by the `document class`_
and can be configured in a LaTeX `style sheet`_, e.g.::

  \setcounter{secnumdepth}{5}

.. note:: The LaTeX name is 'secnumdepth' (without 't').

.. _sectnum directive: ../ref/rst/directives.html#sectnum
.. _sectnum_xform: config.html#sectnum-xform


sidebar
-------

Sidebars__ are like miniature, parallel documents that occur inside other
documents, providing related or reference material. They can be likened to
super-footnotes; their content is outside of the flow of the document's main
text.

Command:
  ``DUsidebar``

Default:
  Box with grey background.

Example:
  Use margin notes::

    \newcommand{\DUsidebar}[1]{\marginpar{\flushleft #1}}

  * Make sure the margin is wide enough to hold the note.
  * This fails with some constructs inside the `side bar` and where
    \marginpar cannot be used, e.g., inside floats, footnotes, or in frames
    made with the framed package (see marginnote_).

__ https://docutils.sourceforge.io/docutils/docs/ref/rst/directives.html#sidebar

size of a "px"
--------------

The `length unit`_ "px" is `defined in pdfTeX and LuaTeX`__, the "XeTeX"
writer uses the ``\pdfpxdimen`` macro as workaround.

Default:
  1 px = 1/72 in

Example:
  Set the value to match the `CSS3 pixel unit`_ 1 px = 1/96 in
  with the `LaTeX code`_::

    \pdfpxdimen=1in
    \divide\pdfpxdimen by 96

__ https://tex.stackexchange.com/questions/41370/
   what-are-the-possible-dimensions-sizes-units-latex-understands
.. _reference pixel: https://www.w3.org/TR/css-values-3/#reference-pixel

table style
------------

A pre-configured *table style* can be globally selected via the table_style_
setting or set for individual tables via a `class directive`_ or the class
option of the `table directive`_.

Supported values:

standard
  Borders around all cells.

booktabs
  A line above and below the table and one after the head.

borderless
  No borders around table cells.

colwidths-auto
  Column width determination by LaTeX.
  Overridden by the `table directive`_'s "widths" option.

  .. warning::

    ``colwidths-auto`` is only suited for tables with simple cell content.

    LaTeX puts the content of auto-sized columns on one line (merging
    paragraphs) and may fail with complex content.

.. eventually in future

   align-left, align-center, align-right
     Align tables.

By default, *column widths* are computed from the source column widths.
The `legacy_column_widths`_ setting selects the conversion algorithm.
Custom column widths can be set with the "widths" option of the `table
directive`_.

See also the section on problems with tables_ below.

.. _new_column_widths:
.. _legacy_column_widths: config.html#legacy-column-widths
.. _table_style: config.html#table-style-latex-writers
.. _"widths" option:
.. _table directive: ../ref/rst/directives.html#table


table of contents
-----------------

A `contents directive`_ is replaced by a table of contents (ToC).

Setting:: use-latex-toc_

With use-latex-toc (default since release 0.6):

* The ToC is generated by LaTeX (via the ``\tableofcontents`` command).

  The layout depends on the chosen document class and can be configured in
  a custom `style sheet`_ (see e.g. the `KOMA-Script Guide`_ for the
  `KOMA-script`_ classes).

* The depth of the ToC and PDF-bookmarks can be configured

  + with the "depth" argument of the `contents directive`_, or

  + in a style sheet with e.g. ``\setcounter{tocdepth}{5}``.

* Local ToCs are done with the minitoc_ package. See the `minitoc
  documentation`_ for the numerous configuration options.

.. note::
   Minitoc supports local ToCs only at "part" and top section level
   ("chapter" or "section"). Local `contents` directives at lower levels
   are ignored (a warning is issued).

   This is an intended feature of the minitoc_ package. If you really
   require local ToCs at lower level, turn off the use-latex-toc_ option.

.. _use-latex-toc: config.html#use-latex-toc
.. _contents directive: ../ref/rst/directives.html#table-of-contents
.. _minitoc: https://ctan.org/pkg/minitoc
.. _minitoc documentation:
   https://mirrors.ctan.org/macros/latex/contrib/minitoc/minitoc.pdf


title reference role
--------------------

`Title reference`_ is the default `default role`_ for `interpreted text`_.

Command:
  ``\DUroletitlereference``

Default:
  use slanted font (``\textsl``)

Example:
  set title references with a bold monospace font::

    \newcommand{\DUroletitlereference}[1]{\texttt{\textbf{#1}}}

.. _title reference: ../ref/rst/roles.html#title-reference
.. _default role:
   ../ref/rst/directives.html#setting-the-default-interpreted-text-role
.. _interpreted text: ../ref/rst/restructuredtext.html#interpreted-text


titles
------

The titles of admonitions_, sidebar_, and `topic element`_ use
the ``\DUtitle`` command which can be re-defined in the corresponding
DUCLASS environment.

* The re-definition is local to the environment, so you don't need to
  save/restore the original function.

* In the nested function redefinition, the argument placeholder requires
  two hashes, ``#1`` → ``##1``!

Example 1:
  a centered and somewhat larger title for topcis::

     \newcommand*{\DUCLASStopic}{
       \renewcommand*{\DUtitle}[1]{\subsection*{\centering ##1}}
     }

Example 2:
  a right-pointing hand as title for the "attention" admonition::

    \usepackage{pifont}
    \newcommand*{\DUCLASSattention}{
      \renewcommand*{\DUtitle}[1]{\ding{43}}
    }

  The title argument is "swallowed" by the command.
  To have both, hand and title use::

    \usepackage{pifont}
    \newcommand*{\DUCLASSattention}{
      \renewcommand*{\DUtitle}[1]{\ding{43} ##1}
    }


text encoding
-------------

The encoding of the LaTeX source file is Docutils' *output* encoding
but LaTeX' *input* encoding.

Setting: output_encoding_

Default:
  "utf-8" (LaTeX's default input encoding)

Example:
  Encode the LaTeX source file with the ISO `latin-1` (west european)
  8-bit encoding (the default in Docutils versions up to 0.6.)::

    --output-encoding=latin-1

Note:
  8-bit LaTeX comes with two packages for UTF-8 support,

  .. class:: field-indent-4em

  :utf8:  by the standard `inputenc`_ package with only limited coverage
          (mainly accented characters).

  :utf8x: supported by the `ucs`_ package covers a wider range of Unicode
          characters than does "utf8".  It is, however, a non-standard
          extension and no longer developed.

.. with utf8x:
   If LaTeX issues a Warning about unloaded/unknown characters adding ::

     \PreloadUnicodePage{n}

   (where *n* is the Unicode page-number) to the style sheet might help.

.. _output_encoding: config.html#output-encoding
.. _inputenc: https://ctan.org/pkg/inputenc
.. _ucs: https://ctan.org/pkg/unicode


topic element
-------------

A topic_ is like a block quote with a title, or a self-contained section
with no subsections. Topics and rubrics can be used at places where a
`section title`_ is not allowed (e.g. inside a directive).

Example:
  Use a standard paragraph for a topic::

    \newcommand{\DUCLASStopic}{%
      \renewenvironment{quote}{}{}%
    }

.. _topic: ../ref/rst/directives.html#topic
.. _section title: ../ref/rst/restructuredtext.html#sections


transition element
------------------

Transitions__ are commonly seen in novels and short fiction, as a gap
spanning one or more lines, marking text divisions or signaling changes in
subject, time, point of view, or emphasis.

Command:
  ``\DUtransition``

Default:
  A horizontal line, 1/3 of text width

Example 1:
  Use three stars::

    \newcommand*{\DUtransition}{\centering{}*\quad*\quad*}

  Alternatively use the more elaborated version in `transition-stars.sty`_.

Example 2:
  If paragraphs are separated by indentation, you can simply use a vertical
  space::

    \newcommand*{\DUtransition}{\vspace{2ex}}

__ https://docutils.sourceforge.io/docutils/docs/ref/rst/restructuredtext.html#transitions
.. _transition-stars.sty: ../../../sandbox/stylesheets/transition-stars.sty


Changes
=======

* The Docutils HISTORY_ lists all changes during the history of docutils.
  Important changes are summarized in the RELEASE-NOTES_.

.. _HISTORY: ../../HISTORY.html
.. _RELEASE-NOTES: ../../RELEASE-NOTES.html


Problems
========

Troubleshooting
---------------

Bad looking PDF output
``````````````````````

  What I am looking for when I try Docutils is PDF files of high quality.
  Unfortunately that never is the case.

  So am I just stupid or is there a way to get really high quality PDF
  from Docutils?

Make sure the default font is not a bitmap font.

There is `Latin Modern`_ if you like the look of the standard font on paper,
but want nice pdf. Or select something else like Times, Palatino, ... via
configuration settings_. See font_ and font-encoding_.


footnote mark and text at different pages
`````````````````````````````````````````

Docutils stores the footnote text in a separate node, at the position where
it is specified in the input document. With the default settings, the
footnote is put at the bottom of the page where the footnote text is located,
maybe far away from the footnote mark (see e.g. `<rst/demo.rst>`_).

To get footnote mark and text at the same page, keep footnote mark and
footnote text close together.


non-breaking hyperlinks
```````````````````````

If you convert with ``latex`` (as opposed to ``pdflatex``), hyperlinks will
not wrap and sometimes stick into the margin.

Wrong:
  ::

     \usepackage[breaklinks=true]{hyperref}

  "breaklinks" is an internal option that indicates whether the chosen
  driver can handle split links. (It might work to *disable* link breaking.)

Right:
  Use one of the following:

  a) compile with pdflatex_,

  b) use the package breakurl_,

  c) (for printout) `disable hyperlinks`_ using the package "nohyperref".

See also the `Link text doesn’t break at end line`_ LaTeX FAQ entry.

.. _breakurl: https://ctan.org/pkg/breakurl
.. _Link text doesn’t break at end line: https://texfaq.org/FAQ-breaklinks


Glyph not defined in PD1 encoding
`````````````````````````````````

If a section title or other link contains non-Latin (e.g. Cyrillic)
characters, the LaTeX log contains lots of warnings like::

  Package hyperref Warning: Glyph not defined in PD1 encoding,
  (hyperref)                removing `\CYRZ' on input line 6.
  ...

This can be solved with the "unicode" hyperref_option_ setting::

  --hyperref-option=unicode

(works also with non-unicode input/output encoding (e.g. "koi8r" or
"latin1"). Newer versions of hyperref default to "unicode=true".

.. _hyperref_option: config.html#stylesheet-latex-writers


image inclusion
```````````````

Images_ are included in LaTeX with the help of the `graphicx`_ package.
The supported graphic formats depend on the postprocessor:

* pdflatex_, lualatex_, and xelatex_ work with PNG, JPG, and PDF
  but *not EPS*.

* When compiling to DVI with the ``latex`` command, support depends on
  the viewer or post-processor:

  - dvips supports EPS but not other format,
  - dvipdfmx works with EPS and JPG (add 'dvipdfmx' to the documentoptions_
    or graphicx-option_ setting and 'bmpsize' to the stylesheet_ setting).

* SVG images are supported if the `"svg" package`_ is listed in the
  stylesheet_ setting.  Pass the ``--shell-escape`` option to the LaTeX
  compiler to enable image conversion on the fly by Inkscape_.
  (New in Docutils 0.22.)

* The Rubber_ wrapper can be used for automatic image conversion.

For details see grfguide.pdf_.

Docutils expects a URI-reference_ as pointer to the image ressource.
The LaTeX writer transforms it to a filesystem path.
By default, LaTeX does not accept spaces and more than one dot in the
filename. If using "traditional" filenames is not an option, loading the
grffile_ package may help.

.. _images: ../ref/rst/directives.html#images
.. _graphicx: https://ctan.org/pkg/graphicx
.. _graphicx-option: config.html#graphicx-option
.. _grfguide.pdf:
   https://mirrors.ctan.org/macros/latex/required/graphics/grfguide.pdf
.. _URI-reference: https://www.rfc-editor.org/rfc/rfc3986.html#section-4.1
.. _"svg" package: https://ctan.org/pkg/svg
.. _Inkscape: https://inkscape.org/
.. _grffile: https://ctan.org/pkg/grffile


Why are my images too big?
``````````````````````````

In LaTeX, the size of the *pixel unit* defaults to 1 px = 1/72 in
while the `CSS3 pixel unit`_ is defined as 1 px = 1/96 in.

This is why pixmap images without size specification or objects with a size
specified in ``px`` tend to come out too large in the PDF.

Solution:
  Specify the image size in fixed `length units`_ (``pt``, ``cm``, ``in``, …)
  or configure the `size of a "px"`_.


Error ``illegal unit px``
`````````````````````````

If you convert the LaTeX source with a legacy program, you might get this
error.

The unit "px" was introduced by the `pdfTeX` converter on 2005-02-04.
`pdfTeX` is used also for conversion into DVI format in all modern LaTeX
distributions (since ca. 2006).


Error ``Symbol \textcurrency not provided`` ...
```````````````````````````````````````````````

The currency sign (\\u00a4) is not supported by all fonts (some have
an Euro sign at its place). You might see an error like::

    ! Package textcomp Error: Symbol \textcurrency not provided by
    (textcomp)                font family ptm in TS1 encoding.
    (textcomp)                Default family used instead.

(which in case of font family "ptm" is a false positive).

Add either ``warn`` (turn the error in a warning, use the default (bitmap)
symbol), or ``force,almostfull`` (use the symbol provided by the font at
the users risk) to the document options or use a different font package.


Warning: language … not supported
`````````````````````````````````

The "latex" writer uses the LaTeX package Babel_ and the "xetex" writer
uses Polyglossia_ for language_ support (hyphenation rules, auto-text
localisations and typographic rules). Polyglossia_ supports more
languages, so switching to the "xetex" writer may help.

For short quotes or if language support is provided by the user via other
`LaTeX document classes and packages`_, the warning can be ignored.

.. _Babel: https://ctan.org/pkg/babel
.. _Polyglossia: https://ctan.org/pkg/polyglossia


Search and text extraction
``````````````````````````

Search for text that contains characters outside the ASCII range might
fail.  See font_ and `font encoding`_ (as well as `Searching PDF files`_
for background information).

It may help to load the `cmap` package (via `style sheets`_ or the custom
`LaTeX preamble`_ (see also `Proper use of cmap and mmmap`_).

.. _Searching PDF files: https://texfaq.org/FAQ-cpy-srchpdf
.. _Proper use of cmap and mmmap:
   https://tex.stackexchange.com/questions/64409/proper-use-of-cmap-and-mmap


Unicode box drawing and block characters
````````````````````````````````````````

The easiest solution is to use xelatex_ or lualatex_ which directly
support all Unicode characters (if included in the used font).

With "traditional" TeX engines (e.g. pdflatex_):

- Generate LaTeX code with the default `output_encoding`_ "utf-8".

- Add the pmboxdraw_ package to the `style sheets`_.
  (For shaded boxes also add the `color` package.)

Unfortunately, this defines only a subset of the characters
(see pmboxdraw.pdf_ for a list).

.. _pmboxdraw: https://ctan.org/pkg/pmboxdraw
.. _pmboxdraw.pdf:
   https://mirrors.ctan.org/macros/latex/contrib/pmboxdraw/pmboxdraw.pdf


Bugs and open issues
--------------------

Open to be fixed or open to discussion.

See also the entries in the `Docutils TODO list`_,
the BUGS_ documentation and the `SourceForge Bug Tracker`_.

.. _Docutils TODO list: ../dev/todo.html#latex-writer
.. _bugs: ../../BUGS.html
.. _SourceForge Bug Tracker: https://sourceforge.net/p/docutils/bugs/


Footnotes and citations
```````````````````````

Initially both were implemented using figure floats, because hyperlinking
back and forth seemed to be impossible. Later the `figure` directive was
added that puts images into figure floats.

This results in footnotes, citations, and figures possibly being mixed at
page foot.

Workaround:
  Select citation handling with the use_latex_citations_ setting.

If ``use-latex-citations`` is used, a bibliography is inserted right at
the end of the document. *This should be customizable*.

If ``use-latex-citations`` is used adjacent citation references (separated
only by a single space or a newline) are combined to a single citation
group, i.e. ``[cite1]_ [cite2]_`` results in ``\cite{cite1,cite2}``.
The appearance in the output can be configured in a `style sheet`_.

.. _use_latex_citations: config.html#use-latex-citations


Tables
``````

* Too wide tables (cf. `bug #422`_):

  Try the new_column_widths_ setting or use the `"widths" option`_ to
  manually set the table column widths.

* Table cells with both multirow and multicolumn are currently not possible.

.. _bug #422: https://sourceforge.net/p/docutils/bugs/422/


Figures
```````

* Figures are always as wide as the containing text. The "figwidth" argument
  is currently not supported. As a consequence, the "align" argument has no
  effect.

* Wrapping text around figures is currently not supported. (Requires the
  `wrapfig`_ package.)

.. _wrapfig: https://ctan.org/pkg/wrapfig


Miscellaneous
`````````````

* Pdfbookmark level 4 (and greater) does not work (might be settable but
  complicated).

* Hyperlinks are not hyphenated; this leads to bad spacing. See
  docs/user/rst/demo.rst 2.14 directives.
