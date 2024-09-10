.. include:: ../../header2.rst

=============================
 reStructuredText Directives
=============================
:Author: David Goodger
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

.. contents::
   :depth: 2

This document describes the directives implemented in the reference
reStructuredText parser.

Directives have the following syntax::

    +-------+-------------------------------+
    | ".. " | directive type "::" directive |
    +-------+ block                         |
            |                               |
            +-------------------------------+

Directives begin with an explicit markup start (two periods and a
space), followed by the directive type and two colons (collectively,
the "directive marker").  The directive block begins immediately after
the directive marker, and includes all subsequent indented lines.  The
directive block is divided into arguments, options (a field list), and
content (in that order), any of which may appear.  See the Directives_
section in the `reStructuredText Markup Specification`_ for syntax
details.

Descriptions below list "doctree elements" (document tree element
names; XML DTD generic identifiers) corresponding to individual
directives.  For details on the hierarchy of elements, please see `The
Docutils Document Tree`_ and the `Docutils Generic DTD`_ XML document
type definition.  For directive implementation details, see `Creating
reStructuredText Directives`_.

.. _Docutils Generic DTD: ../docutils.dtd
.. _Creating reStructuredText Directives:
   ../../howto/rst-directives.html


-------------
 Admonitions
-------------

Admonitions ("safety messages" or "hazard statements") can appear anywhere
an ordinary body element can.  They contain arbitrary body elements.
Typically, an admonition is rendered as an offset block in a document,
sometimes outlined or shaded.

Docutils defines a `generic admonition`_ as well as a set of
`specific admonitions`_.


.. _attention:
.. _caution:
.. _danger:
.. _error:
.. _hint:
.. _important:
.. _note:
.. _tip:
.. _warning:

Specific Admonitions
====================

.. class:: field-indent-13em

:Directive Types: "attention", "caution", "danger", "error", "hint",
                  "important", "note", "tip", "warning"
:Doctree Elements: `\<attention>`_, `\<caution>`_, `\<danger>`_,
                   `\<error>`_, `\<hint>`_, `\<important>`_,
                   `\<note>`_, `\<tip>`_, `\<warning>`_
:Directive Arguments: none
:Directive Options: `class <class option_>`_, name_
:Directive Content: Interpreted as body elements.

Specific admonitions are rendered with a title matching the admonition type.
For example::

    .. DANGER::
       Beware killer rabbits!

This directive might be rendered something like this::

    +------------------------+
    |        !DANGER!        |
    |                        |
    | Beware killer rabbits! |
    +------------------------+

Any text immediately following the directive indicator (on the same
line and/or indented on following lines) is interpreted as a directive
block and is parsed for normal body elements.  For example, the
following "note" admonition directive contains one paragraph and a
bullet list consisting of two list items::

    .. note:: This is a note admonition.
       This is the second line of the first paragraph.

       - The note contains all indented body elements
         following.
       - It includes this bullet list.


.. _admonition:

Generic Admonition
==================

.. class:: field-indent-13em

:Directive Type: "admonition"
:Doctree Elements: `\<admonition>`_, `\<title>`_
:Directive Arguments: one, required (admonition title)
:Directive Options: `class <class option_>`_, name_
:Directive Content: Interpreted as body elements.

This is a generic, titled admonition.  The title may be anything the
author desires.

The author-supplied title is also used as a `classes attribute`_ value
after `identifier normalization`_ and adding the prefix "admonition-".
For example, this admonition::

    .. admonition:: And, by the way...

       You can make up your own admonition too.

becomes the following document tree (pseudo-XML)::

    <document source="test data">
        <admonition classes="admonition-and-by-the-way">
            <title>
                And, by the way...
            <paragraph>
                You can make up your own admonition too.

The `class <class option_>`_ option overrides the generated
`classes attribute`_ value.


--------
 Images
--------

There are two directives to include images: image_ and figure_.

.. attention::

  Images are not supported by the `manpage`_ writer.

  It is up to the author to ensure compatibility of the image data format
  with the output format or user agent (LaTeX engine, `HTML browser`__).
  The following, non exhaustive table provides an overview.

.. _image formats:

=========== ====== ====== ===== ===== ===== ===== ===== ===== ===== =====
..          SVG    PDF    PNG   JPG   GIF   APNG  AVIF  WebM  MP4   OGG
=========== ====== ====== ===== ===== ===== ===== ===== ===== ===== =====
..          vector        raster                        video [#video]_
----------- ------------- ----------------------------- -----------------
HTML4_ [#]_ ✓             ✓     ✓     ✓     (✓)   (✓)   (✓)   (✓)   (✓)

HTML5_      ✓             ✓     ✓     ✓     ✓     ✓     ✓     ✓     ✓

LaTeX_ [#]_        ✓      ✓     ✓

ODT_        ✓      ✓      ✓     ✓     ✓
=========== ====== ====== ===== ===== ===== ===== ===== ===== ===== =====

.. [#video]
       The `html5 writer`_ uses the ``<video>`` tag if the image URI
       ends with an extension matching one of the listed video formats
       (since Docutils 0.17).

.. [#] The html4 writer uses an ``<object>`` tag for SVG images and
       videos for compatibility with older browsers and for XHTML1.1
       conformance respectively.

.. [#] When compiling with ``pdflatex``, ``xelatex``, or ``lualatex``.
       The original ``latex`` engine supports only the EPS image format.
       Some build systems, e.g. rubber_ support additional formats
       via on-the-fly image conversion.

__ https://developer.mozilla.org/en-US/docs/Web/Media/Formats/Image_types
.. _HTML4:
.. _html4 writer: ../../user/html.html#html4css1
.. _HTML5:
.. _html5 writer: ../../user/html.html#html5-polyglot
.. _LaTeX: ../../user/latex.html#image-inclusion
.. _ODT: ../../user/odt.html
.. _manpage: ../../user/manpage.html
.. _rubber: https://github.com/petrhosek/rubber


Image
=====

.. class:: field-indent-13em

:Directive Type: "image"
:Doctree Elements: `\<image>`_, `\<reference>`_ (only with option "target_")
:Directive Arguments: one, required (image URI_)
:Directive Options: `see below <image options_>`__
:Directive Content: none
:Configuration Setting: image_loading_ (only `HTML5 writer`_)

An "image" is a simple picture::

    .. image:: picture.png

A `URI reference`_ to the image source file is specified in the directive
argument.  As with hyperlink targets, the image URI may begin on the
same line as the explicit markup start and target name, or it may
begin in an indented text block immediately following, with no
intervening blank lines.  If there are multiple lines in the link
block, they are stripped of leading and trailing whitespace and joined
together.

Optionally, the image link block may contain a flat field list, the
`image options`_.  For example::

    .. image:: picture.jpeg
       :height: 100px
       :width: 200 px
       :scale: 50 %
       :loading: embed
       :alt: alternate text
       :align: right


*Inline images* can be defined with an "image" directive in a `substitution
definition`_, e.g. ::

    |Red light| means stop, |green light| means go.

    .. |red light|    image:: red_light.png
       :align: top
    .. |green light|  image:: green_light.png
       :align: bottom

.. _image options:

The "image" directive recognizes the common options `class <class option_>`_
and name_ as well as

``align`` : "top", "middle", "bottom", "left", "center", or "right"
    The alignment of the image, equivalent to the HTML ``<img>`` tag's
    deprecated "align" attribute or the corresponding "vertical-align" and
    "text-align" CSS properties.
    The values "top", "middle", and "bottom"
    control an image's vertical alignment (relative to the text
    baseline); they are only useful for inline images (substitutions).
    The values "left", "center", and "right" control an image's
    horizontal alignment, allowing the image to float and have the
    text flow around it.  The specific behaviour depends upon the
    browser or rendering software used.

``alt`` : text_
    Alternate text: a short description of the image, displayed by
    applications that cannot display images, or spoken by applications
    for visually impaired users.

``height`` : length_
    The desired height of the image.
    Used to reserve space or scale the image vertically.  When the ``scale``
    option is also specified, they are combined.  For example, a height of
    200px and a scale of 50 is equivalent to a height of 100px with no scale.

``loading`` : "embed", "link", or "lazy"
    Set the `loading attribute`_ to indicate the
    preferred handling by the Docutils Writer. [#]_

    :embed: Embed the image into the output document. [#]_
    :link:  Refer to the image via its URI.
    :lazy:  Refer to the image.  The HTML5 writer additionally
            specifies the "`lazy loading attribute`_".

    (New in Docutils 0.21.)

``scale`` : integer percentage (the "%" symbol is optional)
    The uniform scaling factor of the image.  The default is "100 %",
    i.e. no scaling.
    Docutils tries to determine dimensions from the image file
    if no ``height`` or ``width`` options are specified
    (requires the `Python Imaging Library`_).

    .. _target:

``target`` : URI_ or `reference name`_
    Nest the image in a hyperlink reference element (make it "clickable").
    The option argument may be a URI reference or a reference name
    with underscore suffix (e.g. ```a name`_``).

``width`` : length_ or percentage_ of the current line width
    The width of the image.
    Used to reserve space or scale the image horizontally.  As with ``height``
    above, when the ``scale`` option is also specified, they are combined.

.. [#] Currently only recognized by the `HTML5 writer`_
   (overriding the `image_loading`_ configuration setting).
   The ODF/ODT writer always embeds images in the
   ``*.odt`` document, XML and LaTeX writers link to the image.
   The behaviour may change for the ODT and XML writers but
   images cannot be embedded in a LaTeX source.

.. [#] SVG images are directly included, other images are base64_ encoded
   and included as a `data URI`_.

.. _lazy loading attribute: https://html.spec.whatwg.org/multipage/
    urls-and-fetching.html#lazy-loading-attributes
.. _base64: https://en.wikipedia.org/wiki/Base64
.. _data URI: https://en.wikipedia.org/wiki/Data_URI_scheme


Figure
======

.. class:: field-indent-13em

:Directive Type: "figure"
:Doctree Elements: `\<figure>`_, `\<image>`_,
                   `\<caption>`_, `\<legend>`_
:Directive Arguments: one, required (image URI_)
:Directive Options: `see below <figure options_>`__
:Directive Content: Interpreted as the figure caption and an optional
                    legend.

A "figure" consists of image_ data (including `image options`_), an optional
caption (a single paragraph), and an optional legend (arbitrary body
elements). For page-based output media, figures might float to a different
position if this helps the page layout.
::

    .. figure:: picture.png
       :scale: 50 %
       :alt: map to buried treasure

       This is the caption of the figure (a simple paragraph).

       The legend consists of all elements after the caption.  In this
       case, the legend consists of this paragraph and the following
       table:

       +-----------------------+-----------------------+
       | Symbol                | Meaning               |
       +=======================+=======================+
       | .. image:: tent.png   | Campground            |
       +-----------------------+-----------------------+
       | .. image:: waves.png  | Lake                  |
       +-----------------------+-----------------------+
       | .. image:: peak.png   | Mountain              |
       +-----------------------+-----------------------+

There must be blank lines before the caption paragraph and before the
legend.  To specify a legend without a caption, use an empty comment
("..") in place of the caption.

.. _figure options:

The "figure" directive supports all `options of the "image" directive
<image options_>`__. These options (except ``align``) are passed on
to the contained image.

``align`` : "left", "center", or "right"
    The horizontal alignment of the figure, allowing the image to
    float and have the text flow around it.  The specific behaviour
    depends upon the browser or rendering software used.

In addition, the following options are recognized:

``figwidth`` : "image", length_, or percentage_ of current line width
    The width of the figure.
    Limits the horizontal space used by the figure.
    A special value of "image" is allowed, in which case the
    included image's actual width is used (requires the `Python Imaging
    Library`_). If the image file is not found or the required software is
    unavailable, this option is ignored.

    Sets the `width attribute`_ of the <figure> doctree element.

    This option does not scale the included image; use the ``width``
    `image option <image options_>`__ for that. ::

        +---------------------------+
        |        figure             |
        |                           |
        |<------ figwidth --------->|
        |                           |
        |  +---------------------+  |
        |  |     image           |  |
        |  |                     |  |
        |  |<--- width --------->|  |
        |  +---------------------+  |
        |                           |
        |The figure's caption should|
        |wrap at this width.        |
        +---------------------------+

``figclass`` : space separated list of `class names`_
    Set a `classes attribute`_ value on the figure element.  See the
    `class directive`_ below.

.. _Python Imaging Library:
.. _Pillow: https://pypi.org/project/Pillow/


---------------
 Body Elements
---------------

Topic
=====

.. class:: field-indent-13em

:Directive Type: "topic"
:Doctree Element: `\<topic>`_
:Directive Arguments: one, required (topic title)
:Directive Options: `class <class option_>`_, name_
:Directive Content: Interpreted as the topic body.

A topic is like a block quote with a title, or a self-contained
section with no subsections.  Use the "topic" directive to indicate a
self-contained idea that is separate from the flow of the document.
Topics may occur anywhere a section or transition may occur.  Body
elements and topics may not contain nested topics.

The directive's sole argument is interpreted as the topic title; the
next line must be blank.  All subsequent lines make up the topic body,
interpreted as body elements.  For example::

    .. topic:: Topic Title

        Subsequent indented lines comprise
        the body of the topic, and are
        interpreted as body elements.


Sidebar
=======

.. class:: field-indent-13em

:Directive Type: "sidebar"
:Doctree Element: `\<sidebar>`_
:Directive Arguments: one, optional (sidebar title)
:Directive Options: `see below <sidebar options_>`__
:Directive Content: Interpreted as the sidebar body.

Sidebars are like miniature, parallel documents that occur inside
other documents, providing related or reference material.  A sidebar
is typically offset by a border and "floats" to the side of the page;
the document's main text may flow around it.  Sidebars can also be
likened to super-footnotes; their content is outside of the flow of
the document's main text.

Sidebars may occur anywhere a section or transition may occur.  Body
elements (including sidebars) may not contain nested sidebars.

The directive's sole argument is interpreted as the sidebar title,
which may be followed by a subtitle option (see below); the next line
must be blank.  All subsequent lines make up the sidebar body,
interpreted as body elements.  For example::

    .. sidebar:: Optional Sidebar Title
       :subtitle: Optional Sidebar Subtitle

       Subsequent indented lines comprise
       the body of the sidebar, and are
       interpreted as body elements.

.. _sidebar options:

Recognizes the common options `class <class option_>`_ and name_ as well as

``subtitle`` : text_
    The sidebar's subtitle.


Line Block
==========

.. admonition:: Deprecated

   The "line-block" directive is deprecated.  Use the `line block
   syntax`_ instead.

   .. _line block syntax: restructuredtext.html#line-blocks

.. class:: field-indent-13em

:Directive Type: "line-block"
:Doctree Element: `\<line_block>`_
:Directive Arguments: none
:Directive Options: `class <class option_>`_, name_
:Directive Content: Becomes the body of the line block.

The "line-block" directive constructs an element where line breaks and
initial indentation is significant and inline markup is supported.  It
is equivalent to a `parsed literal block`_ with different rendering:
typically in an ordinary serif typeface instead of a
typewriter/monospaced face, and not automatically indented.  (Have the
line-block directive begin a block quote to get an indented line
block.)  Line blocks are useful for address blocks and verse (poetry,
song lyrics), where the structure of lines is significant.  For
example, here's a classic::

    "To Ma Own Beloved Lassie: A Poem on her 17th Birthday", by
    Ewan McTeagle (for Lassie O'Shea):

        .. line-block::

            Lend us a couple of bob till Thursday.
            I'm absolutely skint.
            But I'm expecting a postal order and I can pay you back
                as soon as it comes.
            Love, Ewan.


.. _parsed-literal:

Parsed Literal Block
====================

.. class:: field-indent-13em

:Directive Type: "parsed-literal"
:Doctree Element: `\<literal_block>`_
:Directive Arguments: none
:Directive Options: `class <class option_>`_, name_
:Directive Content: Becomes the body of the literal block.

Unlike an ordinary literal block, the "parsed-literal" directive
constructs a literal block where the text is parsed for inline markup.
It is equivalent to a `line block`_ with different rendering:
typically in a typewriter/monospaced typeface, like an ordinary
literal block.  Parsed literal blocks are useful for adding hyperlinks
to code examples.

However, care must be taken with the text, because inline markup is
recognized and there is no protection from parsing.  Backslash-escapes
may be necessary to prevent unintended parsing.  And because the
markup characters are removed by the parser, care must also be taken
with vertical alignment.  Parsed "ASCII art" is tricky, and extra
whitespace may be necessary.

For example, all the element names in this content model are links::

    .. parsed-literal::

       ( (title_, subtitle_?)?,
         decoration_?,
         (docinfo_, transition_?)?,
         `%structure.model;`_ )


Code
====

.. class:: field-indent-13em

:Directive Type: "code"
:Doctree Elements: `\<literal_block>`_, `inline elements`_
:Directive Arguments: one, optional (formal language)
:Directive Options: `see below <code options_>`__
:Directive Content: Becomes the body of the literal block.
:Configuration Setting: syntax_highlight_

The "code" directive constructs a literal block. If the code language is
specified, the content is parsed by the Pygments_ syntax highlighter and
tokens are stored in nested `inline elements`_ with class arguments
according to their syntactic category. The actual highlighting requires
a custom style-sheet, see the `sandbox/stylesheets`_ for examples.

For example, the content of the following directive ::

    .. code:: python
       :number-lines:

       def my_function():
           "just a test"
           print(8/2)

is parsed and marked up as Python source code.

The parsing can be turned off with the syntax_highlight_ configuration
setting and command line option or by specifying the language as
`class <class option_>`_ option instead of directive argument.
This also avoids warnings when Pygments_ is not installed or the language
is not in the `supported languages and markup formats`_.

For code in external files, use the "include_" directive with the
``code`` option. For inline code, use the `"code" role`_.

.. _code options:

Recognizes the common options `class <class option_>`_ and name_ as well as

``number-lines`` : integer_ (start line number, optional)
    Precede every line with a line number.
    The optional argument is the number of the first line (default 1).

.. _sandbox/stylesheets: https://docutils.sourceforge.io/sandbox/stylesheets/
.. _Pygments: https://pygments.org/
.. _supported languages and markup formats: https://pygments.org/languages/


Math
====

.. class:: field-indent-13em

:Directive Type: "math"
:Doctree Element: `\<math_block>`_
:Directive Arguments: none
:Directive Options: `class <class option_>`_, name_
:Directive Content: Becomes the body of the math block.
                    (Content blocks separated by a blank line are put in
                    adjacent math blocks.)
:Configuration Setting: math_output_

The "math" directive inserts blocks with mathematical content
(display formulas, equations) into the document. The input format is
`LaTeX math syntax`_ with support for Unicode symbols, for example::

  .. math::

    α_t(i) = P(O_1, O_2, … O_t, q_t = S_i λ)

Support is limited to a subset of *LaTeX math* by the conversion
required for many output formats.  For HTML, the `math_output`_
configuration setting (or the corresponding ``--math-output``
command line option) select between alternative output formats with
different subsets of supported elements. If a writer does not
support math typesetting, the content is inserted verbatim.

For inline formulas, use the `"math" role`_.

.. _LaTeX math syntax: ../../ref/rst/mathematics.html


Rubric
======

.. class:: field-indent-13em

:Directive Type: "rubric"
:Doctree Element: `\<rubric>`_
:Directive Arguments: one, required (rubric text)
:Directive Options: `class <class option_>`_, name_
:Directive Content: none

..

     rubric n. 1. a title, heading, or the like, in a manuscript,
     book, statute, etc., written or printed in red or otherwise
     distinguished from the rest of the text. ...

     -- Random House Webster's College Dictionary, 1991

The "rubric" directive inserts a "rubric" element into the document
tree.  A rubric is like an informal heading that doesn't correspond to
the document's structure.


Epigraph
========

.. class:: field-indent-13em

:Directive Type: "epigraph"
:Doctree Element: `\<block_quote>`_
:Directive Arguments: none
:Directive Options: none
:Directive Content: Interpreted as the body of the block quote.

An epigraph is an apposite (suitable, apt, or pertinent) short
inscription, often a quotation or poem, at the beginning of a document
or section.

The "epigraph" directive produces an "epigraph"-class block quote.
For example, this input::

     .. epigraph::

        No matter where you go, there you are.

        -- Buckaroo Banzai

becomes this document tree fragment::

    <block_quote classes="epigraph">
        <paragraph>
            No matter where you go, there you are.
        <attribution>
            Buckaroo Banzai


Highlights
==========

.. class:: field-indent-13em

:Directive Type: "highlights"
:Doctree Element: `\<block_quote>`_
:Directive Arguments: none
:Directive Options: none
:Directive Content: Interpreted as the body of the block quote.

Highlights summarize the main points of a document or section, often
consisting of a list.

The "highlights" directive produces a "highlights"-class block quote.
See Epigraph_ above for an analogous example.


Pull-Quote
==========

.. class:: field-indent-13em

:Directive Type: "pull-quote"
:Doctree Element: `\<block_quote>`_
:Directive Arguments: none
:Directive Options: none
:Directive Content: Interpreted as the body of the block quote.

A pull-quote is a small selection of text "pulled out and quoted",
typically in a larger typeface.  Pull-quotes are used to attract
attention, especially in long articles.

The "pull-quote" directive produces a "pull-quote"-class block quote.
See Epigraph_ above for an analogous example.


.. compound:

Compound Paragraph
==================

.. class:: field-indent-13em

:Directive Type: "compound"
:Doctree Element: `\<compound>`_
:Directive Arguments: none
:Directive Options: `class <class option_>`_, name_
:Directive Content: Interpreted as body elements.

The "compound" directive is used to create a compound paragraph, which
is a single logical paragraph containing multiple physical body
elements such as simple paragraphs, literal blocks, tables, lists,
etc., instead of directly containing text and inline elements.  For
example::

    .. compound::

       The 'rm' command is very dangerous.  If you are logged
       in as root and enter ::

           cd /
           rm -rf *

       you will erase the entire contents of your file system.

In the example above, a literal block is "embedded" within a sentence
that begins in one physical paragraph and ends in another.

.. note::

   The "compound" directive is *not* a generic block-level container
   like HTML's ``<div>`` element.  Do not use it only to group a
   sequence of elements, or you may get unexpected results.

   If you need a generic block-level container, please use the
   container_ directive, described below.

Compound paragraphs are typically rendered as multiple distinct text
blocks, with the possibility of variations to emphasize their logical
unity:

* If paragraphs are rendered with a first-line indent, only the first
  physical paragraph of a compound paragraph should have that indent
  -- second and further physical paragraphs should omit the indents;
* vertical spacing between physical elements may be reduced;
* and so on.


Container
=========

.. class:: field-indent-13em

:Directive Type: "container"
:Doctree Element: `\<container>`_
:Directive Arguments: one or more, optional (`class names`_)
:Directive Options: name_
:Directive Content: Interpreted as body elements.

The "container" directive surrounds its contents (arbitrary body
elements) with a generic block-level "container" element.
Combined with the optional argument, this is an
extension mechanism for users & applications.  For example::

    .. container:: custom

       This paragraph might be rendered in a custom way.

Parsing the above results in the following pseudo-XML::

    <container classes="custom">
        <paragraph>
            This paragraph might be rendered in a custom way.

The "container" directive is the equivalent of HTML's ``<div>``
element.  It may be used to group a sequence of elements for user- or
application-specific purposes.


--------
 Tables
--------

Formal tables need more structure than the reStructuredText `table syntax`_
supplies.  Tables may be given titles with the "table_" directive.
Sometimes reStructuredText tables are inconvenient to write, or table
data in a standard format is readily available.  The "csv-table_"
directive supports CSV [#CSV]_ data.

.. _table syntax: restructuredtext.html#tables


Table
=====

.. class:: field-indent-13em

:Directive Type: "table"
:Doctree Element: `\<table>`_
:Directive Arguments: one, optional (table caption)
:Directive Options: `see below <table options_>`__
:Directive Content: A normal `reStructuredText table`_.
:Configuration Setting: table_style_

The "table" directive is used to provide a table caption
or specify options, e.g.::

    .. table:: Truth table for "not"
       :widths: auto

       =====  =====
         A    not A
       =====  =====
       False  True
       True   False
       =====  =====

.. _table options:

Recognizes the common options `class <class option_>`_ and name_ as well as

``align`` : "left", "center", or "right"
    The horizontal alignment of the table (new in Docutils 0.13).

``width`` : length_ or percentage_  of the current line width
    Sets the width of the table to the specified length or percentage
    of the line width.  If omitted, the renderer determines the width
    of the table based on its contents or the column ``widths``.

``widths`` : "auto", "grid", or a `list of integers`_
    Explicitly set column widths.
    Specifies relative widths if used with the ``width`` option.
    Possible values:

    .. class:: field-indent-4em run-in

    :auto: Delegate the determination of column widths to the backend
           (LaTeX, the HTML browser, ...).

    :grid: Determine column widths from the widths of the input columns
           (in characters).

    :list of integers: Must match the number of table columns.
           Used instead of the input column widths. Implies *"grid"*.

    The default depends on the writer. Most writers default to *grid*. [#]_

    .. [#] The `html5 writer`_ defaults to *auto*.
       The default for the HTML and LaTeX writers can be configured
       with the `table_style`_ configuration setting or the special class
       values "colwidths-auto"/"colwidths-grid").


.. _csv-table:

CSV Table
=========

.. class:: field-indent-13em

:Directive Type: "csv-table"
:Doctree Element: `\<table>`_
:Directive Arguments: one, optional (table caption)
:Directive Options: `see below <csv-table options_>`__
:Directive Content: A CSV (comma-separated values) table
                    or (with the `file`_ or `url`_ options) none.
:Configuration Settings: table_style_, file_insertion_enabled_

.. WARNING::

   The "csv-table" directive's `file`_ and `url`_ options represent
   potential security holes.  They can be disabled with the
   "file_insertion_enabled_" runtime setting.

The "csv-table" directive is used to create a table from CSV
(comma-separated values) [#CSV]_ data. The data may be internal
(an integral part of the document) or external (a separate file).

* Block markup and inline markup within cells is supported.
  Line ends are recognized within quoted cells.

* There is no support for checking that the number of columns in each
  row is the same. The directive automatically adds empty entries at
  the end of short rows.

  .. TODO
     Add option ``missing-cells`` with keywords "strict", "fill", "span"?
     (cf. [feature-requests:#103])

Example::

    .. csv-table:: Frozen Delights!
       :header: "Treat", "Quantity", "Description"
       :widths: 15, 10, 30

       "Albatross", 2.99, "On a stick!"
       "Crunchy Frog", 1.49, "If we took the bones out,
       it wouldn't be crunchy, now would it?"
       "Gannet Ripple", 1.99, "On a stick!"

.. _csv-table options:

Recognizes the common options `class <class option_>`_ and name_ as well as

``align`` : "left", "center", or "right"
    The horizontal alignment of the table. (New in Docutils 0.13)

    .. _delimiter:

``delim`` : character_, "tab", or "space"
    The character used to separate data fields.
    The special values "tab" and "space" are converted to the respective
    whitespace characters. [#tab-expansion]_
    Defaults to "``,``" (comma).

``encoding`` : encoding_
    The text encoding of the external CSV data (file or URL).
    Defaults to the document's `input_encoding`_.

``escape`` : character_
    A character used to escape the delimiter_ or quote_ characters from
    the CSV parser. The default is no escape character -- fields may
    contain delimiter or newline characters if they are quoted, two quote_
    characters stand for a literal one, e.g., ``"""Hi!"", he said."``.

    .. Caution:: Setting ``escape`` to ``\`` (backslash) interferes with
       the reStructuredText `escaping mechanism`_ (applied after CSV
       parsing). You will need two backslashes to escape reStructuredText
       markup and four backslashes for a literal one.

    .. _`file`:

``file`` : path_
    The local filesystem path to a CSV data file.

``header`` : text_ (CSV data)
    Supplemental data for the table header, added independently of and
    before any ``header-rows`` from the main CSV data.  Must use the
    same CSV format as the main CSV data. [#]_

``header-rows`` : integer_
    The number of rows of CSV data to use in the table header.
    Defaults to 0.

``keepspace`` : flag_
    Treat whitespace immediately following the delimiter as
    significant.  The default is to ignore such whitespace.

    .. _quote:

``quote`` : character_
    The character used to quote fields containing special characters,
    such as the delimiter_, quote, or new-line characters.
    Defaults to ``"`` (quote).

``stub-columns`` : integer_
    The number of table columns to use as stubs (row titles, on the left).
    Defaults to 0.

    .. _`url`:

``url`` : URI_
    A URI reference to a CSV data file.

``width`` : length_ or percentage_ of the current line width
    Sets the width of the table to the specified length or percentage
    of the line width.  If omitted, the renderer determines the width
    of the table based on its contents or the column ``widths``.

``widths`` : `list of integers`_ or "auto"
    A list of relative column widths.
    The default is equal-width columns (100%/#columns).

    "auto" delegates the determination of column widths to the backend
    (LaTeX, the HTML browser, ...).

.. [#CSV] CSV (comma separated values) is a common data format generated
   by spreadsheet applications and commercial databases. Despite the
   "comma" in its name, the field delimiter_ may be any Unicode character.

.. [#tab-expansion] Note, that tabs can be used as separator only in
   external files because hard tabs in the directive content are
   `converted to spaces`__ before it reaches the CVS reader.

   __ restructuredtext.html#whitespace

.. [#] Before Docutils 0.21, the header option used a hard-coded
   CSV dialect with the backslash as escape character.


List Table
==========

.. class:: field-indent-13em

:Directive Type: "list-table"
:Doctree Element: `\<table>`_
:Directive Arguments: one, optional (table caption)
:Directive Options: `see below <list-table options_>`__
:Directive Content: A uniform two-level bullet list.
:Configuration Setting: table_style_

(This is an initial implementation; `further ideas`__ may be implemented
in the future.)

__ ../../dev/rst/alternatives.html#list-driven-tables

The "list-table" directive is used to create a table from data in a
uniform two-level bullet list.  "Uniform" means that each sublist
(second-level list) must contain the same number of list items.

Example::

    .. list-table:: Frozen Delights!
       :widths: 15 10 30
       :header-rows: 1

       * - Treat
         - Quantity
         - Description
       * - Albatross
         - 2.99
         - On a stick!
       * - Crunchy Frog
         - 1.49
         - If we took the bones out, it wouldn't be
           crunchy, now would it?
       * - Gannet Ripple
         - 1.99
         - On a stick!

.. _list-table options:

Recognizes the common options `class <class option_>`_ and name_ as well as

``align`` : "left", "center", or "right"
    The horizontal alignment of the table.
    (New in Docutils 0.13)

``header-rows`` : integer_
    The number of rows of list data to use in the table header.
    Defaults to 0.

``stub-columns`` : integer_
    The number of table columns to use as stubs (row titles, on the
    left).  Defaults to 0.

    .. _table width:

``width`` : length_ or percentage_ of the current line width
    Sets the width of the table to the specified length or percentage
    of the line width.  If omitted, the renderer determines the width
    of the table based on its contents or the column ``widths``.

``widths`` : `list of integers`_ or "auto"
    A list of relative column widths.
    The default is equal-width columns (100%/#columns).

    "auto" delegates the determination of column widths to the backend
    (LaTeX, the HTML browser, ...).

  .. TODO
     Add option ``missing-cells`` with keywords "strict", "fill", "span"?
     (cf. [feature-requests:#103])


----------------
 Document Parts
----------------

.. A ``_contents:`` hyperlink here became id "contents-1"
   (name clash with the generated ToC)

Table of Contents
=================

.. class:: field-indent-13em

:Directive Type: "contents"
:Doctree Elements: `\<pending>`_, `\<topic>`_
:Directive Arguments: one, optional: title
:Directive Options: `see below <contents options_>`__
:Directive Content: none
:Configuration Settings: toc_backlinks_, use_latex_toc_, generate_oowriter_toc_

The "contents" directive generates a table of contents (TOC) in
a `\<topic>`_ element.  Topics, and therefore tables of contents,
may occur anywhere a section or transition may occur.
Body elements and topics may not contain tables of contents.

Here's the directive in its simplest form::

    .. contents::

Language-dependent boilerplate text will be used for the title.  The
English default title text is "Contents".

An explicit title may be specified::

    .. contents:: Table of Contents

The title may span lines, although it is not recommended::

    .. contents:: Here's a very long Table of
       Contents title

Directive options may be specified using a field list::

    .. contents:: Table of Contents
       :depth: 2

If the default title is to be used, the options field list may begin
on the same line as the directive marker::

    .. contents:: :depth: 2

.. _contents options:

The "contents" directive recognizes the common option
`class <class option_>`_ as well as

``backlinks`` : "entry" or "top" or "none"
    Generate links from section headers back to the table of contents
    entries, the table of contents itself, or generate no back-links.

``depth`` : integer_
    The number of section levels that are collected in the table of
    contents.  The default is unlimited depth.

``local`` : flag_
    Generate a local table of contents.  Entries will only include
    subsections of the section in which the directive is given.  If no
    explicit title is given, the table of contents will not be titled.


.. _sectnum:

Automatic Section Numbering
===========================

.. class:: field-indent-13em

:Directive Type: "sectnum" or "section-numbering" (synonyms)
:Doctree Elements: `\<pending>`_, `\<generated>`_
:Directive Arguments: none
:Directive Options: `see below <sectnum options_>`__
:Directive Content: none
:Configuration Setting: sectnum_xform_

The "sectnum" (or "section-numbering") directive automatically numbers
sections and subsections in a document (if not disabled by the
``--no-section-numbering`` command line option or the `sectnum_xform`_
configuration setting).

Section numbers are of the "multiple enumeration" form, where each
level has a number, separated by periods.  For example, the title of section
1, subsection 2, subsubsection 3 would have "1.2.3" prefixed.

The directive does its work in two passes: the initial parse
and a transform.  During the initial parse, a <pending> element is
generated which acts as a placeholder, storing any options internally.
At a later stage in the processing, the <pending> element triggers a
transform, which adds section numbers to titles.  Section numbers are
enclosed in a <generated> element, and titles have their `auto attribute`_
set to "1".

.. _sectnum options:

The "sectnum" directive recognizes the following options:

``depth`` : integer_
    The number of section levels that are numbered by this directive.
    The default is unlimited depth.

``prefix`` : text_
    An arbitrary string that is prefixed to the automatically
    generated section numbers.  It may be something like "3.2.", which
    will produce "3.2.1", "3.2.2", "3.2.2.1", and so on.  Note that
    any separating punctuation (in the example, a period, ".") must be
    explicitly provided.  The default is no prefix.

``suffix`` : text_
    An arbitrary string that is appended to the automatically
    generated section numbers.  The default is no suffix.

``start`` : integer_
    The value that will be used for the first section number.
    Combined with ``prefix``, this may be used to force the right
    numbering for a document split over several source files.  The
    default is 1.


.. _header:
.. _footer:

Document Header & Footer
========================

:Directive Types: "header" and "footer"
:Doctree Elements: `\<decoration>`_, `\<header>`_, `\<footer>`_
:Directive Arguments: none
:Directive Options: none
:Directive Content: Interpreted as body elements.

The "header" and "footer" directives create document decorations,
useful for page navigation, notes, time/datestamp, etc.  For example::

    .. header:: This space for rent.

This will add a paragraph to the document header, which will appear at
the top of the generated web page or at the top of every printed page.

These directives may be used multiple times, cumulatively.  There is
currently support for only one header and footer.

.. note::

   While it is possible to use the "header" and "footer" directives to
   create navigational elements for web pages, you should be aware
   that Docutils is meant to be used for *document* processing, and
   that a navigation bar is not typically part of a document.

   Thus, you may soon find Docutils' abilities to be insufficient for
   these purposes.  At that time, you should consider using a
   documentation generator like Sphinx_ rather than the "header" and
   "footer" directives.

   .. _Sphinx: http://sphinx-doc.org/

In addition to the use of these directives to populate header and
footer content, content may also be added automatically by the
processing system.  For example, if certain runtime settings are
enabled, the document footer is populated with processing information
such as a datestamp, a link to `the Docutils website`_, etc.

.. _the Docutils website: https://docutils.sourceforge.io


------------
 References
------------

.. _target-notes:

Target Footnotes
================

.. class:: field-indent-13em

:Directive Type: "target-notes"
:Doctree Elements: `\<pending>`_, `\<footnote>`_, `\<footnote_reference>`_
:Directive Arguments: none
:Directive Options: `class <class option_>`_, name_
:Directive Content: none

The "target-notes" directive creates a footnote for each external
target in the text, and corresponding footnote references after each
reference.  For every explicit target (of the form, ``.. _target name:
URL``) in the text, a footnote will be generated containing the
visible URL as content.


Footnotes
=========

**NOT IMPLEMENTED YET**

.. class:: field-indent-13em

:Directive Type: "footnotes"
:Doctree Elements: `\<pending>`_, `\<topic>`_
:Directive Arguments: none?
:Directive Options: Possible?
:Directive Content: none

@@@


Citations
=========

**NOT IMPLEMENTED YET**

.. class:: field-indent-13em

:Directive Type: "citations"
:Doctree Elements: `\<pending>`_, `\<topic>`_
:Directive Arguments: none?
:Directive Options: Possible?
:Directive Content: none

@@@

.. ---------------
    HTML-Specific
   ---------------

   Imagemap
   ========

   **NOT IMPLEMENTED YET**

   Non-standard element: imagemap.


-----------------------------------------
 Directives for Substitution Definitions
-----------------------------------------

The directives introduced in this section may only be used in
`substitution definitions`_.  They may not be used directly,
in standalone context.

.. _substitution definitions:
.. _substitution definition: restructuredtext.html#substitution-definitions


Inline Images
=============

The `image`_ directive can be used both, stand-alone (to define
block-level images) and in substitution definitions to define
inline images.


.. _replace:

Replacement Text
================

.. class:: field-indent-13em

:Directive Type: "replace"
:Doctree Element: Text & `inline elements`_
:Directive Arguments: none
:Directive Options: none
:Directive Content: A single paragraph; may contain inline markup.

The "replace" directive is used to indicate replacement text for a
substitution reference.  It may be used within `substitution
definitions`_ only.  For example, this directive can be used to expand
abbreviations::

    .. |reST| replace:: reStructuredText

    Yes, |reST| is a long word, so I can't blame anyone for wanting to
    abbreviate it.

.. _hyperlink workaround:

As reStructuredText doesn't support nested inline markup, the only way
to create a reference with styled text is to use substitutions with
the "replace" directive::

    I recommend you try |Python|_.

    .. |Python| replace:: Python, *the* best language around
    .. _Python: https://www.python.org/


.. _unicode:

Unicode Character Codes
=======================

.. class:: field-indent-13em

:Directive Type: "unicode"
:Doctree Element: Text
:Directive Arguments: one or more, required (Unicode character codes,
                      optional text, and comments)
:Directive Options: `see below <unicode options_>`__
:Directive Content: none

The "unicode" directive converts Unicode character codes (numerical
values) to characters, and may be used in `substitution definitions`_
only.

The arguments, separated by spaces, can be:

.. _character code:

* **character codes** as

  - decimal numbers or

  - hexadecimal numbers, prefixed by ``0x``, ``x``, ``\x``, ``U+``,
    ``u``, or ``\u`` or as XML-style hexadecimal character entities,
    e.g. ``&#x1a2b;``

* **text**, which is used as-is.

Text following " .. " is a comment and is ignored.  The spaces between
the arguments are ignored and thus do not appear in the output.
Hexadecimal codes are case-insensitive.

For example, the following text::

    Copyright |copy| 2003, |BogusMegaCorp (TM)| |---|
    all rights reserved.

    .. |copy| unicode:: 0xA9 .. copyright sign
    .. |BogusMegaCorp (TM)| unicode:: BogusMegaCorp U+2122
       .. with trademark sign
    .. |---| unicode:: U+02014 .. em dash
       :trim:

results in:

    Copyright |copy| 2003, |BogusMegaCorp (TM)| |---|
    all rights reserved.

    .. |copy| unicode:: 0xA9 .. copyright sign
    .. |BogusMegaCorp (TM)| unicode:: BogusMegaCorp U+2122
       .. with trademark sign
    .. |---| unicode:: U+02014 .. em dash
       :trim:

Docutils comes with a set of character substitution
definitions in the `reStructuredText Standard Definition Files`_.

.. _unicode options:

The "unicode" directive recognizes the following options:

``ltrim`` : flag_
    Whitespace to the left of the substitution reference is removed.

``rtrim`` : flag_
    Whitespace to the right of the substitution reference is removed.

``trim`` : flag_
    Equivalent to ``ltrim`` plus ``rtrim``; whitespace on both sides
    of the substitution reference is removed.


Date
====

.. class:: field-indent-13em

:Directive Type: "date"
:Doctree Element: Text
:Directive Arguments: one, optional (date format)
:Directive Options: none
:Directive Content: none

The "date" directive generates the current local date and inserts it
into the document as text.  This directive may be used in substitution
definitions only.

The optional directive content is interpreted as the desired date
format, using the same codes as Python's `time.strftime()`__ function.  The
default format is "%Y-%m-%d" (ISO 8601 date), but time fields can also
be used.  Examples::

    .. |date| date::
    .. |time| date:: %H:%M

    Today's date is |date|.

    This document was generated on |date| at |time|.

__ https://docs.python.org/3/library/time.html#time.strftime


---------------
 Miscellaneous
---------------

.. _include:

Including an External Document Fragment
=======================================

.. class:: field-indent-13em

:Directive Type: "include"
:Doctree Elements: Depend on data being included;
                   `\<literal_block>`_ with ``code`` or ``literal`` option.
:Directive Arguments: one, required (path_ to the file to include)
:Directive Options: `see below <include options_>`__
:Directive Content: none
:Configuration Setting: file_insertion_enabled_

.. WARNING::

   The "include" directive represents a potential security hole.  It
   can be disabled with the "file_insertion_enabled_" runtime setting.

The "include" directive reads a text file. The directive argument is
the path to the file to be included, relative to the document containing
the directive. Unless the options ``literal``, ``code``, or ``parser``
are given, the file is parsed in the current document's context at the
point of the directive. For example::

    This first example will be parsed at the document level, and can
    thus contain any construct, including section headers.

    .. include:: inclusion.rst

    Back in the main document.

        This second example will be parsed in a block quote context.
        Therefore it may only contain body elements.  It may not
        contain section headers.

        .. include:: inclusion.rst

If an included document fragment contains section structure, the title
adornments must match those of the master document.

Standard data files intended for inclusion in reStructuredText
documents are distributed with the Docutils source code, located in
the "docutils" package in the ``docutils/parsers/rst/include``
directory.  To access these files, use the special syntax for standard
"include" data files, angle brackets around the file name::

    .. include:: <isonum.txt>

The current set of standard "include" data files consists of sets of
substitution definitions.  See `reStructuredText Standard Definition
Files`_ for details.

.. _include options:

The "include" directive recognizes the following options:

``code`` : text_ (formal language, optional)
    The argument and the included content are passed to
    the code_ directive (useful for program listings).

``encoding`` : encoding_
    The text encoding of the external file.
    Defaults to the document's input_encoding_.

``end-before`` : text_
    Only the content before the first occurrence of the specified *text*
    in the external data file (but after any ``start-after`` text)
    will be included.

``end-line`` : integer_
    Only the content up to (but excluding) this line will be included.

``literal`` : flag_
    The entire included text is inserted into the document as a single
    literal block.

``number-lines`` : integer_ (start line number, optional)
    Precede every included line with a line number.
    The optional argument is the number of the first line (default 1).
    Works only with ``code`` or ``literal``.

``parser`` : text_ (parser name)
    Parse the included content with the specified parser.
    See the `"parser" configuration setting`_ for available parsers.

    (New in Docutils 0.17)

``start-after`` : text_
    Only the content after the first occurrence of the specified *text*
    in the external data file will be included.

``start-line`` : integer_
    Only the content starting from this line will be included.
    (As usual in Python, the first line has index 0 and negative values
    count from the end.)

``tab-width`` : integer_
    Number of spaces for hard tab expansion.
    Must be a positive integer, except for literal inclusions and code,
    where a negative value prevents expansion of hard tabs.
    Defaults to the tab_width_ configuration setting.

With ``code`` or ``literal`` the common options `class <class option_>`_ and name_
are recognized as well.

Combining ``start-line``/``end-line`` and ``start-after``/``end-before``
is possible.  The text markers will be searched in the specified lines
(further limiting the included content).


.. _raw:

Raw Data Pass-Through
=====================

.. class:: field-indent-13em

:Directive Type: "raw"
:Doctree Element: `\<raw>`_
:Directive Arguments: one or more, required (output format types)
:Directive Options: `see below <raw options_>`__
:Directive Content: Stored verbatim, uninterpreted.
                    None (empty) if a ``file`` or ``url`` option given.
:Configuration Settings: raw_enabled_, file_insertion_enabled_

.. WARNING::

   The "raw" directive represents a potential security hole.  It can
   be disabled with the "raw_enabled_" runtime setting.
   Insertion of external files can be disabled with the
   "file_insertion_enabled_" runtime setting.

.. Caution::

   The "raw" directive is a stop-gap measure allowing the author to
   bypass reStructuredText's markup.  It is a "power-user" feature
   that should not be overused or abused.  The use of "raw" ties
   documents to specific output formats and makes them less portable.

   If you often need to use the "raw" directive or a "raw"-derived
   interpreted text role, that is a sign either of overuse/abuse or
   that functionality may be missing from reStructuredText.  Please
   describe your situation in a message to the Docutils-users_ mailing
   list.

.. _Docutils-users: ../../user/mailing-lists.html#docutils-users

The "raw" directive indicates non-reStructuredText data that is to be
passed untouched to the Writer.  The names of the output formats are
given in the directive arguments.  The interpretation of the raw data
is up to the Writer.  A Writer may ignore any raw output not matching
its format.

For example, the following input would be passed untouched by an HTML
writer::

    .. raw:: html

       <hr width=50 size=10>

A LaTeX Writer could insert the following raw content into its
output stream::

    .. raw:: latex

       \setlength{\parindent}{0pt}

Raw data can also be read from an external file, specified in the
``file`` or ``url`` directive option.
In this case, the content block must be empty.
For example::

    .. raw:: html
       :file: inclusion.html

Inline equivalents of the "raw" directive can be defined via
`custom interpreted text roles`_ derived from the `"raw" role`_.

.. _raw options:

The "raw" directive recognizes the common option `class <class option_>`_
as well as

``encoding`` : encoding_
    The text encoding of the external raw data (with ``file`` or ``url``).
    Defaults to the main document's `input_encoding`_.

``file`` : path_
    The local filesystem path of a raw data file to be included.

``url`` : URI_
    A URI reference to a raw data file to be included.


.. _class directive:

Class
=====

.. class:: field-indent-13em

:Directive Type: "class"
:Doctree Element: `\<pending>`_
:Directive Arguments: one or more, required
                      (class names / attribute values)
:Directive Options: none
:Directive Content: Optional.  If present, it is interpreted as body
                    elements.

The "class" directive sets the `classes attribute`_ value on its content
or on the first immediately following [#]_ non-comment element [#]_.
The directive argument consists of one or more space-separated class
names. The names are transformed to conform to the regular expression
``[a-z](-?[a-z0-9]+)*`` (see `Identifier Normalization`_ below).

.. tip:: For reStructuredText directives, it is recommended to use the
   `class option`_ option instead of wrapping them in a "class" directive.

Examples::

    .. class:: special

    This is a "special" paragraph.

    .. class:: exceptional remarkable

    An Exceptional Section
    ======================

    This is an ordinary paragraph.

    .. class:: multiple

       First paragraph.

       Second paragraph.

The text above is parsed and transformed into this doctree fragment::

    <paragraph classes="special">
        This is a "special" paragraph.
    <section classes="exceptional remarkable">
        <title>
            An Exceptional Section
        <paragraph>
            This is an ordinary paragraph.
        <paragraph classes="multiple">
            First paragraph.
        <paragraph classes="multiple">
            Second paragraph.


.. [#] This is also true, if the class directive is "nested" at the end of
   an indented text block, for example::

       .. note:: the class values set in this directive-block do not apply to
          the note but the next paragraph.

          .. class:: special

       This is a paragraph with class value "special".

   This allows the "classification" of individual list items (except the
   first, as a preceding class directive applies to the list as a whole)::

       * bullet list

         .. class:: classy item

       * second item, with class argument

.. [#] To set a `classes attribute`_ value on a block quote, the
   "class" directive must be followed by an empty comment::

       .. class:: highlights
       ..

           Block quote text.

   Without the empty comment, the indented text would be interpreted as the
   "class" directive's content, and the classes would be applied to each
   element (paragraph, in this case) individually, instead of to the block
   quote as a whole.


Identifier Normalization
~~~~~~~~~~~~~~~~~~~~~~~~

Docutils normalizes `class names`_ and `identifiers`_ to conform
to the regular expression "``[a-z](-?[a-z0-9]+)*``" by converting

* alphabetic characters to lowercase,
* accented characters to the base character,
* non-alphanumeric characters to hyphens,
* consecutive hyphens into one hyphen

and stripping

* leading hyphens and number characters, and
* trailing hyphens.

For example ``"Rot.Gelb&Grün:+2008"`` becomes ``"rot-gelb-grun-2008"`` and
``"1000_Steps!"`` becomes ``"steps"``.

.. topic:: Rationale:

    Identifier keys must be valid in all supported output formats.

    For HTML 4.1 + CSS1 compatibility, identifiers should have no
    underscores, colons, or periods.  Hyphens may be used.

    - The `HTML 4.01 spec`_ defines identifiers based on SGML tokens:

          ID and NAME tokens must begin with a letter ([A-Za-z]) and
          may be followed by any number of letters, digits ([0-9]),
          hyphens ("-"), underscores ("_"), colons (":"), and periods
          (".").

          -- https://www.w3.org/TR/html401/types.html#type-name

    - The `CSS1 spec`_ defines identifiers based on the "name" token
      ("flex" tokenizer notation below)::

          unicode     \\[0-9a-f]{1,4}
          latin1      [¡-ÿ]
          escape      {unicode}|\\[ -~¡-ÿ]
          nmchar      [-a-z0-9]|{latin1}|{escape}
          name        {nmchar}+

    The CSS1 rule requires underscores ("_"), colons (":"), and
    periods (".") to be escaped [#]_,
    therefore `classes`_ and `ids attribute`_\ s should not
    contain these characters.  Combined with HTML4.1 requirements (the
    first character must be a letter; no "unicode", "latin1", or
    "escape" characters), this results in the regular expression
    ``[A-Za-z][-A-Za-z0-9]*``. Docutils adds a normalization by
    downcasing and merge of consecutive hyphens.

    .. [#] CSS identifiers may use underscores ("_") directly in
       `CSS Level 1`__, `CSS2.1`__, CSS2.2__, and CSS3__.

       __ https://www.w3.org/TR/CSS21/syndata.html#value-def-identifier
       __ https://www.w3.org/TR/CSS/#css-level-1
       __ https://www.w3.org/TR/CSS22/syndata.html
       __ https://www.w3.org/TR/css-syntax-3/#typedef-ident-token

    .. _HTML 4.01 spec: https://www.w3.org/TR/html401/
    .. _CSS1 spec: https://www.w3.org/TR/REC-CSS1


.. _role:

Custom Interpreted Text Roles
=============================

.. class:: field-indent-13em

:Directive Type: "role"
:Doctree Element: none; affects subsequent parsing
:Directive Arguments: two; one required (new `role name`_), one optional
                      (base role name, in parentheses)
:Directive Options: `see below <role options_>`__
:Directive Content: depends on base role.

The "role" directive dynamically creates a custom `interpreted text
role`_ and registers it with the parser.  This means that after
declaring a role like this::

    .. role:: custom

the document may use the new "custom" role::

    An example of using :custom:`interpreted text`

This will be parsed into the following document tree fragment::

    <paragraph>
        An example of using
        <inline classes="custom">
            interpreted text

.. _role name:

*Role names* are case insensitive and must conform to the rules of
simple `reference names`_ (but do not share a namespace with
hyperlinks, footnotes, and citations).

The new role may be based on an existing role, specified as a second
argument in parentheses (whitespace optional)::

    .. role:: custom(emphasis)

    :custom:`text`

The parsed result is as follows::

    <paragraph>
        <emphasis classes="custom">
            text

A special case is the `"raw" role`_: derived roles enable
inline `raw data pass-through`_, e.g.::

   .. role:: raw-role(raw)
      :format: html latex

   :raw-role:`raw text`

If no base role is explicitly specified, a generic custom role is
automatically used.  Subsequent interpreted text will produce an
`\<inline>`_ element with a `classes attribute`_, as in the first
example above.

.. _role options:

Depending on the base role, the following options may be recognized by the
"role" directive:

.. _role directive class option:

class : space separated list of `class names`_
    Set the `classes attribute`_ value on the element produced
    when the custom interpreted text role is used.
    Default value is the directive argument (role name).

    For example ::

      .. role:: custom
         :class: special

      :custom:`interpreted text`

    is parsed as ::

      <paragraph>
          <inline classes="special">
              interpreted text

    The "class" option is recognized with all interpreted text roles.

_`format` : space-separated list of output format names (`writer names`_)
    Specify the generated <raw> element's `format attribute`_.

    Only recognized with the `"raw" <"raw" role_>`__ base role.

_`language` : text
    Name of a formal language, passed to Pygments_ for syntax highlighting.
    See `supported languages and markup formats`_ for recognized values.

    Only recognized with the `"code" <"code" role>`__ base role.

.. _writer names: ../../user/config.html#writer-docutils-application


.. _default-role:

Setting the Default Interpreted Text Role
=========================================

.. class:: field-indent-13em

:Directive Type: "default-role"
:Doctree Element: none; affects subsequent parsing
:Directive Arguments: one, optional (new default role name)
:Directive Options: none
:Directive Content: none

The "default-role" directive sets the default interpreted text role,
the role that is used for interpreted text without an explicit role.
For example, after setting the default role like this::

    .. default-role:: subscript

any subsequent use of implicit-role interpreted text in the document
will use the "subscript" role::

    An example of a `default` role.

This will be parsed into the following document tree fragment::

    <paragraph>
        An example of a
        <subscript>
            default
         role.

Custom roles may be used (see the "role_" directive above), but it
must have been declared in a document before it can be set as the
default role.  See the `reStructuredText Interpreted Text Roles`_
document for details of built-in roles.

The directive may be used without an argument to restore the initial
default interpreted text role, which is application-dependent.  The
initial default interpreted text role of the standard reStructuredText
parser is "title-reference".


.. _meta:

Metadata
========

.. class:: field-indent-13em

:Directive Type: "meta"
:Doctree Element: `\<meta>`_
:Directive Arguments: none
:Directive Options: none
:Directive Content: Must contain a flat `field list`_.

The "meta" directive is used to specify metadata\ [#]_ to be stored
in, e.g., `HTML meta elements`_ or as `ODT file properties`_. The
LaTeX writer passes it to the ``pdfinfo`` option of the hyperref_
package. If an output format does not support "invisible" metadata,
content is silently dropped by the writer.

.. note:: Data from some `bibliographic fields`_ is automatically
   extracted and stored as metadata, too. However, Bibliographic
   Fields are also displayed in the document's screen rendering or
   printout.

   For an "invisible" *document title*, see the `metadata document
   title`_ directive below.

Within the directive block, a flat field list provides the syntax for
metadata.  The field name becomes the contents of the "name" attribute
of the META tag, and the field body (interpreted as a single string
without inline markup) becomes the contents of the "content"
attribute.  For example::

    .. meta::
       :description: The reStructuredText plaintext markup language
       :keywords: plaintext, markup language

This would be converted to the following HTML::

    <meta name="description"
        content="The reStructuredText plaintext markup language">
    <meta name="keywords" content="plaintext, markup language">

Support for other META attributes ("http-equiv", "scheme", "lang",
"dir") are provided through field arguments, which must be of the form
"attr=value"::

    .. meta::
       :description lang=en: An amusing story
       :description lang=fr: Une histoire amusante

And their HTML equivalents::

    <meta name="description" lang="en" content="An amusing story">
    <meta name="description" lang="fr" content="Une histoire amusante">

Some META tags use an "http-equiv" attribute instead of the "name"
attribute.  To specify "http-equiv" META tags, simply omit the name::

    .. meta::
       :http-equiv=Content-Type: text/html; charset=ISO-8859-1

HTML equivalent::

    <meta http-equiv="Content-Type"
         content="text/html; charset=ISO-8859-1">

.. [#] "Metadata" is data about data, in this case data about the
   document. Metadata is, e.g., used to describe and classify web
   pages in the World Wide Web, in a form that is easy for search
   engines to extract and collate.

.. _HTML meta elements:
   https://html.spec.whatwg.org/multipage/semantics.html#the-meta-element
.. _ODT file properties:
   https://en.wikipedia.org/wiki/OpenDocument_technical_specification#Metadata
.. _hyperref: https://ctan.org/pkg/hyperref
.. _bibliographic fields: restructuredtext.html#bibliographic-fields
.. _field list: restructuredtext.html#field-lists


.. _title:

Metadata Document Title
=======================

.. class:: field-indent-13em

:Directive Type: "title"
:Doctree Element: sets the `\<document>`_ element's `title attribute`_)
:Directive Arguments: one, required (the title text)
:Directive Options: none
:Directive Content: none
:Configuration Setting: `title <"title" configuration setting_>`__

The "title" directive specifies the document title as metadata, which
does not become part of the document body. It overrides the
document-supplied `document title`_ and the `"title" configuration
setting`_.


Restructuredtext-Test-Directive
===============================

.. class:: field-indent-13em

:Directive Type: "restructuredtext-test-directive"
:Doctree Element: `\<system_message>`_
:Directive Arguments: none
:Directive Options: none
:Directive Content: Interpreted as a literal block.

This directive is provided for test purposes only.  (Nobody is
expected to type in a name *that* long!)  It is converted into a
level-1 (info) system message showing the directive data, possibly
followed by a literal block containing the rest of the directive
block.


--------------
Common Options
--------------

Most of the directives that generate doctree elements support the following
options:

.. _class option:

``class`` : text_ (space separated list of `class names`_)
    Set a `classes attribute`_ value on the doctree element generated by
    the directive. For example, ::

      .. image:: bild.png
         :alt:   example picture
         :class: large-pics

    is the recommended syntax alternative to a preceding
    `class directive`_ ::

      .. class:: large-pics
      .. image:: bild.png
         :alt:   example picture

    .. _`name`:

``name`` : text_
    Add *text* to the `names attribute`_ of the doctree element generated
    by the directive. This allows `hyperlink references`_ to the element
    using `text` as `reference name`_. For example, ::

      .. image:: bild.png
         :alt:   example picture
         :name: my picture

    is the recommended syntax alternative to a preceding
    `hyperlink target`_ ::

      .. _my picture:
      .. image:: bild.png
         :alt:   example picture


-------------------------
Common Option Value Types
-------------------------

.. class:: run-in narrow

:"keyword": recognized keywords

  Used without quotes in the reStructuredText source.

:_`character`: single character

  May be specified as literal character or as Unicode `character code`_
  (cf. the unicode_ directive).

:_`encoding`: text encoding name

  Docutils looks it up in the list of registered codecs_
  (see also `Standard Encodings`_).

:_`flag`: no value

:_`integer`: integer number

  A _`list of integers` may be comma- or whitespace-separated.

:_`length`: number, optionally followed by one of the
  supported `length units`_

  Handling of values without unit depends on the writer/output format.
  See the writer specific documentation in the `user doc`__ for details.

  __ ../../index.html#introductory-tutorial-material-for-end-users

:_`path`: local filesystem path

  Newlines are removed.
  The `root_prefix`_ configuration setting can be used to tell Docutils
  to interpret paths starting with "/" relative to a "project directory".

:_`text`: free text

  Possible restrictions are given in parentheses.

:_`URI`: _`URI reference`

  Full URI or `relative reference`_
  (absolute or relative path reference, cf. :RFC:`3986`).
  Whitespace is removed (cf. `external hyperlink targets`_ in the
  reStructuredText specification).

.. _codecs: https://docs.python.org/3/library/codecs.html
.. _relative reference: https://www.rfc-editor.org/rfc/rfc3986.html#section-4.2
.. _Standard Encodings:
    https://docs.python.org/3/library/codecs.html#standard-encodings

.. _reStructuredText Markup Specification: restructuredtext.html
.. _Directives: restructuredtext.html#directives
.. _document title: restructuredtext.html#document-title
.. _escaping mechanism: restructuredtext.html#escaping-mechanism
.. _external hyperlink targets:
    restructuredtext.html#external-hyperlink-targets
.. _hyperlink references: restructuredtext.html#hyperlink-references
.. _hyperlink target: restructuredtext.html#hyperlink-targets
.. _length units: restructuredtext.html#length-units
.. _percentage: restructuredtext.html#percentage-units
.. _reference name:
.. _reference names: restructuredtext.html#reference-names
.. _reStructuredText table: restructuredtext.html#tables

.. _reStructuredText Interpreted Text Roles:
.. _interpreted text role: roles.html
.. _"code" role: roles.html#code
.. _"math" role: roles.html#math
.. _"raw" role: roles.html#raw

.. Docutils Configuration
.. _file_insertion_enabled: ../../user/config.html#file-insertion-enabled
.. _generate_oowriter_toc: ../../user/config.html#generate-oowriter-toc
.. _image_loading: ../../user/config.html#image-loading
.. _input_encoding: ../../user/config.html#input-encoding
.. _math_output: ../../user/config.html#math-output
.. _"parser" configuration setting: ../../user/config.html#parser
.. _raw_enabled: ../../user/config.html#raw-enabled
.. _root_prefix: ../../user/config.html#root-prefix
.. _sectnum_xform: ../../user/config.html#sectnum-xform
.. _syntax_highlight: ../../user/config.html#syntax-highlight
.. _tab_width: ../../user/config.html#tab-width
.. _table_style: ../../user/config.html#table-style
.. _"title" configuration setting: ../../user/config.html#title
.. _toc_backlinks: ../../user/config.html#toc-backlinks
.. _use_latex_toc: ../../user/config.html#use-latex-toc

.. _reStructuredText Standard Definition Files: definitions.html

.. _The Docutils Document Tree: ../doctree.html
.. _identifiers: ../doctree.html#identifiers
.. _inline elements: ../doctree.html#inline-elements
.. _class names: ../doctree.html#class-names
.. _auto attribute: ../doctree.html#auto
.. _classes:
.. _classes attribute: ../doctree.html#classes
.. _format attribute: ../doctree.html#format
.. _ids attribute: ../doctree.html#ids
.. _loading attribute: ../doctree.html#loading
.. _names attribute: ../doctree.html#names
.. _title attribute: ../doctree.html#title-attribute
.. _uri attribute: ../doctree.html#uri
.. _width attribute: ../doctree.html#width
.. _<admonition>: ../doctree.html#admonition
.. _<attention>: ../doctree.html#attention
.. _<block_quote>: ../doctree.html#block-quote
.. _<caption>: ../doctree.html#caption
.. _<caution>: ../doctree.html#caution
.. _<compound>: ../doctree.html#compound
.. _<container>: ../doctree.html#container
.. _<danger>: ../doctree.html#danger
.. _<decoration>: ../doctree.html#decoration
.. _<document>: ../doctree.html#document
.. _<error>: ../doctree.html#error
.. _<figure>: ../doctree.html#figure
.. _<footer>: ../doctree.html#footer
.. _<footnote>: ../doctree.html#footnote
.. _<footnote_reference>: ../doctree.html#footnote-reference
.. _<generated>: ../doctree.html#generated
.. _<header>: ../doctree.html#header
.. _<hint>: ../doctree.html#hint
.. _<image>: ../doctree.html#image
.. _<inline>: ../doctree.html#inline
.. _<important>: ../doctree.html#important
.. _<legend>: ../doctree.html#legend
.. _<line_block>: ../doctree.html#line-block
.. _<literal_block>: ../doctree.html#literal-block
.. _<math_block>: ../doctree.html#math-block
.. _<meta>: ../doctree.html#meta
.. _<note>: ../doctree.html#note
.. _<pending>: ../doctree.html#pending
.. _<raw>: ../doctree.html#raw
.. _<reference>: ../doctree.html#reference
.. _<rubric>: ../doctree.html#rubric
.. _<sidebar>: ../doctree.html#sidebar
.. _<system_message>: ../doctree.html#system-message
.. _<table>: ../doctree.html#table
.. _<tip>: ../doctree.html#tip
.. _<title>: ../doctree.html#title
.. _<topic>: ../doctree.html#topic
.. _<warning>: ../doctree.html#warning


.. Emacs settings

   Local Variables:
   mode: indented-text
   mode: rst
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
