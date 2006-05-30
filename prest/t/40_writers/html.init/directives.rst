=============================
 reStructuredText Directives
=============================
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision: 1.1 $
:Date: $Date: 2006-01-26 10:53:09 -0600 (Thu, 26 Jan 2006) $

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

.. _Directives: ./reStructuredText.html#directives
.. _reStructuredText Markup Specification: ./reStructuredText.html


.. contents::


-------------
 Admonitions
-------------

:Directive Types: "attention", "caution", "danger", "error", "hint",
                  "important", "note", "tip", "warning"
:DTD Elements: attention, caution, danger, error, hint, important,
               note, tip, warning
:Directive Arguments: None.
:Directive Options: None.
:Directive Content: Interpreted as body elements.

Admonitions are specially marked "topics" that can appear anywhere an
ordinary body element can.  They contain arbitrary body elements.
Typically, an admonition is rendered as an offset block in a document,
sometimes outlined or shaded, with a title matching the admonition
type.  For example::

    .. DANGER::
       Beware killer rabbits!

This directive might be rendered something like this::

    +------------------------+
    |        !DANGER!        |
    |                        |
    | Beware killer rabbits! |
    +------------------------+

The following admonition directives have been implemented:

- attention
- caution
- danger
- error
- hint
- important
- note
- tip
- warning

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


--------
 Images
--------

There are two image directives: "image" and "figure".


Image
=====

:Directive Type: "image"
:DTD Element: image
:Directive Arguments: One, required (image URI).
:Directive Options: Possible.
:Directive Content: None.

An "image" is a simple picture::

    .. image:: picture.png

The URI for the image source file is specified in the directive
argument.  As with hyperlink targets, the image URI may begin on the
same line as the explicit markup start and target name, or it may
begin in an indented text block immediately following, with no
intervening blank lines.  If there are multiple lines in the link
block, they are stripped of leading and trailing whitespace and joined
together.

Optionally, the image link block may end with a flat field list, the
_`image options`.  For example::

    .. image:: picture.jpeg
       :height: 100
       :width: 200
       :scale: 50
       :alt: alternate text
       :align: right

The following options are recognized:

``alt`` : text
    Alternate text: a short description of the image, displayed by
    applications that cannot display images, or spoken by applications
    for visually impaired users.

``height`` : integer
    The height of the image in pixels, used to reserve space or scale
    the image vertically.

``width`` : integer
    The width of the image in pixels, used to reserve space or scale
    the image horizontally.

``scale`` : integer
    The uniform scaling factor of the image, a percentage (but no "%"
    symbol is required or allowed).  "100" means full-size.

``align`` : "top", "middle", "bottom", "left", "center", or "right"
    The alignment of the image, equivalent to the HTML ``<img>`` tag's
    "align" attribute.  The values "top", "middle", and "bottom"
    control an image's vertical alignment (relative to the text
    baseline); they are only useful for inline images (substitutions).
    The values "left", "center", and "right" control an image's
    horizontal alignment, allowing the image to float and have the
    text flow around it.  The specific behavior depends upon the
    browser or rendering software used.


Figure
======

:Directive Type: "figure"
:DTD Elements: figure, image, caption, legend
:Directive Arguments: One, required (image URI).
:Directive Options: Possible; same as those of the `image`_ directive.
:Directive Content: Interpreted as the figure caption and an optional
                    legend.

A "figure" consists of image_ data (including `image options`_), an
optional caption (a single paragraph), and an optional legend
(arbitrary body elements)::

    .. figure:: picture.png
       :scale: 50
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


---------------
 Body Elements
---------------

Topic
=====

:Directive Type: "topic"
:DTD Element: topic
:Directive Arguments: 1, required (topic title).
:Directive Options: None.
:Directive Content: Interpreted as the topic body.

A topic is like a block quote with a title, or a self-contained
section with no subsections.  Use the "topic" directive to indicate a
self-contained idea that is separate from the flow of the document.
Topics may occur anywhere a section or transition may occur.  Body
elements (including topics) may not contain nested topics.

The directive's sole argument is interpreted as the topic title; the
next line must be blank.  All subsequent lines make up the topic body,
interpreted as body elements.  For example::

    topic:: Topic Title

        Subsequent indented lines comprise
        the body of the topic, and are
        interpreted as body elements.


Line Block
==========

:Directive Type: "line-block"
:DTD Element: line_block
:Directive Arguments: None.
:Directive Options: None.
:Directive Content: Becomes the body of the line block.

The "line-block" directive constructs an element where whitespace
(including linebreaks) is significant and inline markup is supported.
It is equivalent to a `parsed literal block`_ with different
rendering: typically in an ordinary serif typeface instead of a
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


Parsed Literal Block
====================

:Directive Type: "parsed-literal"
:DTD Element: literal_block
:Directive Arguments: None.
:Directive Options: None.
:Directive Content: Becomes the body of the literal block.

Unlike an ordinary literal block, the "parsed-literal" directive
constructs a literal block where the text is parsed for inline markup.
It is equivalent to a `line block`_ with different rendering:
typically in a typewriter/monospaced typeface, like an ordinary
literal block.  Parsed literal blocks are useful for adding hyperlinks
to code examples.  However, care must be taken with the text, because
inline markup is recognized; there is no protection from parsing.
Backslash-escapes may be necessary in places.

For example, all the element names in this content model are links::

    .. parsed-literal::

        ((title_,
          subtitle_?)?,
         docinfo_?,
         decoration_?,
         `%structure.model;`_)


----------------
 Document Parts
----------------

Table of Contents
=================

:Directive Type: "contents"
:DTD Elements: pending, topic
:Directive Arguments: One, optional: title.
:Directive Options: Possible.
:Directive Content: None.

The "contents" directive inserts a table of contents (TOC) in two
passes: initial parse and transform.  During the initial parse, a
"pending" element is generated which acts as a placeholder, storing
the TOC title and any options internally.  At a later stage in the
processing, the "pending" element is replaced by a "topic" element, a
title and the table of contents proper.

The directive in its simplest form::

    .. contents::

Language-dependent boilerplate text will be used for the title.  The
English default title text is "Contents".

An explicit title, may be specified::

    .. contents:: Table of Contents

The title may span lines, although it is not recommended::

    .. contents:: Here's a very long Table of
       Contents title

Options may be specified for the directive, using a field list::

    .. contents:: Table of Contents
       :depth: 2

If the default title is to be used, the options field list may begin
on the same line as the directive marker::

    .. contents:: :depth: 2

The following options are recognized:

``depth`` : integer
    The number of section levels that are collected in the table of
    contents.  The default is unlimited depth.
``local`` : flag (empty)
    Generate a local table of contents.  Entries will only include
    subsections of the section in which the directive is given.  If no
    explicit title is given, the table of contents will not be titled.
``backlinks`` : "entry" or "top" or "none"
    Generate links from section headers back to the table of contents
    entries, the table of contents itself, or generate no backlinks.


Automatic Section Numbering
===========================

:Directive Type: "sectnum" or "section-autonumbering" (synonyms)
:DTD Elements: pending, generated
:Directive Arguments: None.
:Directive Options: Possible.
:Directive Content: None.

The "sectnum" (or "section-autonumbering") directive automatically
numbers sections and subsections in a document.  Section numbers are
of the "multiple enumeration" form, where each level has a number,
separated by periods.  For example, the title of section 1, subsection
2, subsubsection 3 would have "1.2.3" prefixed.

The "sectnum" directive does its work in two passes: the initial parse
and a transform.  During the initial parse, a "pending" element is
generated which acts as a placeholder, storing any options internally.
At a later stage in the processing, the "pending" element triggers a
transform, which adds section numbers to titles.  Section numbers are
enclosed in a "generated" element, and titles have their "auto"
attribute set to "1".

The following options are recognized:

``depth`` : integer
    The number of section levels that are numbered by this directive.
    The default is unlimited depth.


------------
 References
------------

Target Footnotes
================

:Directive Type: "target-notes"
:DTD Elements: pending, footnote, footnote_reference
:Directive Arguments: None.
:Directive Options: None.
:Directive Content: None.

The "target-notes" directive creates a footnote for each external
target in the text, and corresponding footnote references after each
reference.  For every explicit target (of the form, ``.. _target name:
URL``) in the text, a footnote will be generated containing the
visible URL as content.


Footnotes
=========

**NOT IMPLEMENTED YET**

:Directive Type: "footnotes"
:DTD Elements: pending, topic
:Directive Arguments: None?
:Directive Options: Possible?
:Directive Content: None.

@@@


Citations
=========

**NOT IMPLEMENTED YET**

:Directive Type: "citations"
:DTD Elements: pending, topic
:Directive Arguments: None?
:Directive Options: Possible?
:Directive Content: None.

@@@


---------------
 HTML-Specific
---------------

Meta
====

:Directive Type: "meta"
:DTD Element: meta (non-standard)
:Directive Arguments: None.
:Directive Options: None.
:Directive Content: Must contain a flat field list.

The "meta" directive is used to specify HTML metadata stored in HTML
META tags.  "Metadata" is data about data, in this case data about web
pages.  Metadata is used to describe and classify web pages in the
World Wide Web, in a form that is easy for search engines to extract
and collate.

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
       :description lang=fr: Un histoire amusant

And their HTML equivalents::

    <meta name="description" lang="en" content="An amusing story">
    <meta name="description" lang="fr" content="Un histoire amusant">

Some META tags use an "http-equiv" attribute instead of the "name"
attribute.  To specify "http-equiv" META tags, simply omit the name::

    .. meta::
       :http-equiv=Content-Type: text/html; charset=ISO-8859-1

HTML equivalent::

    <meta http-equiv="Content-Type"
         content="text/html; charset=ISO-8859-1">


Imagemap
========

**NOT IMPLEMENTED YET**

Non-standard element: imagemap.


---------------
 Miscellaneous
---------------

Including an External Document Fragment
=======================================

:Directive Type: "include"
:DTD Elements: depend on data being included
:Directive Arguments: One, required (path to include file).
:Directive Options: Possible.
:Directive Content: None.

The "include" directive reads a reStructuredText-formatted text file
and parses it in the current document's context at the point of the
directive.  For example::

    This first example will be parsed at the document level, and can
    thus contain any construct, including section headers.

    .. include:: inclusion.txt

            This second will be parsed in a block quote context.
            Therefore it may only contain body elements.  It may not
            contain section headers.

            .. include:: inclusion.txt

If an included document fragment contains section structure, the title
adornments must match those of the master document.

The following options are recognized:

``literal`` : flag (empty)
    The entire included text is inserted into the document as a single
    literal block (useful for program listings).


Raw Data Pass-Through
=====================

:Directive Type: "raw"
:DTD Element: pending
:Directive Arguments: One, required (output format type).
:Directive Options: Possible.
:Directive Content: Stored verbatim, uninterpreted.  None (empty) if a
                    "file" or "url" option given.

The "raw" directive indicates non-reStructuredText data that is to be
passed untouched to the Writer.  The name of the output format is
given in the first argument.  During the initial parse, a "pending"
element is generated which acts as a placeholder, storing the format
and raw data internally.  The interpretation of the code is up to the
Writer.  A Writer may ignore any raw output not matching its format.

For example, the following input would be passed untouched by an HTML
Writer::

    .. raw:: html

       <hr width=50 size=10>

A LaTeX Writer could insert the following raw content into its
output stream::

    .. raw:: latex

       \documentclass[twocolumn]{article}

Raw data can also be read from an external file, specified in a
directive option.  In this case, the content block must be empty.  For
example::

    .. raw:: html
       :file: inclusion.html

The following options are recognized:

``file`` : string
    The local filesystem path of a raw data file to be included.
``url`` : string
    An Internet URL reference to a raw data file to be included.


Replacement Text
================

:Directive Type: "replace"
:DTD Element: pending
:Directive Arguments: None.
:Directive Options: None.
:Directive Content: A single paragraph; may contain inline markup.

The "replace" directive is used to indicate replacement text for a
substitution reference.  It may be used within substitution
definitions only.  For example, this directive can be used to expand
abbreviations::

    .. |reST| replace:: reStructuredText

    Yes, |reST| is a long word, so I can't blame anyone for wanting to
    abbreviate it.

As reStructuredText doesn't support nested inline markup, the only way
to create a reference with styled text is to use substitutions with
the "replace" directive::

    I recommend you try |Python|_.

    .. |Python| replace:: Python, *the* best language around
    .. _Python: http://www.python.org/


Restructuredtext-Test-Directive
===============================

:Directive Type: "restructuredtext-test-directive"
:DTD Element: system_warning
:Directive Arguments: None.
:Directive Options: None.
:Directive Content: Interpreted as a literal block.

This directive is provided for test purposes only.  (Nobody is
expected to type in a name *that* long!)  It is converted into a
level-1 (info) system message showing the directive data, possibly
followed by a literal block containing the rest of the directive
block.


..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
