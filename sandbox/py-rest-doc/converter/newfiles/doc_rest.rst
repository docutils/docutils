.. highlightlang:: rest

reStructuredText Primer
=======================

This section is a brief introduction to reStructuredText (reST) concepts and
syntax, to provide authors enough information to autor documents productively.
Since reST was designed to be a simple, unobtrusive markup language, this will
not take too long.

For all further question that may arise, refer to the `reStructuredText User
Documentation <http://docutils.sourceforge.net/rst.html>`_.


Paragraphs
----------

The most basic block a reST document is made of. Paragraphs are chunks of text
separated by one ore more blank lines. As in Python, indentation is significant
in reST, so all lines of a paragraph must be left-aligned.


Inline markup
-------------

The standard reST inline markup is quite simple: use

* one asterisk: ``*text*`` for emphasis (italics),
* two asterisks: ``**text**`` for strong emphasis (boldface), and
* backquotes: ````text```` for code samples.

If asterisks or backquotes appear in running text and could be confused with
inline markup delimiters, they have to be escaped with a backslash.

Be aware of some restrictions of this markup:

* it may not be nested,
* content may not start or end with whitespace: ``* text*`` is wrong,
* it must be separated from surrounding text by non-word characters.  Use a
  backslash escaped space to work around that: ``thisis\ *one*\ word``.

These restrictions may be lifted in future versions of the docutils.

reST also allows for custom \`\`interpreted text roles'', which signify that the
enclosed text should be interpreted in a specific way. Sphinx uses this to
provide semantic markup and cross-referencing of identifiers, as described in
the appropriate section.


Lists and Quotes
----------------

List markup is natural: just place an asterisk at the start of a paragraph and
indent properly. The same goes for numbered lists; they can also be autonumbered
using a ``#`` sign::

   * This is a bulleted list.
   * It has two items, the second
     item uses two lines.

   #. This is a numbered list.
   #. It has two items too.

Nested lists are possible, but be aware that they must be separated from the
parent list items by blank lines::

   * this is
   * a list

     * with a nested list
     * and some subitems

   * and here the parent list continues

Definition lists are created as follows::

   term (up to a line of text)
      Definition of the term, which must be indented

      and can even consist of multiple paragraphs

   next term
      Description.


Paragraphs are quoted by just indenting them more than the surrounding
paragraphs.


Source Code
-----------

Literal code blocks are introduced by ending a paragraph with the special marker
``::``. The literal block must be indented, to be able to include blank lines::

   This is a normal text paragraph. The next paragraph is a code sample::

      It is not processed in any way, except
      that the indentation is removed.

      It can span multiple lines.

   This is a normal text paragraph again.

The handling of the ``::`` marker is smart:

* If it occurs in a paragraph of its own, that paragraph is completely left
  out of the document.
* If it is preceded by whitespace, the marker is removed.
* If it is preceded by non-whitespace, the marker is replaced by a single
  colon.

That way, the second sentence in the above example's first paragraph would be
rendered as \`\`The next paragraph is a code sample:''.


Sections
--------

Section headers are created by underlining (and optionally overlining) the
section title with a punctuation character, at least as long as the text::

   =================
   This is a heading
   =================

Normally, there are no heading levels assigned to certain characters as the
structure is determined from the succession of headings. However, for the Python
documentation, we use this convention:

* ``#`` with overline, for parts
* ``*`` with overline, for chapters
* ``=``, for sections
* ``-``, for subsections
* ``^``, for subsubsections
* ``"``, for paragraphs


Footnotes
---------

XXX

Hyperlinks
----------

XXX

Comments
--------

XXX



Directives
----------

XXX


Source encoding
---------------

Since the easiest way to include special characters like em dashes or copyright
signs in reST is to directly write them as Unicode characters, one has to
specify an encoding:

All Python documentation source files must be in UTF-8 encoding, and the HTML
documents written from them will be in that encoding as well.


XXX: Gotchas