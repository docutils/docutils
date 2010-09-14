=================
Development notes
=================

This document should not be included in the release distribution, and serves
simply as internal notes and reminders for why certain things are the way they
are.


The literal and quote problem
-----------------------------

20091024 PMA: The reasoning and history here is complicated so it needs to be
written down. Way back in the genesis of rst2beamer, a problem was encountered
with literals. The docutils LateX writer typeset them correctly in a regular
monospace (\ttfamily) font. However in Beamer documents they appeared as an
italic monospace font. This is because (a) docutils expresses literal blocks
as a \quote, and (b) the Beamer document class sets the quote font to be
italic. (See the beamer themes/fonts files.) Way back, I apparently solved
this problem by overriding the LaTeX writers' visit and depart methods for
literals.

Dim memory says that this worked in the days of docutils 0.4, but now
(docutils 0.6), it strips the indenting from literal blocks. The solution is
thus to use the parent literal methods, but to set the quote font to ttfamily
before visiting a literal and reset it after leaving. This appears to work.


Using Pygments to highlight code
--------------------------------

20091029 PMA: Duplicating the Sphinx ability to use Pygments to highlight code
is both easy and hard. You have to use the fancyvrb package to get the
Verbatim environment that Pygments produces. You also have to include a set of
definitions that the LatexFormatter produces in the header. Also, there is
weird problem that crops up when you use a Verbatim environment inside a
frame. You get odd errors about something not completing or terminating,
unless you give a "fragile" qualifier to the frame::

   \begin{frame}[fragile]
   
The commandline arguments that refer to codeblocks are arguably verbose but
clear.

The objective here is to use a syntax that is compatible and simple, which we
do by implementing behaviour just like those in Sphinx, thus making documents
portable to Sphinx.

Initially, it seemed sensible to a "container trick" with codeblocks, allowing
codeblocks within containers with a special class. As with columns, this
should allow a simple portability without requiring a special directive. But
there's a few reasons why this won't work:

* Making it a container means that the contents will be parsed and formatting
  lost.

* We can't put a class on a literal block, because in ReST it isn't a
  directive, it's pure syntax (`::`).

* Parsed literal blocks are a directive, but parsing it for ReST markup and
  highlighting it with Pygments presents an intractable tangle.

* Line blocks are meant for line formatted normal text (like poetry), and
  perverting it for codeblocks seems inappropriate.

If you use tabs to indent a codeblock, the indenting comes out in Beamer as
very wide - 8 spaces each. Thus the "replace-tabs" argument was introduced,
but things are complicated. Upon reading ReST replaces tabs with 8 spaces.
This happens even for literal and raw blocks. Thus, to resize the tabs (e.g.
replace them with 3 spaces not 8), we have to search and replace 8 space
stretches. If the source has mixed tabs and spaces, or if the "replace-tabs"
option is used where spaces are used for indenting, bad things could happen.

It's unclear to me what happens if you try to syntax-color (or guess the
language of) incorrect or malformed code.


Normalise directive names
-------------------------

20091029 PMA: Most docutils names use hyphens as opposed to underscores (e.g.
``code-blocks``). As an added complexity, underscores appear to be translated
into hyphens by docutils, so we've gotten in a a scrap allowing all sorts of
names. From this point on, hyphens shall be the "official" style (``r2b-`` as
opposed to ``r2b_``), although existing underscores shall maintained silently
for compatibility.

