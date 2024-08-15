.. include:: ../../header2.rst

=====================================
 An Introduction to reStructuredText
=====================================
:Author: David Goodger
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

reStructuredText_ is an easy-to-read, what-you-see-is-what-you-get
plaintext markup syntax and parser system.  It is useful for inline
program documentation (such as Python docstrings), for quickly
creating simple web pages, and for standalone documents.
reStructuredText_ is a proposed revision and reinterpretation of the
StructuredText_ and Setext_ lightweight markup systems.

reStructuredText is designed for extensibility for specific
application domains.  Its parser is a component of Docutils_.

This document defines the goals_ of reStructuredText and provides a
history_ of the project.  It is written using the reStructuredText
markup, and therefore serves as an example of its use.  For a gentle
introduction to using reStructuredText, please read `A
ReStructuredText Primer`_.  The `Quick reStructuredText`_ user
reference is also useful.  The `reStructuredText Markup
Specification`_ is the definitive reference.  There is also an
analysis of the `Problems With StructuredText`_.

ReStructuredText's web page is
https://docutils.sourceforge.io/rst.html.

.. _reStructuredText: https://docutils.sourceforge.io/rst.html
.. _StructuredText: https://zopestructuredtext.readthedocs.org/
.. _Setext: https://docutils.sourceforge.io/mirror/setext.html
.. _Docutils: https://docutils.sourceforge.io/
.. _A ReStructuredText Primer: ../../user/rst/quickstart.html
.. _Quick reStructuredText: ../../user/rst/quickref.html
.. _reStructuredText Markup Specification: restructuredtext.html
.. _Problems with StructuredText: ../../dev/rst/problems.html
.. _history: ./history.html


Goals
=====

The primary goal of reStructuredText_ is to define a markup syntax for
use in Python docstrings and other documentation domains, that is
readable and simple, yet powerful enough for non-trivial use.  The
intended purpose of the reStructuredText markup is twofold:

- the establishment of a set of standard conventions allowing the
  expression of structure within plaintext, and

- the conversion of such documents into useful structured data
  formats.

To clarify the primary goal, here are specific design goals, in order,
beginning with the most important:

1. Readable.  The marked-up text must be easy to read without any
   prior knowledge of the markup language.  It should be as easily
   read in raw form as in processed form.

2. Unobtrusive.  The markup that is used should be as simple and
   unobtrusive as possible.  The simplicity of markup constructs
   should be roughly proportional to their frequency of use.  The most
   common constructs, with natural and obvious markup, should be the
   simplest and most unobtrusive.  Less common constructs, for which
   there is no natural or obvious markup, should be distinctive.

3. Unambiguous.  The rules for markup must not be open for
   interpretation.  For any given input, there should be one and only
   one possible output (including error output).

4. Unsurprising.  Markup constructs should not cause unexpected output
   upon processing.  As a fallback, there must be a way to prevent
   unwanted markup processing when a markup construct is used in a
   non-markup context (for example, when documenting the markup syntax
   itself).

5. Intuitive.  Markup should be as obvious and easily remembered as
   possible, for the author as well as for the reader.  Constructs
   should take their cues from such naturally occurring sources as
   plaintext email messages, newsgroup postings, and text
   documentation such as README.rst files.

6. Easy.  It should be easy to mark up text using any ordinary text
   editor.

7. Scalable.  The markup should be applicable regardless of the length
   of the text.

8. Powerful.  The markup should provide enough constructs to produce a
   reasonably rich structured document.

9. Language-neutral.  The markup should apply to multiple natural (as
   well as artificial) languages, not only English.

10. Extensible.  The markup should provide a simple syntax and
    interface for adding more complex general markup, and custom
    markup.

11. Output-format-neutral.  The markup will be appropriate for
    processing to multiple output formats, and will not be biased
    toward any particular format.

The design goals above were used as criteria for accepting or
rejecting syntax, or selecting between alternatives.

It is emphatically *not* the goal of reStructuredText to define
docstring semantics, such as docstring contents or docstring length.
These issues are orthogonal to the markup syntax and beyond the scope
of this specification.

Also, it is not the goal of reStructuredText to maintain compatibility
with StructuredText_ or Setext_.  reStructuredText shamelessly steals
their great ideas and ignores the not-so-great.

Author's note:

    Due to the nature of the problem we're trying to solve (or,
    perhaps, due to the nature of the proposed solution), the above
    goals unavoidably conflict.  I have tried to extract and distill
    the wisdom accumulated over the years in the Python Doc-SIG_
    mailing list and elsewhere, to come up with a coherent and
    consistent set of syntax rules, and the above goals by which to
    measure them.

    There will inevitably be people who disagree with my particular
    choices.  Some desire finer control over their markup, others
    prefer less.  Some are concerned with very short docstrings,
    others with full-length documents.  This specification is an
    effort to provide a reasonably rich set of markup constructs in a
    reasonably simple form, that should satisfy a reasonably large
    group of reasonable people.

    David Goodger (goodger@python.org), 2001-04-20

.. _Doc-SIG: https://www.python.org/sigs/doc-sig/


.. Emacs settings

   Local Variables:
   mode: indented-text
   mode: rst
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
