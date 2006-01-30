==================================================
 A Record of reStructuredText Syntax Alternatives
==================================================
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision: 1.2 $
:Date: $Date: 2006-01-26 10:53:09 -0600 (Thu, 26 Jan 2006) $

The following are ideas, alternatives, and justifications that were
considered for reStructuredText syntax, which did not originate with
Setext_ or StructuredText_.  For an analysis of constructs which *did*
originate with StructuredText or Setext, please see `Problems With
StructuredText`_.  See the `reStructuredText Markup Specification`_
for full details of the established syntax.

.. _Setext: http://docutils.sourceforge.net/mirror/setext.html
.. _StructuredText:
   http://dev.zope.org/Members/jim/StructuredTextWiki/FrontPage
.. _Problems with StructuredText: problems.html
.. _reStructuredText Markup Specification: reStructuredText.html


.. contents::


... Or Not To Do?
=================

This is the realm of the possible but questionably probable.  These
ideas are kept here as a record of what has been proposed, for
posterity and in case any of them prove to be useful.


Compound Enumerated Lists
-------------------------

Allow for compound enumerators, such as "1.1." or "1.a." or "1(a)", to
allow for nested enumerated lists without indentation?


Sloppy Indentation of List Items
--------------------------------

Perhaps the indentation shouldn't be so strict.  Currently, this is
required::

    1. First line,
       second line.

Anything wrong with this? ::

    1. First line,
     second line.

Problem? ::

    1. First para.

       Block quote.  (no good: requires some indent relative to first
       para)

     Second Para.

    2. Have to carefully define where the literal block ends::

         Literal block

       Literal block?

Hmm...  Non-strict indentation isn't such a good idea.


Lazy Indentation of List Items
------------------------------

Another approach: Going back to the first draft of reStructuredText
(2000-11-27 post to Doc-SIG)::

    - This is the fourth item of the main list (no blank line above).
    The second line of this item is not indented relative to the
    bullet, which precludes it from having a second paragraph.

Change that to *require* a blank line above and below, to reduce
ambiguity.  This "loosening" may be added later, once the parser's
been nailed down.  However, a serious drawback of this approach is to
limit the content of each list item to a single paragraph.


David's Idea for Lazy Indentation
`````````````````````````````````

Consider a paragraph in a word processor.  It is a single logical line
of text which ends with a newline, soft-wrapped arbitrarily at the
right edge of the page or screen.  We can think of a plaintext
paragraph in the same way, as a single logical line of text, ending
with two newlines (a blank line) instead of one, and which may contain
arbitrary line breaks (newlines) where it was accidentally
hard-wrapped by an application.  We can compensate for the accidental
hard-wrapping by "unwrapping" every unindented second and subsequent
line.  The indentation of the first line of a paragraph or list item
would determine the indentation for the entire element.  Blank lines
would be required between list items when using lazy indentation.

The following example shows the lazy indentation of multiple body
elements::

    - This is the first paragraph
    of the first list item.

      Here is the second paragraph
    of the first list item.

    - This is the first paragraph
    of the second list item.

      Here is the second paragraph
    of the second list item.

A more complex example shows the limitations of lazy indentation::

    - This is the first paragraph
    of the first list item.

      Next is a definition list item:

      Term
          Definition.  The indentation of the term is
    required, as is the indentation of the definition's
    first line.

          When the definition extends to more than
    one line, lazy indentation may occur.  (This is the second
    paragraph of the definition.)

    - This is the first paragraph
    of the second list item.

      - Here is the first paragraph of
    the first item of a nested list.

      So this paragraph would be outside of the nested list,
    but inside the second list item of the outer list.

    But this paragraph is not part of the list at all.

And the ambiguity remains::

    - Look at the hyphen at the beginning of the next line
    - is it a second list item marker, or a dash in the text?

    Similarly, we may want to refer to numbers inside enumerated
    lists:

    1. How many socks in a pair? There are
    2. How many pants in a pair? Exactly
    1. Go figure.

Literal blocks and block quotes would still require consistent
indentation for all their lines.  For block quotes, we might be able
to get away with only requiring that the first line of each contained
element be indented.  For example::

    Here's a paragraph.

        This is a paragraph inside a block quote.
    Second and subsequent lines need not be indented at all.

        - A bullet list inside
    the block quote.

          Second paragraph of the
    bullet list inside the block quote.

Although feasible, this form of lazy indentation has problems.  The
document structure and hierarchy is not obvious from the indentation,
making the source plaintext difficult to read.  This will also make
keeping track of the indentation while writing difficult and
error-prone.  However, these problems may be acceptable for Wikis and
email mode, where we may be able to rely on less complex structure
(few nested lists, for example).


Field Lists
===========

Prior to the syntax for field lists being finalized, several
alternatives were proposed.

1. Unadorned RFC822_ everywhere::

       Author: Me
       Version: 1

   Advantages: clean, precedent (RFC822-compliant).  Disadvantage:
   ambiguous (these paragraphs are a prime example).

   Conclusion: rejected.

2. Special case: use unadorned RFC822_ for the very first or very last
   text block of a document::

       """
       Author: Me
       Version: 1

       The rest of the document...
       """

   Advantages: clean, precedent (RFC822-compliant).  Disadvantages:
   special case, flat (unnested) field lists only, still ambiguous::

       """
       Usage: cmdname [options] arg1 arg2 ...

       We obviously *don't* want the like above to be interpreted as a
       field list item.  Or do we?
       """

   Conclusion: rejected for the general case, accepted for specific
   contexts (PEPs, email).

3. Use a directive::

       .. fields::

          Author: Me
          Version: 1

   Advantages: explicit and unambiguous, RFC822-compliant.
   Disadvantage: cumbersome.

   Conclusion: rejected for the general case (but such a directive
   could certainly be written).

4. Use Javadoc-style::

       @Author: Me
       @Version: 1
       @param a: integer

   Advantages: unambiguous, precedent, flexible.  Disadvantages:
   non-intuitive, ugly, not RFC822-compliant.

   Conclusion: rejected.

5. Use leading colons::

       :Author: Me
       :Version: 1

   Advantages: unambiguous, obvious (*almost* RFC822-compliant),
   flexible, perhaps even elegant.  Disadvantages: no precedent, not
   quite RFC822-compliant.

   Conclusion: accepted!

6. Use double colons::

       Author:: Me
       Version:: 1

   Advantages: unambiguous, obvious? (*almost* RFC822-compliant),
   flexible, similar to syntax already used for literal blocks and
   directives.  Disadvantages: no precedent, not quite
   RFC822-compliant, similar to syntax already used for literal blocks
   and directives.

   Conclusion: rejected because of the syntax similarity & conflicts.

Why is RFC822 compliance important?  It's a universal Internet
standard, and super obvious.  Also, I'd like to support the PEP format
(ulterior motive: get PEPs to use reStructuredText as their standard).
But it *would* be easy to get used to an alternative (easy even to
convert PEPs; probably harder to convert python-deviants ;-).

Unfortunately, without well-defined context (such as in email headers:
RFC822 only applies before any blank lines), the RFC822 format is
ambiguous.  It is very common in ordinary text.  To implement field
lists unambiguously, we need explicit syntax.

The following question was posed in a footnote:

   Should "bibliographic field lists" be defined at the parser level,
   or at the DPS transformation level?  In other words, are they
   reStructuredText-specific, or would they also be applicable to
   another (many/every other?) syntax?

The answer is that bibliographic fields are a
reStructuredText-specific markup convention.  Other syntaxes may
implement the bibliographic elements explicitly.  For example, there
would be no need for such a transformation for an XML-based markup
syntax.

.. _RFC822: http://www.rfc-editor.org/rfc/rfc822.txt


Interpreted Text "Roles"
========================

The original purpose of interpreted text was as a mechanism for
descriptive markup, to describe the nature or role of a word or
phrase.  For example, in XML we could say "<function>len</function>"
to mark up "len" as a function.  It is envisaged that within Python
docstrings (inline documentation in Python module source files, the
primary market for reStructuredText) the role of a piece of
interpreted text can be inferred implicitly from the context of the
docstring within the program source.  For other applications, however,
the role may have to be indicated explicitly.

Interpreted text is enclosed in single backquotes (`).

1. Initially, it was proposed that an explicit role could be indicated
   as a word or phrase within the enclosing backquotes:

   - As a prefix, separated by a colon and whitespace::

         `role: interpreted text`

   - As a suffix, separated by whitespace and a colon::

         `interpreted text :role`

   There are problems with the initial approach:

   - There could be ambiguity with interpreted text containing colons.
     For example, an index entry of "Mission: Impossible" would
     require a backslash-escaped colon.

   - The explicit role is descriptive markup, not content, and will
     not be visible in the processed output.  Putting it inside the
     backquotes doesn't feel right; the *role* isn't being quoted.

2. Tony Ibbs suggested that the role be placed outside the
   backquotes::

       role:`prefix` or `suffix`:role

   This removes the embedded-colons ambiguity, but limits the role
   identifier to be a single word (whitespace would be illegal).
   Since roles are not meant to be visible after processing, the lack
   of whitespace support is not important.

   The suggested syntax remains ambiguous with respect to ratios and
   some writing styles.  For example, suppose there is a "signal"
   identifier, and we write::

       ...calculate the `signal`:noise ratio.

   "noise" looks like a role.

3. As an improvement on #2, we can bracket the role with colons::

       :role:`prefix` or `suffix`:role:

   This syntax is similar to that of field lists, which is fine since
   both are doing similar things: describing.

   This is the syntax chosen for reStructuredText.

4. Another alternative is two colons instead of one::

       role::`prefix` or `suffix`::role

   But this is used for analogies ("A:B::C:D": "A is to B as C is to
   D").

   Both alternative #2 and #4 lack delimiters on both sides of the
   role, making it difficult to parse (by the reader).

5. Some kind of bracketing could be used:

   - Parentheses::

         (role)`prefix` or `suffix`(role)

   - Braces::

         {role}`prefix` or `suffix`{role}

   - Square brackets::

         [role]`prefix` or `suffix`[role]

   - Angle brackets::

         <role>`prefix` or `suffix`<role>

     (The overlap of \*ML tags with angle brackets would be too
     confusing and precludes their use.)

Syntax #3 was chosen for reStructuredText.


Comments
========

A problem with comments (actually, with all indented constructs) is
that they cannot be followed by an indented block -- a block quote --
without swallowing it up.

I thought that perhaps comments should be one-liners only.  But would
this mean that footnotes, hyperlink targets, and directives must then
also be one-liners?  Not a good solution.

Tony Ibbs suggested a "comment" directive.  I added that we could
limit a comment to a single text block, and that a "multi-block
comment" could use "comment-start" and "comment-end" directives.  This
would remove the indentation incompatibility.  A "comment" directive
automatically suggests "footnote" and (hyperlink) "target" directives
as well.  This could go on forever!  Bad choice.

Garth Kidd suggested that an "empty comment", a ".." explicit markup
start with nothing on the first line (except possibly whitespace) and
a blank line immediately following, could serve as an "unindent".  An
empty comment does **not** swallow up indented blocks following it,
so block quotes are safe.  "A tiny but practical wart."  Accepted.


Anonymous Hyperlinks
====================

Alan Jaffray came up with this idea, along with the following syntax::

    Search the `Python DOC-SIG mailing list archives`{}_.

    .. _: http://mail.python.org/pipermail/doc-sig/

The idea is sound and useful.  I suggested a "double underscore"
syntax::

    Search the `Python DOC-SIG mailing list archives`__.

    .. __: http://mail.python.org/pipermail/doc-sig/

But perhaps single underscores are okay?  The syntax looks better, but
the hyperlink itself doesn't explicitly say "anonymous"::

    Search the `Python DOC-SIG mailing list archives`_.

    .. _: http://mail.python.org/pipermail/doc-sig/

Mixing anonymous and named hyperlinks becomes confusing.  The order of
targets is not significant for named hyperlinks, but it is for
anonymous hyperlinks::

    Hyperlinks: anonymous_, named_, and another anonymous_.

    .. _named: named
    .. _: anonymous1
    .. _: anonymous2

Without the extra syntax of double underscores, determining which
hyperlink references are anonymous may be difficult.  We'd have to
check which references don't have corresponding targets, and match
those up with anonymous targets.  Keeping to a simple consistent
ordering (as with auto-numbered footnotes) seems simplest.

reStructuredText will use the explicit double-underscore syntax for
anonymous hyperlinks.  An alternative (see `Reworking Explicit
Markup`_ below) for the somewhat awkward ".. __:" syntax is "__"::

    An anonymous__ reference.

    __ http://anonymous


Reworking Explicit Markup
=========================

Alan Jaffray came up with the idea of `anonymous hyperlinks`_, added
to reStructuredText.  Subsequently it was asserted that hyperlinks
(especially anonymous hyperlinks) would play an increasingly important
role in reStructuredText documents, and therefore they require a
simpler and more concise syntax.  This prompted a review of the
current and proposed explicit markup syntaxes with regards to
improving usability.

1. Original syntax::

       .. _blah:                     internal hyperlink target
       .. _blah: http://somewhere    external hyperlink target
       .. _blah: blahblah_           indirect hyperlink target
       .. __:                        anonymous internal target
       .. __: http://somewhere       anonymous external target
       .. __: blahblah_              anonymous indirect target
       .. [blah] http://somewhere    footnote
       .. blah:: http://somewhere    directive
       .. blah: http://somewhere     comment

   .. Note::

      The comment text was intentionally made to look like a hyperlink
      target.

   Origins:

   * Except for the colon (a delimiter necessary to allow for
     phrase-links), hyperlink target ``.. _blah:`` comes from Setext.
   * Comment syntax from Setext.
   * Footnote syntax from StructuredText ("named links").
   * Directives and anonymous hyperlinks original to reStructuredText.

   Advantages:

   + Consistent explicit markup indicator: "..".
   + Consistent hyperlink syntax: ".. _" & ":".

   Disadvantages:

   - Anonymous target markup is awkward: ".. __:".
   - The explicit markup indicator ("..") is excessively overloaded?
   - Comment text is limited (can't look like a footnote, hyperlink,
     or directive).  But this is probably not important.

2. Alan Jaffray's proposed syntax #1::

       __ _blah                      internal hyperlink target
       __ blah: http://somewhere     external hyperlink target
       __ blah: blahblah_            indirect hyperlink target
       __                            anonymous internal target
       __ http://somewhere           anonymous external target
       __ blahblah_                  anonymous indirect target
       __ [blah] http://somewhere    footnote
       .. blah:: http://somewhere    directive
       .. blah: http://somewhere     comment

   The hyperlink-connoted underscores have become first-level syntax.

   Advantages:

   + Anonymous targets are simpler.
   + All hyperlink targets are one character shorter.

   Disadvantages:

   - Inconsistent internal hyperlink targets.  Unlike all other named
     hyperlink targets, there's no colon.  There's an extra leading
     underscore, but we can't drop it because without it, "blah" looks
     like a relative URI.  Unless we restore the colon::

         __ blah:                      internal hyperlink target

   - Obtrusive markup?

3. Alan Jaffray's proposed syntax #2::

       .. _blah                      internal hyperlink target
       .. blah: http://somewhere     external hyperlink target
       .. blah: blahblah_            indirect hyperlink target
       ..                            anonymous internal target
       .. http://somewhere           anonymous external target
       .. blahblah_                  anonymous indirect target
       .. [blah] http://somewhere    footnote
       !! blah: http://somewhere     directive
       ## blah: http://somewhere     comment

   Leading underscores have been (almost) replaced by "..", while
   comments and directives have gained their own syntax.

   Advantages:

   + Anonymous hyperlinks are simpler.
   + Unique syntax for comments.  Connotation of "comment" from
     some programming languages (including our favorite).
   + Unique syntax for directives.  Connotation of "action!".

   Disadvantages:

   - Inconsistent internal hyperlink targets.  Again, unlike all other
     named hyperlink targets, there's no colon.  There's a leading
     underscore, matching the trailing underscores of references,
     which no other hyperlink targets have.  We can't drop that one
     leading underscore though: without it, "blah" looks like a
     relative URI.  Again, unless we restore the colon::

         .. blah:                      internal hyperlink target

   - All (except for internal) hyperlink targets lack their leading
     underscores, losing the "hyperlink" connotation.

   - Obtrusive syntax for comments.  Alternatives::

         ;; blah: http://somewhere
            (also comment syntax in Lisp & others)
         ,, blah: http://somewhere
            ("comma comma": sounds like "comment"!)

   - Iffy syntax for directives.  Alternatives?

4. Tony Ibbs' proposed syntax::

       .. _blah:                     internal hyperlink target
       .. _blah: http://somewhere    external hyperlink target
       .. _blah: blahblah_           indirect hyperlink target
       ..                            anonymous internal target
       .. http://somewhere           anonymous external target
       .. blahblah_                  anonymous indirect target
       .. [blah] http://somewhere    footnote
       .. blah:: http://somewhere    directive
       .. blah: http://somewhere     comment

   This is the same as the current syntax, except for anonymous
   targets which drop their "__: ".

   Advantage:

   + Anonymous targets are simpler.

   Disadvantages:

   - Anonymous targets lack their leading underscores, losing the
     "hyperlink" connotation.
   - Anonymous targets are almost indistinguishable from comments.
     (Better to know "up front".)

5. David Goodger's proposed syntax: Perhaps going back to one of
   Alan's earlier suggestions might be the best solution.  How about
   simply adding "__ " as a synonym for ".. __: " in the original
   syntax?  These would become equivalent::

       .. __:                        anonymous internal target
       .. __: http://somewhere       anonymous external target
       .. __: blahblah_              anonymous indirect target

       __                            anonymous internal target
       __ http://somewhere           anonymous external target
       __ blahblah_                  anonymous indirect target

Alternative 5 has been adopted.


Backquotes in Phrase-Links
==========================

[From a 2001-06-05 Doc-SIG post in reply to questions from Doug
Hellmann.]

The first draft of the spec, posted to the Doc-SIG in November 2000,
used square brackets for phrase-links.  I changed my mind because:

1. In the first draft, I had already decided on single-backquotes for
   inline literal text.

2. However, I wanted to minimize the necessity for backslash escapes,
   for example when quoting Python repr-equivalent syntax that uses
   backquotes.

3. The processing of identifiers (function/method/attribute/module
   etc. names) into hyperlinks is a useful feature.  PyDoc recognizes
   identifiers heuristically, but it doesn't take much imagination to
   come up with counter-examples where PyDoc's heuristics would result
   in embarassing failure.  I wanted to do it deterministically, and
   that called for syntax.  I called this construct "interpreted
   text".

4. Leveraging off the ``*emphasis*/**strong**`` syntax, lead to the
   idea of using double-backquotes as syntax.

5. I worked out some rules for inline markup recognition.

6. In combination with #5, double backquotes lent themselves to inline
   literals, neatly satisfying #2, minimizing backslash escapes.  In
   fact, the spec says that no interpretation of any kind is done
   within double-backquote inline literal text; backslashes do *no*
   escaping within literal text.

7. Single backquotes are then freed up for interpreted text.

8. I already had square brackets required for footnote references.

9. Since interpreted text will typically turn into hyperlinks, it was
   a natural fit to use backquotes as the phrase-quoting syntax for
   trailing-underscore hyperlinks.

The original inspiration for the trailing underscore hyperlink syntax
was Setext.  But for phrases Setext used a very cumbersome
``underscores_between_words_like_this_`` syntax.

The underscores can be viewed as if they were right-pointing arrows:
``-->``.  So ``hyperlink_`` points away from the reference, and
``.. _hyperlink:`` points toward the target.


Substitution Mechanism
======================

Substitutions arose out of a Doc-SIG thread begun on 2001-10-28 by
Alan Jaffray, "reStructuredText inline markup".  It reminded me of a
missing piece of the reStructuredText puzzle, first referred to in my
contribution to "Documentation markup & processing / PEPs" (Doc-SIG
2001-06-21).

Substitutions allow the power and flexibility of directives to be
shared by inline text.  They are a way to allow arbitrarily complex
inline objects, while keeping the details out of the flow of text.
They are the equivalent of SGML/XML's named entities.  For example, an
inline image (using reference syntax alternative 4d (vertical bars)
and definition alternative 3, the alternatives chosen for inclusion in
the spec)::

    The |biohazard| symbol must be used on containers used to dispose
    of medical waste.

    .. |biohazard| image:: biohazard.png
       [height=20 width=20]

The ``|biohazard|`` substitution reference will be replaced in-line by
whatever the ``.. |biohazard|`` substitution definition generates (in
this case, an image).  A substitution definition contains the
substitution text bracketed with vertical bars, followed by a an
embedded inline-compatible directive, such as "image".  A transform is
required to complete the substitution.

Syntax alternatives for the reference:

1. Use the existing interpreted text syntax, with a predefined role
   such as "sub"::

       The `biohazard`:sub: symbol...

   Advantages: existing syntax, explicit.  Disadvantages: verbose,
   obtrusive.

2. Use a variant of the interpreted text syntax, with a new suffix
   akin to the underscore in phrase-link references::

       (a) `name`@
       (b) `name`#
       (c) `name`&
       (d) `name`/
       (e) `name`<
       (f) `name`::
       (g) `name`:


   Due to incompatibility with other constructs and ordinary text
   usage, (f) and (g) are not possible.

3. Use interpreted text syntax with a fixed internal format::

       (a) `:name:`
       (b) `name:`
       (c) `name::`
       (d) `::name::`
       (e) `%name%`
       (f) `#name#`
       (g) `/name/`
       (h) `&name&`
       (i) `|name|`
       (j) `[name]`
       (k) `<name>`
       (l) `&name;`
       (m) `'name'`

   To avoid ML confusion (k) and (l) are definitely out.  Square
   brackets (j) won't work in the target (the substitution definition
   would be indistinguishable from a footnote).

   The ```/name/``` syntax (g) is reminiscent of "s/find/sub"
   substitution syntax in ed-like languages.  However, it may have a
   misleading association with regexps, and looks like an absolute
   POSIX path.  (i) is visually equivalent and lacking the
   connotations.

   A disadvantage of all of these is that they limit interpreted text,
   albeit only slightly.

4. Use specialized syntax, something new::

       (a) #name#
       (b) @name@
       (c) /name/
       (d) |name|
       (e) <<name>>
       (f) //name//
       (g) ||name||
       (h) ^name^
       (i) [[name]]
       (j) ~name~
       (k) !name!
       (l) =name=
       (m) ?name?
       (n) >name<

   "#" (a) and "@" (b) are obtrusive.  "/" (c) without backquotes
   looks just like a POSIX path; it is likely for such usage to appear
   in text.

   "|" (d) and "^" (h) are feasible.

5. Redefine the trailing underscore syntax.  See definition syntax
   alternative 4, below.

Syntax alternatives for the definition:

1. Use the existing directive syntax, with a predefined directive such
   as "sub".  It contains a further embedded directive resolving to an
   inline-compatible object::

       .. sub:: biohazard
          .. image:: biohazard.png
             [height=20 width=20]

       .. sub:: parrot
          That bird wouldn't *voom* if you put 10,000,000 volts
          through it!

   The advantages and disadvantages are the same as in inline
   alternative 1.

2. Use syntax as in #1, but with an embedded directivecompressed::

       .. sub:: biohazard image:: biohazard.png
          [height=20 width=20]

   This is a bit better than alternative 1, but still too much.

3. Use a variant of directive syntax, incorporating the substitution
   text, obviating the need for a special "sub" directive name.  If we
   assume reference alternative 4d (vertical bars), the matching
   definition would look like this::

       .. |biohazard| image:: biohazard.png
          [height=20 width=20]

4. (Suggested by Alan Jaffray on Doc-SIG from 2001-11-06.)

   Instead of adding new syntax, redefine the trailing underscore
   syntax to mean "substitution reference" instead of "hyperlink
   reference".  Alan's example::

       I had lunch with Jonathan_ today.  We talked about Zope_.

       .. _Jonathan: lj [user=jhl]
       .. _Zope: http://www.zope.org/

   A problem with the proposed syntax is that URIs which look like
   simple reference names (alphanum plus ".", "-", "_") would be
   indistinguishable from substitution directive names.  A more
   consistent syntax would be::

       I had lunch with Jonathan_ today.  We talked about Zope_.

       .. _Jonathan: lj:: user=jhl
       .. _Zope: http://www.zope.org/

   (``::`` after ``.. _Jonathan: lj``.)

   The "Zope" target is a simple external hyperlink, but the
   "Jonathan" target contains a directive.  Alan proposed is that the
   reference text be replaced by whatever the referenced directive
   (the "directive target") produces.  A directive reference becomes a
   hyperlink reference if the contents of the directive target resolve
   to a hyperlink.  If the directive target resolves to an icon, the
   reference is replaced by an inline icon.  If the directive target
   resolves to a hyperlink, the directive reference becomes a
   hyperlink reference.

   This seems too indirect and complicated for easy comprehension.

   The reference in the text will sometimes become a link, sometimes
   not.  Sometimes the reference text will remain, sometimes not.  We
   don't know *at the reference*::

       This is a `hyperlink reference`_; its text will remain.
       This is an `inline icon`_; its text will disappear.

   That's a problem.

The syntax that has been incorporated into the spec and parser is
reference alternative 4d with definition alternative 3::

    The |biohazard| symbol...

    .. |biohazard| image:: biohazard.png
       [height=20 width=20]

We can also combine substitution references with hyperlink references,
by appending a "_" (named hyperlink reference) or "__" (anonymous
hyperlink reference) suffix to the substitution reference.  This
allows us to click on an image-link::

    The |biohazard|_ symbol...

    .. |biohazard| image:: biohazard.png
       [height=20 width=20]
    .. _biohazard: http://www.cdc.gov/

There have been several suggestions for the naming of these
constructs, originally called "substitution references" and
"substitutions".

1. Candidate names for the reference construct:

   (a) substitution reference
   (b) tagging reference
   (c) inline directive reference
   (d) directive reference
   (e) indirect inline directive reference
   (f) inline directive placeholder
   (g) inline directive insertion reference
   (h) directive insertion reference
   (i) insertion reference
   (j) directive macro reference
   (k) macro reference
   (l) substitution directive reference

2. Candidate names for the definition construct:

   (a) substitution
   (b) substitution directive
   (c) tag
   (d) tagged directive
   (e) directive target
   (f) inline directive
   (g) inline directive definition
   (h) referenced directive
   (i) indirect directive
   (j) indirect directive definition
   (k) directive definition
   (l) indirect inline directive
   (m) named directive definition
   (n) inline directive insertion definition
   (o) directive insertion definition
   (p) insertion definition
   (q) insertion directive
   (r) substitution definition
   (s) directive macro definition
   (t) macro definition
   (u) substitution directive definition
   (v) substitution definition

"Inline directive reference" (1c) seems to be an appropriate term at
first, but the term "inline" is redundant in the case of the
reference.  Its counterpart "inline directive definition" (2g) is
awkward, because the directive definition itself is not inline.

"Directive reference" (1d) and "directive definition" (2k) are too
vague.  "Directive definition" could be used to refer to any
directive, not just those used for inline substitutions.

One meaning of the term "macro" (1k, 2s, 2t) is too
programming-language-specific.  Also, macros are typically simple text
substitution mechanisms: the text is substituted first and evaluated
later.  reStructuredText substitution definitions are evaluated in
place at parse time and substituted afterwards.

"Insertion" (1h, 1i, 2n-2q) is almost right, but it implies that
something new is getting added rather than one construct being
replaced by another.

Which brings us back to "substitution".  The overall best names are
"substitution reference" (1a) and "substitution definition" (2v).  A
long way to go to add one word!


Reworking Footnotes
===================

As a further wrinkle (see `Reworking Explicit Markup`_ above), in the
wee hours of 2002-02-28 I posted several ideas for changes to footnote
syntax:

    - Change footnote syntax from ``.. [1]`` to ``_[1]``? ...
    - Differentiate (with new DTD elements) author-date "citations"
      (``[GVR2002]``) from numbered footnotes? ...
    - Render footnote references as superscripts without "[]"? ...

These ideas are all related, and suggest changes in the
reStructuredText syntax as well as the docutils tree model.

The footnote has been used for both true footnotes (asides expanding
on points or defining terms) and for citations (references to external
works).  Rather than dealing with one amalgam construct, we could
separate the current footnote concept into strict footnotes and
citations.  Citations could be interpreted and treated differently
from footnotes.  Footnotes would be limited to numerical labels:
manual ("1") and auto-numbered (anonymous "#", named "#label").

The footnote is the only explicit markup construct (starts with ".. ")
that directly translates to a visible body element.  I've always been
a little bit uncomfortable with the ".. " marker for footnotes because
of this; ".. " has a connotation of "special", but footnotes aren't
especially "special".  Printed texts often put footnotes at the bottom
of the page where the reference occurs (thus "foot note").  Some HTML
designs would leave footnotes to be rendered the same positions where
they're defined.  Other online and printed designs will gather
footnotes into a section near the end of the document, converting them
to "endnotes" (perhaps using a directive in our case); but this
"special processing" is not an intrinsic property of the footnote
itself, but a decision made by the document author or processing
system.

Citations are almost invariably collected in a section at the end of a
document or section.  Citations "disappear" from where they are
defined and are magically reinserted at some well-defined point.
There's more of a connection to the "special" connotation of the ".. "
syntax.  The point at which the list of citations is inserted could be
defined manually by a directive (e.g., ".. citations::"), and/or have
default behavior (e.g., a section automatically inserted at the end of
the document) that might be influenced by options to the Writer.

Syntax proposals:

+ Footnotes:

  - Current syntax::

        .. [1] Footnote 1
        .. [#] Auto-numbered footnote.
        .. [#label] Auto-labeled footnote.

  - The syntax proposed in the original 2002-02-28 Doc-SIG post:
    remove the ".. ", prefix a "_"::

        _[1] Footnote 1
        _[#] Auto-numbered footnote.
        _[#label] Auto-labeled footnote.

    The leading underscore syntax (earlier dropped because
    ``.. _[1]:`` was too verbose) is a useful reminder that footnotes
    are hyperlink targets.

  - Minimal syntax: remove the ".. [" and "]", prefix a "_", and
    suffix a "."::

        _1. Footnote 1.
        _#. Auto-numbered footnote.
        _#label. Auto-labeled footnote.

                 ``_1.``, ``_#.``, and ``_#label.`` are markers,
                 like list markers.

    Footnotes could be rendered something like this in HTML

        \| 1. This is a footnote.  The brackets could be dropped
        |    from the label, and a vertical bar could set them
        |    off from the rest of the document in the HTML.

    Two-way hyperlinks on the footnote marker ("1." above) would also
    help to differentiate footnotes from enumerated lists.

    If converted to endnotes (by a directive/transform), a horizontal
    half-line might be used instead.  Page-oriented output formats
    would typically use the horizontal line for true footnotes.

+ Footnote references:

  - Current syntax::

        [1]_, [#]_, [#label]_

  - Minimal syntax to match the minimal footnote syntax above::

        1_, #_, #label_

    As a consequence, pure-numeric hyperlink references would not be
    possible; they'd be interpreted as footnote references.

+ Citation references: no change is proposed from the current footnote
  reference syntax::

      [GVR2001]_

+ Citations:

  - Current syntax (footnote syntax)::

        .. [GVR2001] Python Documentation; van Rossum, Drake, et al.;
           http://www.python.org/doc/

  - Possible new syntax::

        _[GVR2001] Python Documentation; van Rossum, Drake, et al.;
                   http://www.python.org/doc/

        _[DJG2002]
            Docutils: Python Documentation Utilities project; Goodger
            et al.; http://docutils.sourceforge.net/

    Without the ".. " marker, subsequent lines would either have to
    align as in one of the above, or we'd have to allow loose
    alignment (I'd rather not)::

        _[GVR2001] Python Documentation; van Rossum, Drake, et al.;
            http://www.python.org/doc/

I proposed adopting the "minimal" syntax for footnotes and footnote
references, and adding citations and citation references to
reStructuredText's repertoire.  The current footnote syntax for
citations is better than the alternatives given.

From a reply by Tony Ibbs on 2002-03-01:

    However, I think easier with examples, so let's create one::

        Fans of Terry Pratchett are perhaps more likely to use
        footnotes [1]_ in their own writings than other people
        [2]_.  Of course, in *general*, one only sees footnotes
        in academic or technical writing - it's use in fiction
        and letter writing is not normally considered good
        style [4]_, particularly in emails (not a medium that
        lends itself to footnotes).

        .. [1] That is, little bits of referenced text at the
           bottom of the page.
        .. [2] Because Terry himself does, of course [3]_.
        .. [3] Although he has the distinction of being
           *funny* when he does it, and his fans don't always
           achieve that aim.
        .. [4] Presumably because it detracts from linear
           reading of the text - this is, of course, the point.

    and look at it with the second syntax proposal::

        Fans of Terry Pratchett are perhaps more likely to use
        footnotes [1]_ in their own writings than other people
        [2]_.  Of course, in *general*, one only sees footnotes
        in academic or technical writing - it's use in fiction
        and letter writing is not normally considered good
        style [4]_, particularly in emails (not a medium that
        lends itself to footnotes).

        _[1] That is, little bits of referenced text at the
             bottom of the page.
        _[2] Because Terry himself does, of course [3]_.
        _[3] Although he has the distinction of being
             *funny* when he does it, and his fans don't always
             achieve that aim.
        _[4] Presumably because it detracts from linear
             reading of the text - this is, of course, the point.

    (I note here that if I have gotten the indentation of the
    footnotes themselves correct, this is clearly not as nice.  And if
    the indentation should be to the left margin instead, I like that
    even less).

    and the third (new) proposal::

        Fans of Terry Pratchett are perhaps more likely to use
        footnotes 1_ in their own writings than other people
        2_.  Of course, in *general*, one only sees footnotes
        in academic or technical writing - it's use in fiction
        and letter writing is not normally considered good
        style 4_, particularly in emails (not a medium that
        lends itself to footnotes).

        _1. That is, little bits of referenced text at the
            bottom of the page.
        _2. Because Terry himself does, of course 3_.
        _3. Although he has the distinction of being
            *funny* when he does it, and his fans don't always
            achieve that aim.
        _4. Presumably because it detracts from linear
            reading of the text - this is, of course, the point.

    I think I don't, in practice, mind the targets too much (the use
    of a dot after the number helps a lot here), but I do have a
    problem with the body text, in that I don't naturally separate out
    the footnotes as different than the rest of the text - instead I
    keep wondering why there are numbers interspered in the text.  The
    use of brackets around the numbers ([ and ]) made me somehow parse
    the footnote references as "odd" - i.e., not part of the body text
    - and thus both easier to skip, and also (paradoxically) easier to
    pick out so that I could follow them.

    Thus, for the moment (and as always susceptable to argument), I'd
    say -1 on the new form of footnote reference (i.e., I much prefer
    the existing ``[1]_`` over the proposed ``1_``), and ambivalent
    over the proposed target change.

    That leaves David's problem of wanting to distinguish footnotes
    and citations - and the only thing I can propose there is that
    footnotes are numeric or # and citations are not (which, as a
    human being, I can probably cope with!).

From a reply by Paul Moore on 2002-03-01:

    I think the current footnote syntax ``[1]_`` is *exactly* the
    right balance of distinctness vs unobtrusiveness.  I very
    definitely don't think this should change.

    On the target change, it doesn't matter much to me.

From a further reply by Tony Ibbs on 2002-03-01, referring to the
"[1]" form and actual usage in email:

    Clearly this is a form people are used to, and thus we should
    consider it strongly (in the same way that the usage of ``*..*``
    to mean emphasis was taken partly from email practise).

    Equally clearly, there is something "magical" for people in the
    use of a similar form (i.e., ``[1]``) for both footnote reference
    and footnote target - it seems natural to keep them similar.

    ...

    I think that this established plaintext usage leads me to strongly
    believe we should retain square brackets at both ends of a
    footnote.  The markup of the reference end (a single trailing
    underscore) seems about as minimal as we can get away with.  The
    markup of the target end depends on how one envisages the thing -
    if ".." means "I am a target" (as I tend to see it), then that's
    good, but one can also argue that the "_[1]" syntax has a neat
    symmetry with the footnote reference itself, if one wishes (in
    which case ".." presumably means "hidden/special" as David seems
    to think, which is why one needs a ".." *and* a leading underline
    for hyperlink targets.

Given the persuading arguments voiced, we'll leave footnote & footnote
reference syntax alone.  Except that these discussions gave rise to
the "auto-symbol footnote" concept, which has been added.  Citations
and citation references have also been added.


Auto-Enumerated Lists
=====================

The advantage of auto-numbered enumerated lists would be similar to
that of auto-numbered footnotes: lists could be written and rearranged
without having to manually renumber them.  The disadvantages are also
the same: input and output wouldn't match exactly; the markup may be
ugly or confusing (depending on which alternative is chosen).

1. Use the "#" symbol.  Example::

       #. Item 1.
       #. Item 2.
       #. Item 3.

   Advantages: simple, explicit.  Disadvantage: enumeration sequence
   cannot be specified (limited to arabic numerals); ugly.

2. As a variation on #1, first initialize the enumeration sequence?
   For example::

       a) Item a.
       #) Item b.
       #) Item c.

   Advantages: simple, explicit, any enumeration sequence possible.
   Disadvantages: ugly; perhaps confusing with mixed concrete/abstract
   enumerators.

3. Alternative suggested by Fred Bremmer, from experience with MoinMoin::

       1. Item 1.
       1. Item 2.
       1. Item 3.

   Advantages: enumeration sequence is explicit (could be multiple
   "a." or "(I)" tokens).  Disadvantages: perhaps confusing; otherwise
   erroneous input (e.g., a duplicate item "1.") would pass silently,
   either causing a problem later in the list (if no blank lines
   between items) or creating two lists (with blanks).

   Take this input for example::

       1. Item 1.

       1. Unintentional duplicate of item 1.

       2. Item 2.

   Currently the parser will produce two list, "1" and "1,2" (no
   warnings, because of the presence of blank lines).  Using Fred's
   notation, the current behavior is "1,1,2 -> 1 1,2" (without blank
   lines between items, it would be "1,1,2 -> 1 [WARNING] 1,2").  What
   should the behavior be with auto-numbering?

   Fred has produced a patch__, whose initial behavior is as follows::

       1,1,1   -> 1,2,3
       1,2,2   -> 1,2,3
       3,3,3   -> 3,4,5
       1,2,2,3 -> 1,2,3 [WARNING] 3
       1,1,2   -> 1,2 [WARNING] 2

   (After the "[WARNING]", the "3" would begin a new list.)

   I have mixed feelings about adding this functionality to the spec &
   parser.  It would certainly be useful to some users (myself
   included; I often have to renumber lists).  Perhaps it's too
   clever, asking the parser to guess too much.  What if you *do* want
   three one-item lists in a row, each beginning with "1."?  You'd
   have to use empty comments to force breaks.  Also, I question
   whether "1,2,2 -> 1,2,3" is optimal behavior.

   In response, Fred came up with "a stricter and more explicit rule
   [which] would be to only auto-number silently if *all* the
   enumerators of a list were identical".  In that case::

       1,1,1   -> 1,2,3
       1,2,2   -> 1,2 [WARNING] 2
       3,3,3   -> 3,4,5
       1,2,2,3 -> 1,2 [WARNING] 2,3
       1,1,2   -> 1,2 [WARNING] 2

   Should any start-value be allowed ("3,3,3"), or should
   auto-numbered lists be limited to begin with ordinal-1 ("1", "A",
   "a", "I", or "i")?

   __ http://sourceforge.net/tracker/index.php?func=detail&aid=548802
      &group_id=38414&atid=422032

4. Alternative proposed by Tony Ibbs::

       #1. First item.
       #3. Aha - I edited this in later.
       #2. Second item.

   The initial proposal required unique enumerators within a list, but
   this limits the convenience of a feature of already limited
   applicability and convenience.  Not a useful requirement; dropped.

   Instead, simply prepend a "#" to a standard list enumerator to
   indicate auto-enumeration.  The numbers (or letters) of the
   enumerators themselves are not significant, except:

   - as a sequence indicator (arabic, roman, alphabetic; upper/lower),

   - and perhaps as a start value (first list item).

   Advantages: explicit, any enumeration sequence possible.
   Disadvantages: a bit ugly.


Inline External Targets
=======================

Currently reStructuredText has two hyperlink syntax variations:

* Named hyperlinks::

      This is a named reference_ of one word ("reference").  Here is
      a `phrase reference`_.  Phrase references may even cross `line
      boundaries`_.

      .. _reference: http://www.example.org/reference/
      .. _phrase reference: http://www.example.org/phrase_reference/
      .. _line boundaries: http://www.example.org/line_boundaries/

  + Advantages: 

    - The plaintext is readable.
    - Each target may be reused multiple times (e.g., just write
      ``"reference_"`` again).
    - No syncronized ordering of references and targets is necessary.
  
  + Disadvantages:
  
    - The reference text must be repeated as target names; could lead
      to mistakes.
    - The target URLs may be located far from the references, and hard
      to find in the plaintext.

* Anonymous hyperlinks (in current reStructuredText)::

      This is an anonymous reference__.  Here is an anonymous
      `phrase reference`__.  Phrase references may even cross `line
      boundaries`__.

      __ http://www.example.org/reference/
      __ http://www.example.org/phrase_reference/
      __ http://www.example.org/line_boundaries/

  + Advantages: 

    - The plaintext is readable.
    - The reference text does not have to be repeated.
  
  + Disadvantages:
  
    - References and targets must be kept in sync.
    - Targets cannot be reused.
    - The target URLs may be located far from the references.

For comparison and historical background, StructuredText also has two
syntaxes for hyperlinks:

* First, ``"reference text":URL``::

      This is a "reference":http://www.example.org/reference/
      of one word ("reference").  Here is a "phrase
      reference":http://www.example.org/phrase_reference/.

* Second, ``"reference text", http://example.com/absolute_URL``::

      This is a "reference", http://www.example.org/reference/
      of one word ("reference").  Here is a "phrase reference",
      http://www.example.org/phrase_reference/.

Both syntaxes share advantages and disadvantages:

+ Advantages: 

  - The target is specified immediately adjacent to the reference.

+ Disadvantages:

  - Poor plaintext readability.
  - Targets cannot be reused.
  - Both syntaxes use double quotes, common in ordinary text.
  - In the first syntax, the URL and the last word are stuck
    together, exacerbating the line wrap problem.
  - The second syntax is too magical; text could easily be written
    that way by accident (although only absolute URLs are recognized
    here, perhaps because of the potential for ambiguity).

A new type of "inline external hyperlink" has been proposed.

1. On 2002-06-28, Simon Budig proposed__ a new syntax for
   reStructuredText hyperlinks::

       This is a reference_(http://www.example.org/reference/) of one
       word ("reference").  Here is a `phrase
       reference`_(http://www.example.org/phrase_reference/).  Are
       these examples, (single-underscore), named?  If so, `anonymous
       references`__(http://www.example.org/anonymous/) using two
       underscores would probably be preferable.
 
   __ http://mail.python.org/pipermail/doc-sig/2002-June/002648.html

   The syntax, advantages, and disadvantages are similar to those of
   StructuredText.

   + Advantages: 
   
     - The target is specified immediately adjacent to the reference.
   
   + Disadvantages:
   
     - Poor plaintext readability.
     - Targets cannot be reused (unless named, but the semantics are
       unclear).

   + Problems:

     - The ``"`ref`_(URL)"`` syntax forces the last word of the
       reference text to be joined to the URL, making a potentially
       very long word that can't be wrapped (URLs can be very long). 
       The reference and the URL should be separate.  This is a
       symptom of the following point:

     - The syntax produces a single compound construct made up of two
       equally important parts, *with syntax in the middle*, *between*
       the reference and the target.  This is unprecedented in
       reStructuredText.

     - The "inline hyperlink" text is *not* a named reference (there's
       no lookup by name), so it shouldn't look like one.

     - According to the IETF standards RFC 2396 and RFC 2732,
       parentheses are legal URI characters and curly braces are legal
       email characters, making their use prohibitively difficult.

     - The named/anonymous semantics are unclear.

2. After an analysis__ of the syntax of (1) above, we came up with the
   following compromise syntax::

       This is an anonymous reference__
       __<http://www.example.org/reference/> of one word
       ("reference").  Here is a `phrase reference`__
       __<http://www.example.org/phrase_reference/>.  `Named
       references`_ _<http://www.example.org/anonymous/> use single
       underscores.

   __ http://mail.python.org/pipermail/doc-sig/2002-July/002670.html

   The syntax builds on that of the existing "inline internal
   targets": ``an _`inline internal target`.``

   + Advantages: 

     - The target is specified immediately adjacent to the reference,
       improving maintainability:
  
       - References and targets are easily kept in sync.
       - The reference text does not have to be repeated.

     - The construct is executed in two parts: references identical to
       existing references, and targets that are new but not too big a
       stretch from current syntax.

     - There's overwhelming precedent for quoting URLs with angle
       brackets [#]_.

   + Disadvantages:
  
     - Poor plaintext readability.
     - Lots of "line noise".
     - Targets cannot be reused (unless named; see below).

   To alleviate the readability issue slightly, we could allow the
   target to appear later, such as after the end of the sentence::

       This is a named reference__ of one word ("reference").
       __<http://www.example.org/reference/>  Here is a `phrase
       reference`__.  __<http://www.example.org/phrase_reference/>

   Problem: this could only work for one reference at a time
   (reference/target pairs must be proximate [refA trgA refB trgB],
   not interleaved [refA refB trgA trgB] or nested [refA refB trgB
   trgA]).  This variation is too problematic; references and inline
   external targets will have to be kept imediately adjacent (see (3)
   below).

   The ``"reference__ __<target>"`` syntax is actually for "anonymous
   inline external targets", emphasized by the double underscores.  It
   follows that single trailing and leading underscores would lead to
   *implicitly named* inline external targets.  This would allow the
   reuse of targets by name.  So after ``"reference_ _<target>"``,
   another ``"reference_"`` would point to the same target.

   .. [#]
      From RFC 2396 (URI syntax):

          The angle-bracket "<" and ">" and double-quote (")
          characters are excluded [from URIs] because they are often
          used as the delimiters around URI in text documents and
          protocol fields.
   
          Using <> angle brackets around each URI is especially
          recommended as a delimiting style for URI that contain
          whitespace.
   
      From RFC 822 (email headers):
   
          Angle brackets ("<" and ">") are generally used to indicate
          the presence of a one machine-usable reference (e.g.,
          delimiting mailboxes), possibly including source-routing to
          the machine.

3. If it is best for references and inline external targets to be
   immediately adjacent, then they might as well be integrated.
   Here's an alternative syntax embedding the target URL in the
   reference::

       This is an anonymous `reference <http://www.example.org
       /reference/>`__ of one word ("reference").  Here is a `phrase
       reference <http://www.example.org/phrase_reference/>`__.

   Advantages and disadvantages are similar to those in (2).
   Readability is still an issue, but the syntax is a bit less
   heavyweight (reduced line noise).  Backquotes are required, even
   for one-word references; the target URL is included within the
   reference text, forcing a phrase context.

   We'll call this variant "embedded URIs".

   Problem: how to refer to a title like "HTML Anchors: <a>" (which
   ends with an HTML/SGML/XML tag)?  We could either require more
   syntax on the target (like ``"`reference text
   __<http://example.com/>`__"``), or require the odd conflicting
   title to be escaped (like ``"`HTML Anchors: \<a>`__"``).  The
   latter seems preferable, and not too onerous.

   Similarly to (2) above, a single trailing underscore would convert
   the reference & inline external target from anonymous to implicitly
   named, allowing reuse of targets by name.

   I think this is the least objectionable of the syntax alternatives.

Other syntax variations have been proposed (by Brett Cannon and Benja
Fallenstein)::

    `phrase reference`->http://www.example.com

    `phrase reference`@http://www.example.com

    `phrase reference`__ ->http://www.example.com

    `phrase reference` [-> http://www.example.com]

    `phrase reference`__ [-> http://www.example.com]

    `phrase reference` <http://www.example.com>_

None of these variations are clearly superior to #3 above.  Some have
problems that exclude their use.

With any kind of inline external target syntax it comes down to the
conflict between maintainability and plaintext readability.  I don't
see a major problem with reStructuredText's maintainability, and I
don't want to sacrifice plaintext readability to "improve" it.

The proponents of inline external targets want them for easily
maintainable web pages.  The arguments go something like this:

- Named hyperlinks are difficult to maintain because the reference
  text is duplicated as the target name.

  To which I said, "So use anonymous hyperlinks."

- Anonymous hyperlinks are difficult to maintain becuase the
  references and targets have to be kept in sync.

  "So keep the targets close to the references, grouped after each
  paragraph.  Maintenance is trivial."

- But targets grouped after paragraphs break the flow of text.

  "Surely less than URLs embedded in the text!  And if the intent is
  to produce web pages, not readable plaintext, then who cares about
  the flow of text?"

Many participants have voiced their objections to the proposed syntax:

    Garth Kidd: "I strongly prefer the current way of doing it.
    Inline is spectactularly messy, IMHO."

    Tony Ibbs: "I vehemently agree... that the inline alternatives
    being suggested look messy - there are/were good reasons they've
    been taken out...  I don't believe I would gain from the new
    syntaxes."

    Paul Moore: "I agree as well.  The proposed syntax is far too
    punctuation-heavy, and any of the alternatives discussed are
    ambiguous or too subtle."

Others have voiced their support:

    fantasai: "I agree with Simon.  In many cases, though certainly
    not in all, I find parenthesizing the url in plain text flows
    better than relegating it to a footnote."

    Ken Manheimer: "I'd like to weigh in requesting some kind of easy,
    direct inline reference link."

(Interesting that those *against* the proposal have been using
reStructuredText for a while, and those *for* the proposal are either
new to the list ["fantasai", background unknown] or longtime
StructuredText users [Ken Manheimer].)

I was initially ambivalent/against the proposed "inline external
targets".  I value reStructuredText's readability very highly, and
although the proposed syntax offers convenience, I don't know if the
convenience is worth the cost in ugliness.  Does the proposed syntax
compromise readability too much, or should the choice be left up to
the author?  Perhaps if the syntax is *allowed* but its use strongly
*discouraged*, for aesthetic/readability reasons?

After a great deal of thought and much input from users, I've decided
that there are reasonable use cases for this construct.  The
documentation should strongly caution against its use in most
situations, recommending independent block-level targets instead.
Syntax #3 above ("embedded URIs") will be used.


Doctree Representation of Transitions
=====================================

(Although not reStructuredText-specific, this section fits best in
this document.)

Having added the "horizontal rule" construct to the `reStructuredText
Markup Specification`_, a decision had to be made as to how to reflect
the construct in the implementation of the document tree.  Given this
source::

    Document
    ========

    Paragraph 1

    --------

    Paragraph 2

The horizontal rule indicates a "transition" (in prose terms) or the
start of a new "division".  Before implementation, the parsed document
tree would be::

    <document>
        <section name="document">
            <title>
                Document
            <paragraph>
                Paragraph 1
            --------               <--- error here
            <paragraph>
                Paragraph 2

There are several possibilities for the implementation:

1. Implement horizontal rules as "divisions" or segments.  A
   "division" is a title-less, non-hierarchical section.  The first
   try at an implementation looked like this::

       <document>
           <section name="document">
               <title>
                   Document
               <paragraph>
                   Paragraph 1
               <division>
                   <paragraph>
                       Paragraph 2

   But the two paragraphs are really at the same level; they shouldn't
   appear to be at different levels.  There's really an invisible
   "first division".  The horizontal rule splits the document body
   into two segments, which should be treated uniformly.

2. Treating "divisions" uniformly brings us to the second
   possibility::

       <document>
           <section name="document">
               <title>
                   Document
               <division>
                   <paragraph>
                       Paragraph 1
               <division>
                   <paragraph>
                       Paragraph 2

   With this change, documents and sections will directly contain
   divisions and sections, but not body elements.  Only divisions will
   directly contain body elements.  Even without a horizontal rule
   anywhere, the body elements of a document or section would be
   contained within a division element.  This makes the document tree
   deeper.  This is similar to the way HTML_ treats document contents:
   grouped within a ``<body>`` element.

3. Implement them as "transitions", empty elements::

       <document>
           <section name="document">
               <title>
                   Document
               <paragraph>
                   Paragraph 1
               <transition>
               <paragraph>
                   Paragraph 2

   A transition would be a "point element", not containing anything,
   only identifying a point within the document structure.  This keeps
   the document tree flatter, but the idea of a "point element" like
   "transition" smells bad.  A transition isn't a thing itself, it's
   the space between two divisions.  However, transitions are a
   practical solution.

Solution 3 was chosen for incorporation into the document tree model.

.. _HTML: http://www.w3.org/MarkUp/


..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
