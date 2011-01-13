The following is a list of ideas of functionality which would be nice
to have in `rst.el`. In the examples a ``*`` stands for the cursor.

Convert to id
=============

* Convert the region to an HTML id

  * For instance "Eine Überschrift" to "eine-berschrift"

  * According the same rules as reST does this

Jump to internal target
=======================

* A command to jump to the internal target the point is on

* A target may be

  * A section title

  * Footnotes / citations

  * Inline internal targets

  * Hyperlink target definition

  * Substitution definition

* See hunk #26 in `rst_el-emacs_V23_1_patch1_1_2` vs. `emacs_V23_1`
  for some ideas

Completion for directive options
================================

* Imagine ::

    .. list-table::
       :*

  with the cursor at the asterisk

* There should be a command which offers all the possible options for
  this particular directive as completion

Completion for directives
=========================

* Imagine ::

    .. *

* There should be a command which offers all directives as completion

* May be this should work for other keywords as well

* May be this could work even at the beginning of the line

* Completion must be bound to M-TAB

  * Already existing binding must be chained

  * May be `expand.el` can help (look in package finder)?

  * May be `hippie` is good here

  * Check `(info)autotype`

Completion for user-defined elements
====================================

* Imagine ::

    |*

  or ::

    [*

  or ::

    _*

* There should be a command which offers all defined substitutions /
  footnotes / links as completion

Insertion of link alias
=======================

* Imagine ::

    Aspect of something
    ===================

    This is about the `aspect of something`_*

* There should be a command which asks you for an alias for the link,
  add the alias and change the link ::

    .. _aspects of something:

    Aspect of something
    ===================

    This is about the `aspects of something`_*

Smart use of `iimage-mode`
==========================

* There is `iimage-mode` which shows ``.. image::``\s in Emacs

* May be we can add a binding to toggle it

TOC in speedbar
===============

* If the TOC is displayed in the speedbar this could be used for
  permanent navigation

toc-mode without markup
=======================

* The markup which may be contained in a section title is not useful
  in toc-mode and should be suppressed

Sophisticated navigation in sections
====================================

* Navigation in sections similar to navigation in other structured data

  * Like XML, Lisp

  * C-M-u für Up

  * C-M-d für Down

  * C-M-f / C-M-b für Forward / Backward

Display of current location
===========================

* Display the "section path" to the current point

* Like in XML: In which element is the point?

toc-mode only to a certain level
================================

* If a TOC buffer is created a prefix argument should limit the depth
  of the listing to the given level

Imenu support
=============

* Imenu could be supported

  * See `(elisp)Imenu`

Outline support
===============

* Support for `outline-mode' / `allout-mode' would be nice

  * Should consider section titles

* May be folding is also possible

  * For item lists
