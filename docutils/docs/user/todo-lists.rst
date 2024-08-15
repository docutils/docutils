.. include:: ../header.rst

===================
Docutils TODO lists
===================

TODO lists allow you to create a list of items with checkboxes.
In `extended Markdown`_, they are called `task lists`.

Checkbox variants
=================

[x] ASCII-art checkbox.

[ ] ASCII space character and NBSP are smaller than the x.

[ ] The "figure space" has the correct width but is not easy to type.

You may define substitutions for ballot box and checked ballot box or
other suitable Unicode characters:

.. |-| unicode:: U+2610
.. |x| unicode:: U+2611
.. |y| unicode:: U+1F5F9

|-| U+2610 BALLOT BOX

|x| U+2611 BALLOT BOX WITH CHECK

|y| U+1F5F9 BALLOT BOX WITH BOLD CHECK

List Markup
===========

Paragraphs
----------

|x| Simple *paragraphs* are easy for small lists with short values

|-| but not well suited for complex TODO items.

Line Blocks
-----------

| |x| Line blocks are rendered as "unstyled" lists.
| |y| They don't need additional styling.
| |-| However, you cannot nest block elements.


Description Lists
-----------------

.. class:: description

|x|
    A *description list* works out of the box in HTML5 and XeTeX.

|-|
    It looks suboptimal in the rST source (definition list with class
    value "description").

[x]
    Lists may use ASCII-art or substitutions.

[ ]
    All list markup variants require special styling based on a
    preceding class__ directive.

__ https://docutils.sourceforge.io/docs/ref/rst/directives.html#class

Bullet Lists
------------

.. class:: todo

- |x| bullet lists (similar to the Markdown_ for `task
  lists`) can be styled accordingly.
- |-| They don't look good in the rST source.
- [x] Lists may use ASCII-art or substitutions.
- [ ] The "figure space" has the correct width.

- |x| Another idea: use bullet lists with ``+`` and ``-`` markers.

      + Clean and simple markup in the source.

      - A new marker character starts a new list :-(

      - Requires change to the writer: Pass the "bullet" attribute to
        the output document (use `HTML5 "data-" attriibutes`__?).

__ https://html.spec.whatwg.org/#embedding-custom-non-visible-data-with-the-data-*-attributes

Field Lists
-----------

.. class:: todo

:|x|: Compile this example with ``rst2html5``,
:|y|: compare markup variants,
:|-|: select the best.

.. class:: todo monospace

:[x]: ASCII-art checkbox.
:[ ]: The "figure space" has the correct width.
:[ ]: CSS styling can switch to monospace fonts for the ASCII-art boxes,
      so an ASCII space character can be used.

.. class:: todo monospace brackets

:x: ASCII-art checkbox - brackets added by CSS.
: : Simple rST source, easy typing.

.. class:: todo monospace framed

:x: checkbox border added by CSS.
: : Simple rST source, easy typing.

I recommend a field list with class argument and some CSS styling.


.. _Markdown: https://www.markdownguide.org
.. _extended Markdown: https://www.markdownguide.org/extended-syntax/


.. ! The follwing CSS rules are included here as raw HTML to keep the
     example compact. They compromise the HTML validity, as styling is not
     allowed in the document body.
     For production use, place CSS styling rules in a custom style sheet.

.. raw:: html

   <style type="text/css"><!--
    ul.todo {list-style-type: none;
             padding-left: 0;}
    ul.todo > li > p {padding-left: 1.2em;
                      text-indent: -1.2em;}

    dl.todo > dt {font-style: normal}
    dl.todo > dt > .colon {display: none}
    dl.todo > dd {margin-left: 2.5em;}

    dl.monospace > dt {font-family: monospace;}

    dl.brackets > dt:before {content: '[';}
    dl.brackets > dt:after {content: ']';}

    dl.framed > dt {border: solid;
                    border-width: thin;
                    padding: 0 0.2em;
                    margin-top: 0.2em;
                    line-height: 0.9;
                   }

    --></style>
