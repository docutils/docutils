==============================
Each TITLE may occur only once
==============================

-----------------
Subtitles as well
-----------------

:Organization: Should propagate to docinfo

.. header:: All parts of decoration may occur singly.

PCDATA
======

Some elements may contain only #PCDATA. They need to propagate changes
up. These are the references of [CITATION]_ and footnotes [2]_.

Only once only
==============

Other elements may occur only once and thus need to propagate their
changes up or down.

* Titles and subtitles of any kind

  See this section and document title / subtitle

* Transitions

  They have no children which could change.

* Parts of definition lists

  Need to know
    Explanation

  Unknown term
    Term becomes known

* Parts of field lists

  :Single: Some content

  :Another field: Body of a field.

* The attribution of a citation

    To be or not to be!

    -- Shakespeare

* Parts of a figure

  .. figure:: image.gif

     Picture title

     Legendary!

     May consist of multiple paragraphs.

* Several parts of a table

  +-----+-------+
  |Left |Right  |
  +=====+=======+
  |Row  |Changed|
  |     |entry  |
  +-----+-------+

  Also colspec, thead and tbody but they are ignored here.

PCDATA *and* only once
======================

Some elements are of both types.

--same		A NEW DESCRIPTION

--changed       A changing option_string must propagate to option

-g, --group	THE DESCRIPTION AS WELL AS THE OPTION_GROUP MUST BE SINGLE.

.. [CITATION] The label of a citation.

.. [2] The label of a footnote.
