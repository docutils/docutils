================
A document title
================

-------------------
A document subtitle
-------------------

:Author: Stefan Merten

.. header:: The document header is once only.

.. footer:: And the document footer.

PCDATA
======

Some elements may contain only #PCDATA. They need to propagate changes
up. These are the references of [CITA]_ and footnotes [1]_.

Only once only
==============

Other elements may occur only once and thus need to propagate their
changes up or down.

* Titles and subtitles of any kind

  See this section and document title / subtitle

* Transitions

  They have no children which could change.

* Parts of definition lists

  Defined term
    Explanation

  Unknown term
    Definition of term is also only once

* Parts of field lists

  :Only once: Some content

  :Another field: Field body is also only once

* The attribution of a citation

    To be or not to be!

    -- Mozart

* Parts of a figure

  .. figure:: image.gif

     The caption of a figure

     And its legend

* Several parts of a table

  +-----+-----+
  |Left |Right|
  +=====+=====+
  |Row  |Entry|
  |     |only |
  |     |once |
  +-----+-----+

  Also colspec, thead and tbody but they are ignored here.

PCDATA *and* only once
======================

Some elements are of both types.

--same			Some old description - still description may
  			appear only once

--option        	A changing option_string must propagate to option

-o, --option_group	The option_group and the description may also appear
  			only once

.. [CITA] The label of a citation.

.. [1] The label of a footnote.
