Test footnote and citation rendering
************************************

Paragraphs may contain footnote references (manually numbered
[1]_, anonymous auto-numbered [#]_, labeled auto-numbered [#label]_, or
symbolic [*]_) or citation references ([CIT2002]_, [DU2015]_).

.. include:: data/standard.rst
   :start-after: Footnotes
                 ---------
   :end-before: Citations
                ---------

.. [CIT2002] Citations are text-labeled footnotes. They may be
   rendered separately and differently from footnotes.

.. [DU2015] `Example document`, Hometown: 2015.


Body elements in footnotes [#note-note]_
----------------------------------------

Footnotes may contain block elements like lists  [#]_ [#list-note]_ [#]_,
admonitions [#note-note]_, or tables [#]_.

.. [#] #. An ordered list
       #. in a footnote.

.. [#list-note] * An unordered list (bullet list)
                * in a footnote.

                And a trailing paragraph.

.. [#] :Field: list
       :with: 2 items.

.. [#note-note] .. note:: This is a note in a note.

.. [#] =====  =========
       a      table
       in a   footnote
       =====  =========

.. [5] This footnote is missing in the standard example document.

This [#list-note]_ is a second reference to the footnote containing
a bullet list.

A second reference to [CIT2002]_.
