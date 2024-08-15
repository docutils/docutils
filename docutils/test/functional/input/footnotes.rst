Test footnote and citation rendering
************************************

Paragraphs may contain footnote references (manually numbered
[1]_, anonymous auto-numbered [#]_, labeled auto-numbered [#label]_, or
symbolic [*]_) or citation references ([CIT2002]_, [DU2015]_).

.. include:: data/standard.rst
   :start-after: Footnotes
                 ---------
   :end-before: Here's a reference to the above, [CIT2002]_,

.. [DU2015] `Example document`, Hometown: 2015.

Here's a reference to the above, [CIT2002]_.

.. [5] this footnote is missing in the standard example document.

Footnotes may contain block elements like lists  [#]_ [#list-note]_ [#]_,
admonitions [#]_, or tables [#]_.

.. [#] #. An ordered list
       #. in a footnote.

.. [#list-note] * An unordered list (bullet list)
                * in a footnote.

                And a trailing paragraph.

.. [#] :Field: list
       :with: 2 items.

.. [#] .. note:: This is a note in a note.

.. [#] =====  =========
       a      table
       in a   footnote
       =====  =========

This [#list-note]_ is a second reference to the footnote containing
a bullet. list.
