.. [4] manually numbered
.. [#exi] autonumber-label
.. [#] auto-numbered (skipping 2)
.. [#] autonumber label (skipping 4_ and 5_)
.. [*] auto-symbol
.. [*] second auto-symbol

Numbered footnotes [#exi]_ [#]_ [5]_ may be referenced more than once
and also via hyperlink references:

.. class:: run-in

manually numbered [4]_
  reference name is the specified number: 4_,

auto-number [3]_
  reference name is the auto-assigned number: 3_
  (caution: the assigned number may change if another auto-numbered
  footnote is added or removed),

autonumber label [#exi]_
  refname is the label (without #) *not* the number: exi_
  (both ``1_`` and ``[1]_`` fail).


Auto-symbol footnotes [*]_ [*]_ can only be referenced once and only
with a footnote-reference.

The markup characters for auto-labeled footnotes _`*` and _`#` and
the symbols selected by auto-symbol footnotes _`†` don't become
reference names and can be used in other hyperref targets allowing
links to `*`_, `#`_ and `†`_.


4
=======

An implicit target with conflicting refname (like this section) is
overwritten by manually numbered footnotes [4]_ and footnotes with
autonumber-label [#exi]_. An INFO is generated.
Hyperlink references to 4_ and exi_ point to the footnotes, not the
sections.

exi
---

Explicit targets conflicting with manually numbered footnotes (``_`4```)
or footnotes with autonumber-label (``_`exi```) .

5
=======

Both, explicit and implicit targets with conflicting refname (e.g. _`2` and
this section title) cause a gap in footnote numbering.

The refname can still be used in a footnote reference [2]_: it refers to
the target whatever it is. [5]_

TODO:
  Emit a warning if a footnote referenc references something else?
