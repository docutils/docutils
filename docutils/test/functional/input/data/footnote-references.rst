Test features of rST footnotes [#top]_
======================================

*Numbered* footnotes [#exi]_ [#]_ [4]_ [#]_ may be referenced more than
once and also via hyperlink references:

autonumber label [#exi]_
  Reference name is the *label* (without #),
  exi_, not the number (both ``1_`` and ``[1]_`` fail).

manually numbered [4]_
  Reference name is the specified number, 4_.

auto-numbered [3]_
  Reference name is the auto-assigned number, 3_
  (caution: the assigned number may change if another
  auto-numbered footnote is added or removed).

*Symbol* footnotes [*]_ [*]_ do not have a reference name.
They can only be referenced once and only with a footnote-reference.

The markup characters for auto-labeled footnotes "_`*`" and "_`#`" and
the symbols selected by auto-symbol footnotes "_`†`" do not become
reference names. (They can be used in other hyperref targets allowing
links to `*`_, `#`_ and `†`_.)

.. [4] manually numbered
.. [#exi] autonumber-label
.. [#] auto-numbered (skipping 2)
.. [#] second auto-numbered (skipping 4_ and 5_)
.. [*] auto-symbol with nested footnotes [#]_ [*]_
.. [*] second auto-symbol
.. [#] referenced from the first auto-symbol footnote
.. [*] referenced from the first auto-symbol footnote, too
.. [#top] referenced from a section title

4
===

An *implicit* target with conflicting refname (like this section) is
overwritten by manually numbered footnotes [4]_ and footnotes with
autonumber-label [#exi]_. An INFO is generated.

Hyperlink references to 4_ and exi_ point to the footnotes, not the
sections.

exi
---

Numbers of manually numbered footnotes and autonumber-labels
cannot be used in *explicit* targets:
``_`4``` and ``_`exi``` lead to a "duplicate target name" ERROR .

5
===

Both, explicit and implicit targets with a number as reference name
(e.g. the inline target "_`2`" and the section title "5" above)
cause a gap in footnote auto-numbering. The number can be used in a
footnote-reference although it does not refer to a footnote! [5]_ [2]_

TODO:
  Emit a warning if a footnote-reference refers to something else?
