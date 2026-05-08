Test features of rST footnotes
==============================

*Numbered* footnotes [#exi]_ [#]_ [4]_ may be referenced more than
once and also via hyperlink references:


*Symbol* footnotes [*]_ do not have a reference name.
They can only be referenced once and only with a footnote-reference.

.. table:: Numbered footnotes [#tab]_
   :width: 100%

   ========================  ===============================================
   autonumber label [#exi]_  Reference name is the *label* (exi_),
                             not the number (both ``1_`` and ``[1]_`` fail).

   auto-numbered [3]_        Reference name is the Docutils-assigned
                             number (3_). [#unstable]_ [#latex]_

   manually numbered [4]_    Reference name is the specified number
                             (4_). [#latex]_
   ========================  ===============================================

.. [4] manually numbered footnote
.. [#exi] autonumber-label footnote
.. [#] auto-numbered footnote (skipping 2)
.. [*] auto-symbol footnote [#]_ [*]_
.. [#] auto-numbered footnote referenced from the first auto-symbol footnote
       (skipping 4_ and 5_)
.. [*] auto-symbol footnote referenced from the first auto-symbol footnote
.. [#sec] autonumber-label footnote referenced from a section title
.. [#tab] autonumber-label footnote referenced from a table title
   (missing with "latex-footnotes").
.. [#unstable]
   .. caution:: The assigned number may change if numbered
                footnotes are added, removed or rearranged.
.. [#latex] Numbers assigned by LaTeX may differ from the
   numbers assigned manually or by Docutils.
.. [#] auto-number footnote referenced from admonition title
.. [*] auto-symbol footnote referenced from admonition text


Name Conflicts [#sec]_
======================

4
-

An *implicit* target with conflicting refname (like this section and the
next) is overwritten by manually numbered footnotes [4]_ and footnotes
with autonumber-label [#exi]_.  An INFO is generated.

Hyperlink references to 4_ and exi_ point to the footnotes, not the
sections.

exi
---

Numbers of manually numbered footnotes and autonumber-labels
cannot be used in *explicit* targets:
``_`4``` and ``_`exi``` would lead to a "duplicate target name" ERROR .

The markup characters for auto-labeled footnotes "_`*`" and "_`#`" and
the symbols selected by auto-symbol footnotes "_`†`" do not become
reference names. They can be used in other hyperref targets allowing
links to `*`_, `#`_ and `†`_.

5
-

Both, explicit and implicit targets with a number as reference name (e.g.
the inline target "_`2`" and the section title "5" above) cause a gap in
footnote auto-numbering. [#latex]_  The number can be used in a
footnote-reference although it does not refer to a footnote! [5]_ [2]_

.. admonition:: TODO [#]_

  Emit a warning if a footnote-reference refers to something else? [*]_
