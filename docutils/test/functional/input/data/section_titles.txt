Section heading levels
======================

Level 1
=======
Nested sections

Level 2
-------
reach at some level

Level 3
```````
(depending on the document class and output format)

level 4
^^^^^^^
a level

level 5
+++++++
that is not supported by the output format.

level 6
:::::::
Unsupported in LaTeX and HTML5
(HTML5 reserves the 1st level for the document title).

level 7
#######
Unsupported in HTML4.

level 8
<<<<<<<
Unsupported in ODT.

.. _references:

Section titles with inline markup
==================================

*emphasized*, H\ :sub:`2`\ O, :math:`x^2`, and references_
----------------------------------------------------------

Substitutions |fail|
--------------------
.. |fail| replace:: work

Note, that the "reference name" for this section is derived from the
content *before* substitution. You can link to it with the `phrase
reference`_ "`substitutions fail`_".
This behaviour may be exploited to get intelligible IDs after `identifier
normalization`_ of the section's reference name.

.. _identifier normalization: https://docutils.sourceforge.io/docs/ref/rst/
                              directives.html#identifier-normalization
.. _phrase reference: https://docutils.sourceforge.io/docs/ref/rst/
                      restructuredtext.html#hyperlink-references
