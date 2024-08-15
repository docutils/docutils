Special Features of the `tuftig.css` Stylesheet
===============================================

``tuftig.css`` is a CSS3_ style sheet for the output of Docutils'
HTML5 writer. The rules are inspired by [tufte.css]_ and
[tufte-latex]_ going back to Edward Tufte's layout design.

.. [tufte.css] Dave Liepmann, `Tufte CSS`,
   https://edwardtufte.github.io/tufte-css/.
.. [tufte-latex] Bil Kleb, Bill Wood and Kevin Godby,
                 `A Tufte-Style Book`,
                 https://www.ctan.org/pkg/tufte-latex.

.. _CSS3: http://www.w3.org/TR/CSS3


Fullwidth and Margin Objects
----------------------------

.. class:: fullwidth

Block elements (paragraphs, admonitions, topics, figures, tables, ...)
with the "fullwidth" class argument use full text width.


.. table:: A fullwidth table with LaTeX math commands
  :class: numbered colwidths-auto fullwidth 

  ========= ===========  ========= ===========  ============= ================
  `\arccos` ``\arccos``  `\gcd`    ``\gcd``     `\Pr`         ``\Pr``
  `\arcsin` ``\arcsin``  `\hom`    ``\hom``     `\projlim`    ``\projlim``
  ========= ===========  ========= ===========  ============= ================

.. figure:: ../../../docs/user/rst/images/title.svg
   :alt: reStructuredText, the markup syntax
   :width: 90%
   :height: 1.5em
   :figclass: numbered fullwidth

   A numbered fullwidth figure.

Block elements (admonitions, figures, tables, ...) with the
"marginal" class argument are set in the right margin (if place permits).

.. class:: marginal

An ordinary paragraph with the "marginal" class argument.

Tight integration of graphics with text is central to Tufte’s work
even when those graphics are ancillary to the main body of a text. In
many of those cases, a margin figure may be most appropriate.

.. figure:: ../../../docs/user/rst/images/biohazard.png
   :figclass: marginal numbered
   :width: 2em

   This is a marginal figure.

   This is the legend.

To place an image in the margin, use a marginal figure without caption.

.. figure:: ../../../docs/user/rst/images/biohazard.png
   :figclass: marginal
   :width: 2em

Marginal objects are placed to the right of the preceding main text
block.

.. Note:: This is a "note" type admonition with a block-quote inside.
   :class: marginal

      This is a silly text that is only there to
      demonstrate line wrapping.

By default, citations and footnotes are set in the margin.
To have them in the main text area (like [Testbook]_ and [tb98]_ here),
use the "align-left" class value.

.. class:: align-left

.. [Testbook] John Ex Ample, `How to test web pages`, Ontario, 1978.

.. class:: align-left language-de

.. [tb98] Horst Schramm, `Docutils 0.5`, in Testberichte III,
   Leipzig, 1998.

.. table:: A marginal table
   :widths: auto
   :class: marginal

   ======= ======= ==========
   A       B       A or B
   ======= ======= ==========
   False   False   False
   True    False   True
   False   True    True
   True    True    True
   ======= ======= ==========

.. class:: align-left

.. [#not-in-margin] The "align-left" class value ensures this footnote is set
   in the main text area.

.. footer:: |HTML 5| |validator| |valid-CSS2|

.. |HTML 5| image:: http://www.w3.org/html/logo/badge/html5-badge-h-css3-semantics.png
   :height: 31
   :width: 88
   :alt: Conforms to HTML 5
   :target: http://www.w3.org/TR/html5/

.. |validator| image:: https://www.w3.org/Icons/ValidatorSuite/vs-blue-190.png
   :height: 31
   :width: 88
   :alt: Check validity!
   :target: http://validator.w3.org/check?uri=referer

.. |valid-CSS2| image:: http://jigsaw.w3.org/css-validator/images/vcss
   :height: 31
   :width: 88
   :alt: Valid CSS 2.1!
   :target: http://jigsaw.w3.org/css-validator/check/referer
