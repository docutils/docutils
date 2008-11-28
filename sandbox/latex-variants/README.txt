..                            -*- rst-mode -*-

=====================
Latex Writer Variants
=====================

:Author: Günter Milde <milde@users.berlios.de>
:Date: $Date$

This sandbox project is a place to try out and discuss variants of the
latex2e writer. Latex writer variants in this project are

#. enhancements or improvements of the latex2e writer, or

#. alternative latex writers for special needs.

.. contents::

``latex2e`` writer extensions
*****************************

Questionnaire
=============

1. Do you use custom `LaTeX style sheets`_ with Docutils-generated LaTeX
   files?

2. Would you prefer the style sheet to be a LaTeX package or inclusion?

3. Did you ever use `custom roles`_ and realise that the exported
   LaTeX code does not compile?

4. Did you ever use `custom roles`_, ``myclass``, say, and define a
   ``\docutilsrole<myclassname>`` LaTeX command for it?

5. If YES, how important is backwards compatibility?
   Would you update your style-sheets if the interface improves
   significantly but non-compatible?

6. Which `default font`_ do you prefer for the output?

7. Which default `font encoding`_ do you prefer for the output?

   Did you have problems with missing ``aeguill.sty`` package?
   
   Did you have problems with Find or Export of words with
   non-ASCII chars (e.g. Umlauts) in the PDF reader (Acrobat, xpdf, ...)

#. Feature wishes or ideas of improvement for the `latex2e` writer.

Users of ``rst2latex2.py`` are encouraged to respond by mail to the
docutils-users list.


LaTeX style sheets
==================

Would you prefer the style sheet to be a LaTeX *package* or *inclusion*?

  No preference for .sty or .tex.

  Maybe it would be useful to have both stylesheets (``*.sty``) and tex
  snippets.

Comparison
----------

LaTeX package: ``<name>.sty``
  Input with ``\usepackage{<name>}``, expects the file extension ``.sty``.

  +2  Existing LaTeX packages can be used as --stylesheet argument.

  +1  Command names can contain the char @ (no \makeatletter needed).

  -1  Warnings, if home-made style sheets do not use the proper LaTeX
      package commands.

  +1  Clear distinction between self-contained, complete LaTeX
      documents and style sheets (cf. ``*.html`` vs. ``*.css``).

  -1  not backwards compatible.

LaTeX inclusion: ``<name>.tex``
  Input with ``\input{<name>}`` or ``\input{<name.tex>}``.
  Expects file extension ".tex".

  -1  Some existing LaTeX packages fail if given as ``--stylesheet`` argument
      (as they must be called via \usepackage, e.g. in a home-made custom
      style sheet).

  -0  Command names with char @ must be wrapped in ``\makeatletter``
      ``\makeatother``.

  +1  Backwards compatible.

Proposal
--------

"intelligent" handling: use ``\usepackage`` if style sheet has ``.sty`` or no
extension and ``\input`` else

Rationale:
  while ``\input`` works with extension as well as without extension,
  ``\usepackage`` expects the package name without extension. (The latex2e
  writer will drop a ``.sty`` extension to be more liberal.)

::

  if stylesheet:
     (stylesheet, styleformat) = os.path.splitext(stylesheet)
     if styleformat in ["", ".sty"]::
         self.head_prefix.append(r"\usepackage{%s}" % (stylesheet))
     else
         self.head_prefix.append(r"\input{%s}" % (stylesheet))

Examples::

  rst2latex.py --stylesheet==mathptmx      # use mathptmx.sty package

  rst2latex.py --stylesheet==mathptmx.sty  # use mathptmx.sty package

  rst2latex.py --stylesheet==preamble.tex  # input preamble.tex

  rst2latex.py --stylesheet==preamble.mystyle  # input preamble.mystyle

Put this in a loop over all arguments, so that ::

  rst2latex.py --stylesheet==mathptmx,parskip,preamble.tex

uses packages `mathptmx` (Times fonts) and `parskip` (separate paragraphs by
vertical white-space) as well as the tex snippet ``preamble.tex``

Backwards compatibility
-----------------------

Currently, if no filename extension is given in the ``stylesheet`` argument,
``.tex`` is assumed (by latex).

This behaviour will change.

Needed Action:
  Always specify the extension if you want the style sheet to be
  ``\input`` by LaTeX.

Custom roles
============

Did you ever use custom roles and realise that the exported LaTeX code does
not compile?

  yes

  I don't use custom roles, primarily because I'm afraid of such problems.

If YES, how important is backwards compatibility? Would you update your
style-sheets if the interface improves significantly but non-compatible?

  I would update, but as I suggested in the other thread, I do not see any
  advantage in using a shorter prefix than 'docutils'.

Default font
============

Which default font do you prefer for the output?

  By default, PDFLaTeX uses embedded computer modern family, which look good
  on paper but poor on LCDs (even if outlined, due to suboptimal hinting),
  and result in large PDFs.  rst2pdf uses the "standard" PDF fonts by
  default (Times, Helvetica, Courier) which IMO look reasonable both on
  screen and on paper.  Because they aren't embedded, it also results in a
  smaller file size.

Using different fonts (e.g. standard postscript fonts) can be achieved
easily with the ``--stylesheet=times`` (or similar) command line option or
configuration setting or by choosing a font package in the style-sheet.

  Hmm, is this documented in and easy-to-discover place in the docutils /
  rst2latex documentation?  I don't remember seeing it mentioned before.

Not yet.

Font embedding must be configured in the LaTeX installation.

Proposal
--------

Use one of the Postscript default fonts supported by
standard LaTeX (pp 10 and 11 of the `PSNFSS documentation`_)

Bookman
  -2 very very wide
Charter
  +0 nonspectacular
  -1 no "Base35" font 
New Century Schoolbook
  -1 very wide
Palatino
  +1 recommended by font experts
Times
  ±0 used everywhere, 
  -1 narrow (devised for multi-column layouts)
Utopia
  +1 recommended by font experts
  -1 no "Base35" font, not regarded as a "required" part of LaTeX any more.

* Implement as default stylesheet option, so it can be easily
  overridden.

Font encoding
=============

In modern LaTeX distributions (MikTeX, TeXLive, teTeX) T1 encoded fonts
belong to the "core" while the "ae" (and even more the "aeguill")
workarounds are optional (and not always installed by default).

"ae" uses the original 7-bit encoded CM fonts and combines additional
(non-ASCII) characters. This can lead to suboptimal appearance and to
problems if text shall be extracted from (or found in) the generated
PDF document.

Proposal
--------

Use the T1 font encoding as default.


Adaptive preamble
=================

  TeXlive is a large download, and in addition to the base package
  rst2latex also requires a bunch of extra .sty packages, even if they
  aren't used in the current document.

Solution:
  include only package loading commands and definitions that are
  needed in the current document.

Proposal
--------

sample definitions and commands in a dictionary, e.g.::

	# Fallback definitions for Docutils-specific commands
        self.latex_fallbacks = {}

	...

        # Fallback definitions for docutils-specific latex objects
        required_fallbacks = self.latex_fallbacks.keys()
        required_fallbacks.sort()
        for key in required_fallbacks:
            self.head_prefix.append(self.latex_fallbacks[key])

 	...

	def visit_inline(self, node): # titlereference
        # insert fallback definition
        self.latex_fallbacks['inline'] = latex_headings['DUspan']
        classes = node.get('classes', [])
        self.body.append(r'\DUspan{%s}{' %','.join(classes))

Allow customising in a style sheet by use of ``\ProvideCommand`` for the
fallback definition.

Implementations
***************

Browse the `SVN repository at berlios.de`__

__ http://svn.berlios.de/viewvc/docutils/trunk/sandbox/latex-variants/

* The `docs` dir contains generic documentation and discussion.

  Writer specific documentation is placed in the respective "literate"
  source.

latex2e branches
================

* `latex2e_external_stylesheet` replaces most of the generated preamble by
  a latex stylesheet.

* `latex2e_headings_as_multi_line_strings.py` tries to make the latex2e
  writer source more readable by the use of raw literal strings
  instead of escaped backslashes.

* `latex2e_listings` provides an alternative implementation of the new
  ``--literal-env`` option to select the environment for
  typesetting literal blocks.

  This options allows e.g. to choose `lstlisting` from the ``listings.sty``
  LaTeX package. See also `<docs/syntax-highlight-with-listings.html>`_

alternative latex writers
=========================

Currently none.

related sandbox projects
========================

* `<../dkuhlman/Docs>`__,
* `<../docpy-writer>`__,
* `<../jensj/latex_math>`__,
* `<../latex_directive>`__, and
* `<../verse-writer>`__.

.. References
   ==========
.. _PSNFSS documentation:
   http://dante.ctan.org/CTAN/macros/latex/required/psnfss/psnfss2e.pdf
