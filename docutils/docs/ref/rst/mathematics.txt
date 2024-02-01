.. include:: ../../header2.txt

============================
LaTeX syntax for mathematics
============================

.. role:: m(math)
.. default-role:: math
.. |latex| replace:: L\ :sup:`A`\ T\ :sub:`E`\ X

:abstract: Docutils supports mathematical content with a `"math"
           directive`__ and `role`__. The input format is *LaTeX math
           syntax*\ [#math-syntax]_ with support for literal Unicode symbols.

.. sectnum::
.. contents::

__ https://docutils.sourceforge.io/docs/ref/rst/directives.html#math
__ https://docutils.sourceforge.io/docs/ref/rst/roles.html#math

Inline formulas and displayed equations
=======================================

The **math role** can be used for inline mathematical expressions:
``:math:`\psi(r) = \exp(-2r)``` will produce :m:`\psi(r)=\exp(-2r)`.
Inside the backtics you can write anything you would write between dollar
signs in a LaTeX document.  [#math-syntax]_
 
.. tip::

  If you put ``.. default-role:: math`` at the top of your
  document, you can write ```x^2``` instead of the longer
  version: ``:math:`x^2```.  You can also introduce an
  abbreviation like this ``.. role:: m(math)``.  That will allow
  you to write ``:m:`x^2``` or ```x^2`:m:``.


The **math directive** is used for displayed equations. It corresponds to
an ``equation*`` or ``align*`` environment in a LaTeX document. If you
write::

  .. math:: \psi(r) = e^{-2r}

you will get:

.. math:: \psi(r) = e^{-2r}

A more complex example is the definition of the `Fourier transform`_::

  .. math::
     :name: Fourier transform

     (\mathcal{F}f)(y)
      = \frac{1}{\sqrt{2\pi}^{\ n}}
        \int_{\mathbb{R}^n} f(x)\,
        e^{-\mathrm{i} y \cdot x} \,\mathrm{d} x.

which is rendered as:

.. math::
   :name: Fourier transform

     (\mathcal{F}f)(y)
      = \frac{1}{\sqrt{2\pi}^{\ n}}
        \int_{\mathbb{R}^n} f(x)\,
        e^{-\mathrm{i} y \cdot x} \,\mathrm{d} x.

The ``:name:`` option puts a label on the equation that can be
linked to by `hyperlink references`_.

Displayed equations can use ``\\`` and ``&`` for line shifts and alignments::

  .. math::

   a &= (x + y)^2         &  b &= (x - y)^2 \\
     &= x^2 + 2xy + y^2   &    &= x^2 - 2xy + y^2

LaTeX output will wrap it in an ``align*`` environment.
The result is:

.. math::

   a & = (x + y)^2         &  b & = (x - y)^2 \\
     & = x^2 + 2xy + y^2   &    & = x^2 - 2xy + y^2

The ``aligned`` environment can be used as a component in a containing
expression. E.g., ::

  .. math::
    \left.
      \begin{aligned}
        B' & = -\partial\times E,         \\
        E' & = \partial\times B - 4\pi j,
      \end{aligned}
    \right\}
    \qquad \text{Maxwell’s equations}

results in

.. math::
  \left.
    \begin{aligned}
      B' & = -\partial\times E         \\
      E' & =  \partial\times B - 4\pi j
    \end{aligned}
  \;\right\}
  \qquad \text{Maxwell’s equations.}


.. [#math-syntax] The supported LaTeX commands include AMS extensions
   (see, e.g., the `Short Math Guide`_). Some of the shown symbols
   require the "amssymb" `LaTeX package`_ (or another package providing
   the AMS symbol macros) when exported with the "latex" writer.

   The support is limited to a subset of *LaTeX math* by the conversion
   required for many output formats.  For HTML, the `math_output`_
   configuration setting (or the corresponding ``--math-output`` command
   line option) selects between alternative output formats with different
   subsets of supported elements. If a writer does not support math
   typesetting, the content is inserted verbatim.

.. _hyperlink references:
   ../ref/rst/restructuredtext.html#hyperlink-references
.. _Short Math Guide:
   https://mirrors.ctan.org/info/short-math-guide/short-math-guide.pdf
.. _math_output:
   https://docutils.sourceforge.io/docs/user/config.html#math-output
.. _LaTeX package:
   ../../user/latex.html#latex-document-classes-and-packages


Mathematical symbols
====================

The following tables are adapted from the first edition of
"The LaTeX Companion" (Goossens, Mittelbach, Samarin) and the
AMS `Short Math Guide`_.


Accents and embellishments
--------------------------

The "narrow" accents are intended for a single-letter base.

.. class:: colwidths-auto

  =========== =============  ============ ==============  ============== ================  ========= ===========
  `\acute{x}` ``\acute{x}``  `\dot{t}`    ``\dot{t}``     `\grave{x}`    ``\grave{x}``     `\vec{x}` ``\vec{x}``
  `\bar{v}`   ``\bar{v}``    `\ddot{t}`   ``\ddot{t}``    `\hat{x}`      ``\hat{x}``
  `\breve{x}` ``\breve{x}``  `\dddot{t}`  ``\dddot{t}``   `\mathring{x}` ``\mathring{x}``
  `\check{x}` ``\check{x}``  `\ddddot{t}` ``\ddddot{t}``  `\tilde{n}`    ``\tilde{n}``
  =========== =============  ============ ==============  ============== ================  ========= ===========

When adding an accent to an i or j in math, dotless variants can be
obtained with ``\imath`` and ``\jmath``: `\hat \imath`, `\vec{\jmath}`.

For embellishments that span multiple symbols, use:

.. class:: colwidths-auto

  ========================== ============================  =========================== =============================
  `\widetilde{gbi}`          ``\widetilde{gbi}``           `\widehat{gbi}`             ``\widehat{gbi}``
  `\overline{gbi}`           ``\overline{gbi}``            `\underline{gbi}`           ``\underline{gbi}``
  `\overbrace{gbi}`          ``\overbrace{gbi}``           `\underbrace{gbi}`          ``\underbrace{gbi}``
  `\overleftarrow{gbi}`      ``\overleftarrow{gbi}``       `\underleftarrow{gbi}`      ``\underleftarrow{gbi}``
  `\overrightarrow{gbi}`     ``\overrightarrow{gbi}``      `\underrightarrow{gbi}`     ``\underrightarrow{gbi}``
  `\overleftrightarrow{gbi}` ``\overleftrightarrow{gbi}``  `\underleftrightarrow{gbi}` ``\underleftrightarrow{gbi}``
  ========================== ============================  =========================== =============================


Binary operators
----------------
.. class:: colwidths-auto

  ================== ====================  ================= ===================  ================== ====================
  `*`                ``*``                 `\circledast`     ``\circledast``      `\ominus`          ``\ominus``
  `+`                ``+``                 `\circledcirc`    ``\circledcirc``     `\oplus`           ``\oplus``
  `-`                ``-``                 `\circleddash`    ``\circleddash``     `\oslash`          ``\oslash``
  `:`                ``:``                 `\cup`            ``\cup``             `\otimes`          ``\otimes``
  `\Cap`             ``\Cap``              `\curlyvee`       ``\curlyvee``        `\pm`              ``\pm``
  `\Cup`             ``\Cup``              `\curlywedge`     ``\curlywedge``      `\rightthreetimes` ``\rightthreetimes``
  `\amalg`           ``\amalg``            `\dagger`         ``\dagger``          `\rtimes`          ``\rtimes``
  `\ast`             ``\ast``              `\ddagger`        ``\ddagger``         `\setminus`        ``\setminus``
  `\bigcirc`         ``\bigcirc``          `\diamond`        ``\diamond``         `\smallsetminus`   ``\smallsetminus``
  `\bigtriangledown` ``\bigtriangledown``  `\div`            ``\div``             `\sqcap`           ``\sqcap``
  `\bigtriangleup`   ``\bigtriangleup``    `\divideontimes`  ``\divideontimes``   `\sqcup`           ``\sqcup``
  `\boxdot`          ``\boxdot``           `\dotplus`        ``\dotplus``         `\star`            ``\star``
  `\boxminus`        ``\boxminus``         `\doublebarwedge` ``\doublebarwedge``  `\times`           ``\times``
  `\boxplus`         ``\boxplus``          `\gtrdot`         ``\gtrdot``          `\triangleleft`    ``\triangleleft``
  `\boxtimes`        ``\boxtimes``         `\intercal`       ``\intercal``        `\triangleright`   ``\triangleright``
  `\bullet`          ``\bullet``           `\leftthreetimes` ``\leftthreetimes``  `\uplus`           ``\uplus``
  `\cap`             ``\cap``              `\lessdot`        ``\lessdot``         `\vee`             ``\vee``
  `\cdot`            ``\cdot``             `\ltimes`         ``\ltimes``          `\veebar`          ``\veebar``
  `\centerdot`       ``\centerdot``        `\mp`             ``\mp``              `\wedge`           ``\wedge``
  `\circ`            ``\circ``             `\odot`            ``\odot``           `\wr`              ``\wr``
  ================== ====================  ================= ===================  ================== ====================


Extensible delimiters
---------------------
Unless you indicate otherwise, delimiters in math formulas remain at the
standard size regardless of the height of the enclosed material. To get
adaptable sizes, use ``\left`` and ``\right`` prefixes, for example
`g(A,B,Y) = f \left(A,B,X=h^{[X]}(Y)\right)` or

.. math:: a_n = \left(\frac{1}{2}\right)^n

Use ``.`` for "empty" delimiters:

.. math:: A = \left . \frac{1}{1-n}\, \right |_{n=0}^\infty

See also the commands for fixed `delimiter sizes`_ below.

The following symbols extend when used with ``\left`` and ``\right``:

Pairing delimiters
~~~~~~~~~~~~~~~~~~
.. class:: colwidths-auto

  =============== =================   ========================= ===========================
  `( )`           ``( )``             `\langle \rangle`         ``\langle \rangle``
  `[ ]`           ``[ ]``             `\lceil  \rceil`          ``\lceil \rceil``
  `\{ \}`         ``\{ \}``           `\lfloor \rfloor`         ``\lfloor \rfloor``
  `\lvert \rvert` ``\lvert \rvert``   `\lgroup \rgroup`         ``\lgroup \rgroup``
  `\lVert \rVert` ``\lVert \rVert``   `\lmoustache \rmoustache` ``\lmoustache \rmoustache``
  =============== =================   ========================= ===========================


Nonpairing delimiters
~~~~~~~~~~~~~~~~~~~~~
.. class:: colwidths-auto

  ==== ======  ============ ==============  ============ ==============
  `|`  ``|``   `\vert`      ``\vert``       `\arrowvert` ``\arrowvert``
  `\|` ``\|``  `\Vert`      ``\Vert``       `\Arrowvert` ``\Arrowvert``
  `/`  ``/``   `\backslash` ``\backslash``  `\bracevert` ``\bracevert``
  ==== ======  ============ ==============  ============ ==============

The use of ``|`` and ``\|`` for pairs of vertical bars may produce
incorrect spacing, e.g., ``|k|=|-k|`` produces `|k| = |−k|` and
``|\sin(x)|`` produces `|\sin(x)|`. The pairing delimiters, e.g.
`\lvert -k\rvert` and `\lvert\sin(x)\rvert`, prevent this problem.


Extensible vertical arrows
--------------------------
.. class:: colwidths-auto

  ===============================  ======================================
  `\uparrow`     ``\uparrow``      `\Uparrow`     ``\Uparrow``
  `\downarrow`   ``\downarrow``    `\Downarrow`   ``\Downarrow``
  `\updownarrow` ``\updownarrow``  `\Updownarrow` ``\Updownarrow``
  ===============================  ======================================


Functions (named operators)
---------------------------
.. class:: colwidths-auto

  ========= ===========  ========= ===========  ============= ================
  `\arccos` ``\arccos``  `\gcd`    ``\gcd``     `\Pr`         ``\Pr``
  `\arcsin` ``\arcsin``  `\hom`    ``\hom``     `\projlim`    ``\projlim``
  `\arctan` ``\arctan``  `\inf`    ``\inf``     `\sec`        ``\sec``
  `\arg`    ``\arg``     `\injlim` ``\injlim``  `\sin`        ``\sin``
  `\cos`    ``\cos``     `\ker`    ``\ker``     `\sinh`       ``\sinh``
  `\cosh`   ``\cosh``    `\lg`     ``\lg``      `\sup`        ``\sup``
  `\cot`    ``\cot``     `\lim`    ``\lim``     `\tan`        ``\tan``
  `\coth`   ``\coth``    `\liminf` ``\liminf``  `\tanh`       ``\tanh``
  `\csc`    ``\csc``     `\limsup` ``\limsup``  `\varlimsup`  ``\varlimsup``
  `\deg`    ``\deg``     `\ln`     ``\ln``      `\varliminf`  ``\varliminf``
  `\det`    ``\det``     `\log`    ``\log``     `\varprojlim` ``\varprojlim``
  `\dim`    ``\dim``     `\max`    ``\max``     `\varinjlim`  ``\varinjlim``
  `\exp`    ``\exp``     `\min`    ``\min``
  ========= ===========  ========= ===========  ============= ================

Named operators outside the above list can be typeset with
``\operatorname{name}``, e.g.

.. math:: \operatorname{sgn}(-3) = -1.

.. TODO: \operatorname* for function name with limits.

The ``\DeclareMathOperator`` command can only be used in the
`LaTeX preamble`_.

.. _LaTeX preamble: latex.html#latex-preamble


Greek letters
-------------

Greek letters that have Latin look-alikes are rarely used in math
formulas and not supported by LaTeX.

.. class:: colwidths-auto

  ========== ============  ========== ============  ========== ============  ============== ===============
  `\Gamma`   ``\Gamma``    `\alpha`   ``\alpha``    `\mu`      ``\mu``       `\omega`       ``\omega``
  `\Delta`   ``\Delta``    `\beta`    ``\beta``     `\nu`      ``\nu``       `\digamma`     ``\digamma``
  `\Lambda`  ``\Lambda``   `\gamma`   ``\gamma``    `\xi`      ``\xi``       `\varepsilon`  ``\varepsilon``
  `\Phi`     ``\Phi``      `\delta`   ``\delta``    `\pi`      ``\pi``       `\varkappa`    ``\varkappa``
  `\Pi`      ``\Pi``       `\epsilon` ``\epsilon``  `\rho`     ``\rho``      `\varphi`      ``\varphi``
  `\Psi`     ``\Psi``      `\zeta`    ``\zeta``     `\sigma`   ``\sigma``    `\varpi`       ``\varpi``
  `\Sigma`   ``\Sigma``    `\eta`     ``\eta``      `\tau`     ``\tau``      `\varrho`      ``\varrho``
  `\Theta`   ``\Theta``    `\theta`   ``\theta``    `\upsilon` ``\upsilon``  `\varsigma`    ``\varsigma``
  `\Upsilon` ``\Upsilon``  `\iota`    ``\iota``     `\phi`     ``\phi``      `\vartheta`    ``\vartheta``
  `\Xi`      ``\Xi``       `\kappa`   ``\kappa``    `\chi`     ``\chi``
  `\Omega`   ``\Omega``    `\lambda`  ``\lambda``   `\psi`     ``\psi``
  ========== ============  ========== ============  ========== ============  ============== ===============

In LaTeX, the default font for capital Greek letters is upright/roman.
*Italic* capital Greek letters can be obtained by loading a `package
providing the "ISO" math style`__. They are used by default in MathML.

Individual Greek italic capitals can also be achieved preceding the
letter name with ``var`` like ``\varPhi``:
`\varGamma\ \varDelta\ \varLambda\ \varPhi\ \varPi\ \varPsi\ \varSigma\
\varTheta\ \varUpsilon\ \varXi\ \varOmega`


__ https://mirrors.ctan.org/macros/latex/contrib/isomath/isomath.html#table-2


Letterlike symbols
------------------
.. class:: colwidths-auto

  ============= ===============  ========== ============  ========== ============  =========== =============
  `\forall`     ``\forall``      `\aleph`   ``\aleph``    `\hbar`    ``\hbar``     `\ell`      ``\ell``
  `\complement` ``\complement``  `\beth`    ``\beth``     `\hslash`  ``\hslash``   `\wp`       ``\wp``
  `\exists`     ``\exists``      `\gimel`   ``\gimel``    `\Im`      ``\Im``       `\Re`       ``\Re``
  `\Finv`       ``\Finv``        `\daleth`  ``\daleth``   `\imath`   ``\imath``    `\circledR` ``\circledR``
  `\Game`       ``\Game``        `\partial` ``\partial``  `\jmath`   ``\jmath``    `\circledS` ``\circledS``
  `\mho`        ``\mho``         `\eth`     ``\eth``      `\Bbbk`    ``\Bbbk``
  ============= ===============  ========== ============  ========== ============  =========== =============


.. _math alphabet:

Math alphabets
--------------

The TeX *math alphabet* macros are intended for mathematical variables
where style variations are important semantically.
They style letters and numbers with a combination of font attributes
(shape, weight, family) --- non-alphanumerical symbols, function names,
and mathematical text are left unchanged.

MathML uses the *mathvariant* `style attribute`_ or pre-styled characters
from the `Mathematical Alphanumeric Symbols`_ Unicode block.

.. class:: colwidths-auto

  ===============  ============================  ==========================
  command          example                       result
  ===============  ============================  ==========================
  ``\mathrm``      ``s_\mathrm{out}``            `s_\mathrm{out}`
  ``\mathbf``      ``\mathbf{r}^2=x^2+y^2``      `\mathbf{r}^2=x^2+y^2`
  ``\mathit``      ``\mathit{\sin\Gamma}``       `\mathit{\sin\Gamma}`
  ``\mathcal``     ``\mathcal{F}f(x)``           `\mathcal{F}f(x)`
  ``\mathbb``      ``\mathbb{R \subset C}``      `\mathbb{R \subset C}`
  ``\mathfrak``    ``\mathfrak{a+b}``            `\mathfrak{a+b}`
  ``\mathsf``      ``\mathsf x``                 `\mathsf x`
  ``\mathtt``      ``\mathtt{0.12}``             `\mathtt{0.12}`
  ===============  ============================  ==========================

The set of characters in a given "math alphabet" varies.
LaTeX may produce garbage for unsupported characters.
Additional math alphabets are defined in LaTeX packages, e.g.,

* ``\mathbfit`` from isomath_ allows vector symbols in line with the
  International Standard [ISO-80000-2]_.
  E.g., ``\mathbfit{r}^2=x^2+y^2`` becomes `\mathbfit{r}^2=x^2+y^2`.

* Several packages, e.g. mathrsfs_, define ``\mathscr`` that selects a
  differently shaped "script" alphabet.

The listing below shows the characters supported by Unicode and
Docutils with math_output_ MathML. [#italic-digits]_

default:
  `{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz\ ı\jmath}`
  `{ΓΔΘΛΞΠΣΥΦΨΩ\ αβγδεζηθικλμνξπρςστυφχψω\ ϵϑϕϰϱϖϜϝ\ \partial∇}`
  `{0123456789}`
mathrm:
  `\mathrm{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz\ ı\jmath}`
  `\mathrm{ΓΔΘΛΞΠΣΥΦΨΩ\ αβγδεζηθικλμνξπρςστυφχψω\ ϵϑϕϰϱϖϜϝ\ \partial∇}`
  `\mathrm{0123456789}`
mathbf:
  `\mathbf{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
  `\mathbf{ΓΔΘΛΞΠΣΥΦΨΩ\ αβγδεζηθικλμνξπρςστυφχψω\ ϵϑϕϰϱϖϜϝ\ \partial∇}`
  `\mathbf{0123456789}`
mathit:
  `\mathit{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz\ ı\jmath}`
  `\mathit{ΓΔΘΛΞΠΣΥΦΨΩ\ αβγδεζηθικλμνξπρςστυφχψω\ ϵϑϕϰϱϖ\ \partial∇}`
  `\mathit{0123456789}` [#italic-digits]_
mathbfit:
  `\mathbfit{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
  `\mathbfit{ΓΔΘΛΞΠΣΥΦΨΩ\ αβγδεζηθικλμνξπρςστυφχψω\ ϵϑϕϰϱϖ\ \partial∇}`
mathcal:
  `\mathcal{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
mathscr:
  `\mathscr{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
mathbb:
  `\mathbb{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
  `\mathbb{ΓΠΣ\ γπ}`
  `\mathbb{0123456789}`
mathfrak:
  `\mathfrak{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
mathsf:
  `\mathsf{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
  `\mathsf{0123456789}`
mathsfit:
  `\mathsfit{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
mathsfbfit:
  `\mathsfbfit{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
  `\mathsfbfit{ΓΔΘΛΞΠΣΥΦΨΩ\ αβγδεζηθικλμνξπρςστυφχψω\ ϵϑϕϰϱϖ\ \partial∇}`
mathtt:
  `\mathtt{ABCDEFGHIJKLMNOPQRSTUVWXYZ\ abcdefghijklmnopqrstuvwxyz}`
  `\mathtt{0123456789}`

.. [#italic-digits] Italic digits are not defined in Unicode
   but work in LaTeX.

In contrast to the *math alphabet* selectors, ``\boldsymbol`` only
changes the *font weight*. It can be used to get a bold version of
any mathematical symbol:

.. math::
   V_i x \pm \cos(\alpha) \approx 3\Gamma \quad \forall x\in\mathbb{R}

   \boldsymbol{V_i x \pm \cos(\alpha) \approx 3\Gamma \quad \forall x\in\mathbb{R}}

It is usually ill-advised to apply ``\boldsymbol`` to more than one symbol
at a time.

.. _style attribute: https://www.w3.org/TR/mathml4/#presm_commatt
.. _Mathematical Alphanumeric Symbols:
    https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols
.. _isomath: https://www.ctan.org/pkg/isomath
.. _mathrsfs: https://www.ctan.org/pkg/mathrsfs
.. _unicode-math: https://www.ctan.org/pkg/unicode-math


Miscellaneous symbols
---------------------

.. class:: colwidths-auto

==================== ======================  ================ ==================  ================= ===================
`\#`                 ``\#``                  `\clubsuit`      ``\clubsuit``       `\neg`            ``\neg``
`\&`                 ``\&``                  `\diamondsuit`   ``\diamondsuit``    `\nexists`        ``\nexists``
`\angle`             ``\angle``              `\emptyset`      ``\emptyset``       `\prime`          ``\prime``
`\backprime`         ``\backprime``          `\exists`        ``\exists``         `\sharp`          ``\sharp``
`\bigstar`           ``\bigstar``            `\flat`          ``\flat``           `\spadesuit`      ``\spadesuit``
`\blacklozenge`      ``\blacklozenge``       `\forall`        ``\forall``         `\sphericalangle` ``\sphericalangle``
`\blacksquare`       ``\blacksquare``        `\heartsuit`     ``\heartsuit``      `\square`         ``\square``
`\blacktriangle`     ``\blacktriangle``      `\infty`         ``\infty``          `\surd`           ``\surd``
`\blacktriangledown` ``\blacktriangledown``  `\lozenge`       ``\lozenge``        `\top`            ``\top``
`\bot`               ``\bot``                `\measuredangle` ``\measuredangle``  `\triangle`       ``\triangle``
`\diagdown`          ``\diagdown``           `\nabla`         ``\nabla``          `\triangledown`   ``\triangledown``
`\diagup`            ``\diagup``             `\natural`       ``\natural``        `\varnothing`     ``\varnothing``
==================== ======================  ================ ==================  ================= ===================


Punctuation
-----------
.. class:: colwidths-auto

=== =====  ======== ===============  ======== ==========
`.` ``.``  `!`      ``!``            `\vdots` ``\vdots``
`/` ``/``  `?`      ``?``            `\dotsb` ``\dotsb``
`|` ``|``  `\colon` ``\colon`` [#]_  `\dotsc` ``\dotsc``
`'` ``'``  `\cdots` ``\cdots``       `\dotsi` ``\dotsi``
`;` ``;``  `\ddots` ``\ddots``       `\dotsm` ``\dotsm``
`:` ``:``  `\ldots` ``\ldots``       `\dotso` ``\dotso``
=== =====  ======== ===============  ======== ==========

.. [#] Punctuation (not ratio):
       Compare spacing in `a\colon b\to c` to `a:b = c`.


Relation symbols
----------------

Arrows
~~~~~~
.. class:: colwidths-auto

  ====================== ========================  ===================== =======================
  `\circlearrowleft`     ``\circlearrowleft``      `\circlearrowright`   ``\circlearrowright``
  `\curvearrowleft`      ``\curvearrowleft``       `\curvearrowright`    ``\curvearrowright``
  `\hookleftarrow`       ``\hookleftarrow``        `\hookrightarrow`     ``\hookrightarrow``
  `\leftarrow`           ``\leftarrow``            `\rightarrow`         ``\rightarrow``
  `\Leftarrow`           ``\Leftarrow``            `\Rightarrow`         ``\Rightarrow``
  `\leftarrowtail`       ``\leftarrowtail``        `\rightarrowtail`     ``\rightarrowtail``
  `\leftharpoondown`     ``\leftharpoondown``      `\rightharpoondown`   ``\rightharpoondown``
  `\leftharpoonup`       ``\leftharpoonup``        `\rightharpoonup`     ``\rightharpoonup``
  `\leftleftarrows`      ``\leftleftarrows``       `\rightrightarrows`   ``\rightrightarrows``
  `\leftrightarrow`      ``\leftrightarrow``       `\Leftrightarrow`     ``\Leftrightarrow``
  `\leftrightarrows`     ``\leftrightarrows``      `\rightleftarrows`    ``\rightleftarrows``
  `\leftrightharpoons`   ``\leftrightharpoons``    `\rightleftharpoons`  ``\rightleftharpoons``
  `\leftrightsquigarrow` ``\leftrightsquigarrow``  `\rightsquigarrow`    ``\rightsquigarrow``
  `\Lleftarrow`          ``\Lleftarrow``           `\Rrightarrow`        ``\Rrightarrow``
  `\longleftarrow`       ``\longleftarrow``        `\longrightarrow`     ``\longrightarrow``
  `\Longleftarrow`       ``\Longleftarrow``        `\Longrightarrow`     ``\Longrightarrow``
  `\longleftrightarrow`  ``\longleftrightarrow``   `\Longleftrightarrow` ``\Longleftrightarrow``
  `\looparrowleft`       ``\looparrowleft``        `\looparrowright`     ``\looparrowright``
  `\Lsh`                 ``\Lsh``                  `\Rsh`                ``\Rsh``
  `\mapsto`              ``\mapsto``               `\longmapsto`         ``\longmapsto``
  `\multimap`            ``\multimap``
  `\nleftarrow`          ``\nleftarrow``           `\nrightarrow`        ``\nrightarrow``
  `\nLeftarrow`          ``\nLeftarrow``           `\nRightarrow`        ``\nRightarrow``
  `\nleftrightarrow`     ``\nleftrightarrow``      `\nLeftrightarrow`    ``\nLeftrightarrow``
  `\nwarrow`             ``\nwarrow``              `\nearrow`            ``\nearrow``
  `\swarrow`             ``\swarrow``              `\searrow`            ``\searrow``
  `\twoheadleftarrow`    ``\twoheadleftarrow``     `\twoheadrightarrow`  ``\twoheadrightarrow``
  `\upharpoonleft`       ``\upharpoonleft``        `\upharpoonright`     ``\upharpoonright``
  `\downharpoonleft`     ``\downharpoonleft``      `\downharpoonright`   ``\downharpoonright``
  `\upuparrows`          ``\upuparrows``           `\downdownarrows`     ``\downdownarrows``
  ====================== ========================  ===================== =======================

Synonyms: `\gets` ``\gets``, `\to` ``\to``, `\restriction` ``\restriction``.

Comparison
~~~~~~~~~~

.. class:: colwidths-auto

================ ==================  ============= ===============  ============= ===============  =============== =================
`<`              ``<``               `\geq`           ``\geq``      `\ll`         ``\ll``          `\prec`         ``\prec``
`=`              ``=``               `\geqq`       ``\geqq``        `\lll`        ``\lll``         `\precapprox`   ``\precapprox``
`>`              ``>``               `\geqslant`   ``\geqslant``    `\lnapprox`   ``\lnapprox``    `\preccurlyeq`  ``\preccurlyeq``
`\approx`        ``\approx``         `\gg`         ``\gg``          `\lneq`       ``\lneq``        `\preceq`       ``\preceq``
`\approxeq`      ``\approxeq``       `\ggg`        ``\ggg``         `\lneqq`      ``\lneqq``       `\precnapprox`  ``\precnapprox``
`\asymp`         ``\asymp``          `\gnapprox`   ``\gnapprox``    `\lnsim`      ``\lnsim``       `\precneqq`     ``\precneqq``
`\backsim`       ``\backsim``        `\gneq`       ``\gneq``        `\ncong`      ``\ncong``       `\precnsim`     ``\precnsim``
`\backsimeq`     ``\backsimeq``      `\gneqq`      ``\gneqq``       `\neq`        ``\neq``         `\precsim`      ``\precsim``
`\bumpeq`        ``\bumpeq``         `\gnsim`      ``\gnsim``       `\ngeq`       ``\ngeq``        `\risingdotseq` ``\risingdotseq``
`\Bumpeq`        ``\Bumpeq``         `\gtrapprox`  ``\gtrapprox``   `\ngeqq`      ``\ngeqq``       `\sim`          ``\sim``
`\circeq`        ``\circeq``         `\gtreqless`  ``\gtreqless``   `\ngeqslant`  ``\ngeqslant``   `\simeq`        ``\simeq``
`\cong`          ``\cong``           `\gtreqqless` ``\gtreqqless``  `\ngtr`       ``\ngtr``        `\succ`         ``\succ``
`\curlyeqprec`   ``\curlyeqprec``    `\gtrless`    ``\gtrless``     `\nleq`       ``\nleq``        `\succapprox`   ``\succapprox``
`\curlyeqsucc`   ``\curlyeqsucc``    `\gtrsim`     ``\gtrsim``      `\nleqq`      ``\nleqq``       `\succcurlyeq`  ``\succcurlyeq``
`\doteq`         ``\doteq``          `\leq`        ``\leq``         `\nleqslant`  ``\nleqslant``   `\succeq`       ``\succeq``
`\doteqdot`      ``\doteqdot``       `\leqq`       ``\leqq``        `\nless`      ``\nless``       `\succnapprox`  ``\succnapprox``
`\eqcirc`        ``\eqcirc``         `\leqslant`   ``\leqslant``    `\nprec`      ``\nprec``       `\succneqq`     ``\succneqq``
`\eqsim`         ``\eqsim``          `\lessapprox` ``\lessapprox``  `\npreceq`    ``\npreceq``     `\succnsim`     ``\succnsim``
`\eqslantgtr`    ``\eqslantgtr``     `\lesseqgtr`  ``\lesseqgtr``   `\nsim`       ``\nsim``        `\succsim`      ``\succsim``
`\eqslantless`   ``\eqslantless``    `\lesseqqgtr` ``\lesseqqgtr``  `\nsucc`      ``\nsucc``       `\thickapprox`  ``\thickapprox``
`\equiv`         ``\equiv``          `\lessgtr`    ``\lessgtr``     `\nsucceq`    ``\nsucceq``     `\thicksim`     ``\thicksim``
`\fallingdotseq` ``\fallingdotseq``  `\lesssim`    ``\lesssim``                                    `\triangleq`    ``\triangleq``
================ ==================  ============= ===============  ============= ===============  =============== =================

The commands ``\lvertneqq`` and ``\gvertneqq`` are not supported
with MathML output, as there is no corresponding Unicode character.

Synonyms: `\ne` ``\ne``, `\le` ``\le``, `\ge` ``\ge``,
`\Doteq` ``\Doteq``, `\llless` ``\llless``, `\gggtr` ``\gggtr``.

Symbols can be negated prepending ``\not``, e.g.
`\not=` ``\not=``, `\not\equiv` ``\not\equiv``,
`\not\gtrless` ``\not\gtrless``, `\not\lessgtr` ``\not\lessgtr``.

Miscellaneous relations
~~~~~~~~~~~~~~~~~~~~~~~
.. class:: colwidths-auto

  ===================== =======================  =================== =====================  =================== =====================
  `\backepsilon`        ``\backepsilon``         `\ntrianglelefteq`  ``\ntrianglelefteq``   `\subseteq`         ``\subseteq``
  `\because`            ``\because``             `\ntriangleright`   ``\ntriangleright``    `\subseteqq`        ``\subseteqq``
  `\between`            ``\between``             `\ntrianglerighteq` ``\ntrianglerighteq``  `\subsetneq`        ``\subsetneq``
  `\blacktriangleleft`  ``\blacktriangleleft``   `\nvdash`           ``\nvdash``            `\subsetneqq`       ``\subsetneqq``
  `\blacktriangleright` ``\blacktriangleright``  `\nVdash`           ``\nVdash``            `\supset`           ``\supset``
  `\bowtie`             ``\bowtie``              `\nvDash`           ``\nvDash``            `\Supset`           ``\Supset``
  `\dashv`              ``\dashv``               `\nVDash`           ``\nVDash``            `\supseteq`         ``\supseteq``
  `\frown`              ``\frown``               `\parallel`         ``\parallel``          `\supseteqq`        ``\supseteqq``
  `\in`                 ``\in``                  `\perp`             ``\perp``              `\supsetneq`        ``\supsetneq``
  `\mid`                ``\mid``                 `\pitchfork`        ``\pitchfork``         `\supsetneqq`       ``\supsetneqq``
  `\models`             ``\models``              `\propto`           ``\propto``            `\therefore`        ``\therefore``
  `\ni`                 ``\ni``                  `\shortmid`         ``\shortmid``          `\trianglelefteq`   ``\trianglelefteq``
  `\nmid`               ``\nmid``                `\shortparallel`    ``\shortparallel``     `\trianglerighteq`  ``\trianglerighteq``
  `\notin`              ``\notin``               `\smallfrown`       ``\smallfrown``        `\varpropto`        ``\varpropto``
  `\nparallel`          ``\nparallel``           `\smallsmile`       ``\smallsmile``        `\vartriangle`      ``\vartriangle``
  `\nshortmid`          ``\nshortmid``           `\smile`            ``\smile``             `\vartriangleleft`  ``\vartriangleleft``
  `\nshortparallel`     ``\nshortparallel``      `\sqsubset`         ``\sqsubset``          `\vartriangleright` ``\vartriangleright``
  `\nsubseteq`          ``\nsubseteq``           `\sqsubseteq`       ``\sqsubseteq``        `\vdash`            ``\vdash``
  `\nsubseteqq`         ``\nsubseteqq``          `\sqsupset`         ``\sqsupset``          `\Vdash`            ``\Vdash``
  `\nsupseteq`          ``\nsupseteq``           `\sqsupseteq`       ``\sqsupseteq``        `\vDash`            ``\vDash``
  `\nsupseteqq`         ``\nsupseteqq``          `\subset`           ``\subset``            `\Vvdash`           ``\Vvdash``
  `\ntriangleleft`      ``\ntriangleleft``       `\Subset`           ``\Subset``
  ===================== =======================  =================== =====================  =================== =====================

Synonyms: `\owns` ``\owns``.

Symbols can be negated prepending ``\not``, e.g.
`\not\in` ``\not\in``, `\not\ni` ``\not\ni``.

The commands ``\varsubsetneq``, ``\varsubsetneqq``, ``\varsupsetneq``,
and ``\varsupsetneqq`` are not supported with MathML output as there is no
corresponding Unicode character.


Variable-sized operators
------------------------
.. class:: colwidths-auto

  =========================  =========================  =========================  ===========================
  `\sum`      ``\sum``       `\prod`     ``\prod``      `\bigcap`   ``\bigcap``    `\bigodot`   ``\bigodot``
  `\int`      ``\int``       `\coprod`   ``\coprod``    `\bigcup`   ``\bigcup``    `\bigoplus`  ``\bigoplus``
  `\oint`     ``\oint``      `\bigwedge` ``\bigwedge``  `\biguplus` ``\biguplus``  `\bigotimes` ``\bigotimes``
  `\smallint` ``\smallint``  `\bigvee`   ``\bigvee``    `\bigsqcup` ``\bigsqcup``
  =========================  =========================  =========================  ===========================

Larger symbols are used in displayed formulas, sum-like symbols have
indices above/below the symbol:

.. math:: \sum_{n=1}^N a_n \qquad
          \int_0^1f(x)\,dx \qquad
          \prod_{i=1}^{10} b_i \ldots


Notations
=========

Top and bottom embellishments
-----------------------------

See `Accents and embellishments`_.


Extensible arrows
-----------------

\xleftarrow and \xrightarrow produce arrows that extend automatically to
accommodate unusually wide subscripts or superscripts. These commands
take one optional argument (the subscript) and one mandatory argument
(the superscript, possibly empty)::

  A \xleftarrow{n+\mu-1} B \xrightarrow[T]{n\pm i-1} C

results in

.. math:: A \xleftarrow{n+\mu-1} B \xrightarrow[T]{n\pm i-1} C


Affixing symbols to other symbols
---------------------------------

In addition to the standard `accents and embellishments`_, other symbols
can be placed above or below a base symbol with the ``\overset`` and
``\underset`` commands. The symbol is set in "scriptstyle" (smaller font
size). For example, writing ``\overset{*}{X}`` becomes `\overset{*}{X}`
and ``\underset{+}{M}`` becomes `\underset{+}{M}`.


Matrices
--------

The ``matrix`` and ``cases`` environments can also contain ``\\`` and
``&``::

  .. math::
     \left ( \begin{matrix} a & b \\ c & d \end{matrix}\right)

Result:

.. math::
     \left ( \begin{matrix} a & b \\ c & d \end{matrix} \right)

The environments ``pmatrix``, ``bmatrix``, ``Bmatrix``, ``vmatrix``, and
``Vmatrix`` have (respectively) ( ), [ ], { }, \| \|, and `\Vert\ \Vert`
delimiters built in, e.g.

.. math:: \begin{pmatrix} a & b \\ c & d \end{pmatrix} \qquad
          \begin{bmatrix} a & b \\ c & d \end{bmatrix} \qquad
          \begin{Vmatrix} a & b \\ c & d \end{Vmatrix}

To produce a small matrix suitable for use in text, there is a
``smallmatrix`` environment
`\bigl(\begin{smallmatrix} a & b \\ c & d \end{smallmatrix}\bigr)`
that comes closer to fitting within a single text line than a normal
matrix.


For piecewise function definitions there is a ``cases`` environment:

.. math:: \mathrm{sgn}(x) = \begin{cases}
                                        -1 & x<0\\
                              \phantom{-}1 & x>0
                            \end{cases}


Spacing commands
----------------

Horizontal spacing of elements can be controlled with the following
commands:

.. class:: colwidths-auto

  ======================  ========  =====================  ===================
  :m:`3\qquad 4`                    ``3\qquad 4``          = 2em
  :m:`3\quad 4`                     ``3\quad 4``           = 1em
  :m:`3~4`                ``3~4``   ``3\nobreakspace 4``
  :m:`3\ 4`               ``3\ 4``                         escaped space
  :m:`3\;4`               ``3\;4``  ``3\thickspace 4``
  :m:`3\:4`               ``3\:4``  ``3\medspace 4``
  :m:`3\,4`               ``3\,4``  ``3\thinspace 4``
  :m:`3  4`               ``3  4``                         regular space [#]_
  :m:`3\!4`               ``3\!4``  ``3\negthinspace 4``   negative space [#]_
  :m:`3\negmedspace 4`              ``3\negmedspace 4``
  :m:`3\negthickspace 4`            ``3\negthickspace 4``
  `3\hspace{1ex}4`                  ``3\hspace{1ex}4``     custom length
  `3\mspace{20mu}4`                 ``3\mspace{20mu}4``    custom length [#]_
  ======================  ========  =====================  ===================

.. [#] Whitespace characters are ignored in LaTeX math mode.
.. [#] Negative spacing does not work with MathML (last tested in Firefox 115).
.. [#] In LaTeX, the unit must be 'mu' (1 mu = 1/18em).

There are also three commands that leave a space equal to the height and
width of its argument. For example ``\phantom{XXX}`` results in space as
wide and high as three X’s:

.. math:: \frac{\phantom{XXX}+1}{XXX-1}

The commands ``\hphantom`` and ``\vphantom`` insert space with the
width or height of the argument. They are not supported with `math_output`_
MathML.


Modular arithmetic and modulo operation
---------------------------------------

The commands ``\bmod``, ``\pmod``, ``\mod``, and ``\pod`` deal with the
special spacing conventions of the “mod” notation. [#]_

.. class:: colwidths-auto

  =========  ===========================  =========================
  command    example                      result
  =========  ===========================  =========================
  ``\bmod``  ``\gcd(n,m \bmod n)``        `\gcd(n,m \bmod n)`
  ``\pmod``  ``x\equiv y \pmod b``        `x\equiv y \pmod b`
  ``\mod``   ``x\equiv y \mod c``         `x\equiv y \mod c`
  ``\pod``   ``x\equiv y \pod d``         `x\equiv y \pod d`
  ..         ``\operatorname{mod}(m,n)``  `\operatorname{mod}(m,n)`
  =========  ===========================  =========================

.. [#] Currently `not supported`__ by the "HTML" math_output_ option
       of the HTML writer.

__ https://sourceforge.net/p/docutils/feature-requests/93/


Roots
-----

.. class:: colwidths-auto

  =========  ====================  ==================
  command    example               result
  =========  ====================  ==================
  ``\sqrt``  ``\sqrt{x^2-1}``      `\sqrt{x^2-1}`
  ..         ``\sqrt[3n]{x^2-1}``  `\sqrt[3n]{x^2-1}`
  ..         ``\sqrt\frac{1}{2}``  `\sqrt\frac{1}{2}`
  =========  ====================  ==================


Boxed formulas
--------------

The command ``\boxed`` puts a box around its argument:

.. math:: \boxed{\eta \leq C(\delta(\eta) +\Lambda_M(0,\delta))}


Fractions and related constructions
===================================

The ``\frac`` command takes two ar guments, numerator and denominator,
and typesets them in normal fraction form. For example, ``U = \frac{R}{I}``
produces `U = \frac{R}{I}`. Use ``\dfrac`` or ``\tfrac`` to
force text style and display style respectively.

.. math:: \frac{x+1}{x-1}  \quad
          \dfrac{x+1}{x-1} \quad
          \tfrac{x+1}{x-1}

and in text: `\frac{x+1}{x-1}`, `\dfrac{x+1}{x-1}`, `\tfrac{x+1}{x-1}`.

For binomial expressions such as `\binom{n}{k}`,
there are ``\binom``, ``\dbinom`` and ``\tbinom`` commands::

  2^k-\binom{k}{1}2^{k-1}+\binom{k}{2}2^{k-2}

prints

.. math::  2^k-\binom{k}{1}2^{k-1}+\binom{k}{2}2^{k-2}

The ``\cfrac`` command for continued fractions uses displaystyle and
padding for sub-fractions:

.. math:: \frac{\pi}{4} = 1 + \cfrac{1^2}{
                              2 + \cfrac{3^2}{
                                  2 + \cfrac{5^2}{
                                      2 + \cfrac{7^2}{2 + \cdots}
                              }}}
          \qquad \text{vs.}\qquad
          \frac{\pi}{4} = 1 + \frac{1^2}{
                              2 + \frac{3^2}{
                                  2 + \frac{5^2}{
                                      2 + \frac{7^2}{2 + \cdots}
                              }}}

The optional argument ``[l]`` or ``[r]`` for left or right placement of
the numerator is `not supported by MathML Core`__:

.. math::  \cfrac[l]{x}{x-1} \quad
           \cfrac{x}{x-1}    \quad
           \cfrac[r]{x}{x-1}

__ https://github.com/w3c/mathml/issues/30


Delimiter sizes
===============

Besides the automatic scaling of `extensible delimiters`_ with ``\left``
and ``\right``, there are four commands to manually select delimiters of
fixed size:

.. class:: colwidths-auto

  =========  ==============  ==============  ==============  ==============  ===============  ===============
  Sizing     no              ``\left``       ``\bigl``       ``\Bigl``       ``\biggl``       ``\Biggl``
  command                    ``\right``      ``\bigr``       ``\Bigr``       ``\biggr``       ``\Biggr``
  ---------  --------------  --------------  --------------  --------------  ---------------  ---------------
  Result     `\displaystyle  `\displaystyle  `\displaystyle  `\displaystyle  `\displaystyle   `\displaystyle
             (b)             \left(b\right)  \bigl(b\bigr)   \Bigl(b\Bigr)   \biggl(b\biggr)  \Biggl(b\Biggr)
             (\frac{c}{d})`  \left(\frac{c}  \bigl(\frac{c}  \Bigl(\frac{c}  \biggl(\frac{c}  \Biggl(\frac{c}
                             {d}\right)`     {d}\bigr)`      {d}\Bigr)`      {d}\biggr)`      {d}\Biggr)`
  =========  ==============  ==============  ==============  ==============  ===============  ===============

There are two or three situations where the delimiter size is commonly
adjusted using these commands:

The first kind of adjustment is done for cumulative operators with
limits, such as summation signs. With ``\left`` and ``\right`` the
delimiters usually turn out larger than necessary, and using the ``Big``
or ``bigg`` sizes instead gives better results:

.. math::
   \left[\sum_i a_i\left\lvert\sum_j x_{ij}\right\rvert^p\right]^{1/p}
   \text{ versus }
   \biggl[\sum_i a_i\Bigl\lvert\sum_j x_{ij}\Bigr\rvert^p\biggr]^{1/p}

The second kind of situation is clustered pairs of delimiters, where
\left and \right make them all the same size (because that is adequate to
cover the encompassed material), but what you really want is to make some
of the delimiters slightly larger to make the nesting easier to see.

.. math:: \left((a_1 b_1) - (a_2 b_2)\right)
          \left((a_2 b_1) + (a_1 b_2)\right)
          \quad\text{versus}\quad
          \bigl((a_1 b_1) - (a_2 b_2)\bigr)
          \bigl((a_2 b_1) + (a_1 b_2)\bigr)

The third kind of situation is a slightly oversize object in running
text, such as `\left|\frac{b'}{d'}\right|` where the delimiters produced
by ``\left`` and ``\right`` cause too much line spreading. [#]_ In that case
``\bigl`` and ``\bigr`` can be used to produce delimiters that are larger
than the base size but still able to fit within the normal line spacing:
`\bigl|\frac{b'}{d'}\bigr|`.

.. [#] With MathML, an example would be parentheses
   around a ``smallmatrix`` environment
   `\left(\begin{smallmatrix} a & b \\ c & d \end{smallmatrix}\right)`
   vs. `\Bigl(\begin{smallmatrix} a & b \\ c & d \end{smallmatrix}\Bigr)`.


Text
====

The main use of the command ``\text`` is for words or phrases in a
display. It is similar to ``\mbox`` in its effects but, unlike ``\mbox``,
automatically produces subscript-size text if used in a subscript,
``k_{\text{B}}T`` becomes `k_{\text{B}}T`.

Whitespace is kept inside the argument:

.. Math:: f_{[x_{i-1},x_i]} \text{ is monotonic for }  i = 1,\,…,\,c+1


The text may contain math commands wrapped in ``$`` signs, e.g.

.. math:: (-1)^{n_i} = \begin{cases} -1 \quad \text{if $n_i$ is odd,} \\
                                     +1 \quad \text{if $n_i$ is even.}
                       \end{cases}

.. TODO ignore {}, handle text-mode commands


Integrals and sums
==================

The limits on integrals, sums, and similar symbols are placed either to
the side of or above and below the base symbol, depending on convention
and context. In inline formulas and fractions, the limits on sums, and
similar symbols like

.. math:: \lim_{n\to\infty} \sum_1^n \frac{1}{n}

move to index positions: `\lim_{n\to\infty} \sum_1^n \frac{1}{n}`.


Altering the placement of limits
--------------------------------

The commands ``\intop`` and ``\ointop`` produce integral signs with
limits as in sums and similar: `\intop_0^1`, `\ointop_c` and

.. math:: \intop_0^1 \quad \ointop_c
             \quad \text{vs.} \quad
          \int^1_0   \quad \oint_c

The commands ``\limits`` and ``\nolimits`` override the default placement
of the limits for any operator; ``\displaylimits`` forces standard
positioning as for the \sum command. They should follow immediately after
the operator to which they apply.

Compare the same term with default positions, ``\limits``, and
``\nolimits`` in inline and display mode: `\lim_{x\to0}f(x)`,
`\lim\limits_{x\to0}f(x)`, `\lim\nolimits_{x\to0}f(x)`, vs.

.. math:: \lim_{x\to0}f(x), \quad
          \lim\limits_{x\to0}f(x) \quad
          \lim\nolimits_{x\to0}f(x).

.. TODO: \substack

.. TODO: \sideset


Changing the size of elements in a formula
==========================================

The declarations [#]_ ``\displaystyle``, ``\textstyle``,
``\scriptstyle``, and ``\scriptscriptstyle``, select a symbol size and
spacing that would be applied in display math, inline
math, first-order subscript, or second-order subscript, respectively
even when the current context would normally yield some other size.

For example ``:math:`\displaystyle \sum_{n=0}^\infty
\frac{1}{n}``` is printed as `\displaystyle \sum_{n=0}^\infty \frac{1}{n}`
rather than `\sum_{n=0}^\infty \frac{1}{n}` and ::

  \frac{\scriptstyle\sum_{n > 0} z^n}
  {\displaystyle\prod_{1\leq k\leq n} (1-q^k)}

yields

.. math::

  \frac{\scriptstyle\sum_{n > 0} z^n}
  {\displaystyle\prod_{1\leq k\leq n} (1-q^k)}
  \text{ instead of the default }
  \frac{\sum_{n > 0} z^n}
  {\prod_{1\leq k\leq n} (1-q^k)}.

.. [#] "Declarations" are commands that affect processing of the current
   "group". In particular, notice where the braces fall that delimit the
   effect of the command: Right: ``{\displaystyle ...}`` Wrong:
   ``\displaystyle{...}``.

   With math_output_ MathML, the declaration must be the first element
   after the opening bracket.


.. [ISO-80000-2] :title:`Quantities and units – Part 2: Mathematical signs
   and symbols to be used in the natural sciences and technology`:
   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=31887.
