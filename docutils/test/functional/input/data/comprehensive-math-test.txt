==================================
Using LaTeX syntax for mathematics
==================================

.. role:: m(math)
.. default-role:: math
.. |latex| replace:: L\ :sup:`A`\ T\ :sub:`E`\ X

.. contents::


Introduction
============

Since version 0.8, Docutils supports mathematical content with a `"math"
directive`__ and `role`__.
The input format is *LaTeX math syntax*\ [#math-syntax]_ with support for
Unicode symbols.


.. [#math-syntax] The supported LaTeX commands include AMS extensions
   (see, e.g., the `Short Math Guide`_).

   The support is limited to a subset of *LaTeX math* by the conversion
   required for many output formats.  For HTML, the `math_output`_
   configuration setting (or the corresponding ``--math-output`` command
   line option) select between alternative output formats with different
   subsets of supported elements. If a writer does not support math
   typesetting at all, the content is inserted verbatim.

__ https://docutils.sourceforge.io/docs/ref/rst/directives.html#math
__ https://docutils.sourceforge.io/docs/ref/rst/roles.html#math
.. _Short Math Guide:
   http://mirrors.ctan.org/info/short-math-guide/short-math-guide.pdf
.. _math_output:
   https://docutils.sourceforge.io/docs/user/config.html#math-output


Role and directive
==================

The ``math`` role can be used for inline mathematical expressions:
``:math:`\psi(r) = \exp(-2r)``` will produce :m:`\psi(r)=\exp(-2r)`.
Inside the backtics you can write anything you would write between dollar
signs in a LaTeX document.

For producing displayed math (like an ``equation*`` environment in a
LaTeX document) there is a ``math`` directive.  If you write::

  .. math:: \psi(r) = e^{-2r}

you will get:

.. math:: \psi(r) = e^{-2r}

A more complex example is the definition of the Fourier transform

.. math::

    (\mathcal{F}f)(y) =
        \frac{1}{\sqrt{2\pi}^{\ n}}
        \int_{\mathbb{R}^n} f(x)\,e^{-\mathrm{i} y \cdot x} \,\mathrm{d} x.


.. tip::

  If you put ``.. default-role:: math`` at the top of your
  document, then you can write ```x^2``` instead of the longer
  version: ``:math:`x^2```.  You can also introduce an
  abreviation like this ``.. role:: m(math)``.  That will allow
  you to write ``:m:`x^2``` or ```x^2`:m:``.


Not *all* math syntax constructs work with every output format, but basic
everyday-math should work.

If a command or a special symbol is not desribed in this document, then
it is probably not implemented in the internal LaTeX -> MathML converter.


Commands
========


.. class:: colwidths-auto

  =====================  ==============================  ============================
  command                example                         result
  =====================  ==============================  ============================
  ``\sqrt``              ``\sqrt{x^2-1}``                `\sqrt{x^2-1}`
  ``\frac``              ``\frac{1}{2}``                 `\frac{1}{2}`
  ``\left``, ``\right``  ``\left(\frac{1}{2}\right)^n``  `\left(\frac{1}{2}\right)^n`
  =====================  ==============================  ============================


Environments
============

Displayed math can use ``\\`` and ``&`` for line shifts and alignments::

  .. math::

     a & = (x + y)^2 \\
       & = x^2 + 2xy + y^2

LaTeX output will wrap it in an ``align*`` environment.
The result is:

.. math::

   a & = (x + y)^2 \\
     & = x^2 + 2xy + y^2

The ``matrix`` and ``cases`` environments can also contain ``\\`` and
``&``::

  .. math::

    \left(\begin{matrix} a & b \\ c & d \end{matrix}\right)

Result:

.. math::

  \left(\begin{matrix} a & b \\ c & d \end{matrix}\right)


Mathematical symbols
====================

The following tables are adapted from the first edition of
"The LaTeX Companion" (Goossens, Mittelbach, Samarin).

Accents
-------
.. class:: colwidths-auto

  =========== =============  =========== =============  ============== ================
  `\acute{x}` ``\acute{x}``  `\dot{t}`   ``\dot{t}``    `\hat{H}`      ``\hat{H}``
  `\bar{v}`   ``\bar{v}``    `\ddot{t}`  ``\ddot{t}``   `\mathring{x}` ``\mathring{x}``
  `\breve{x}` ``\breve{x}``  `\dddot{t}` ``\dddot{t}``  `\tilde{n}`    ``\tilde{n}``
  `\check{x}` ``\check{x}``  `\grave{x}` ``\grave{x}``  `\vec{x}`      ``\vec{x}``
  =========== =============  =========== =============  ============== ================

When adding an accent to an i or j in math, dotless variants can be
obtained with ``\imath`` and ``\jmath``: `\bar \imath`, `\hat{\jmath}`
(MathML drops the dot automatically).

For adornment that span multiple symbols, see `top and bottom
embellishments`_.

Font switches
-------------

TeX’s *math alphabets* correspond to the
:t:`mathematical alphanumeric symbols` block in Unicode and the
"mathvariant" `style attribute`__ in MathML. They are “to be used for
mathematical variables where style variations are important
semantically”.

__ https://developer.mozilla.org/en-US/docs/Web/MathML/Attribute

.. class:: colwidths-auto

  ===============  ============================  ==========================
  command          example                       result
  ===============  ============================  ==========================
  ``\mathbf``      ``\mathbf{r}^2=x^2+y^2+z^2``  `\mathbf{r}^2=x^2+y^2+z^2`
  ``\mathbb``      ``\mathbb{R \subset C}``      `\mathbb{R \subset C}`
  ``\mathcal``     ``\mathcal{F}f(x)``           `\mathcal{F}f(x)`
  ``\mathfrak``    ``\mathfrak{a}``              `\mathfrak{a}`
  ``\mathit``      ``\mathit{\Gamma}``           `\mathit{\Gamma}`
  ``\mathrm``      ``s_\mathrm{out}``            `s_\mathrm{out}`
  ``\mathsf``      ``\mathsf x``                 `\mathsf x`
  ``\mathtt``      ``\mathtt{0.12}``             `\mathtt{0.12}`
  ===============  ============================  ==========================

Additional alphabets are defined in LaTeX packages, e.g.

.. class:: colwidths-auto

  ===========  =============  ======================
  TeX command  LaTeX package  MathML "mathvariant"
  ===========  =============  ======================
  mathbfit     isomath_       bold-italic
  mathsfit     isomath_       sans-serif-italic
  mathsfbfit   isomath_       sans-serif-bold-italic
  mathscr      mathrsfs_      script
  ===========  =============  ======================
.. _isomath: https://www.ctan.org/pkg/isomath
.. _mathrsfs: https://www.ctan.org/pkg/mathrsfs

This can be used to typeset vector symbols in **bold** *italic*
in line with the International Standard [ISO-80000-2].
``\mathbfit{r}^2=x^2+y^2+z^2`` becomes

.. math:: \mathbfit{r}^2=x^2+y^2+z^2.



In contrast to the math alphabet selectors, ``\boldsymbol`` only changes
the *font weight*. In LaTeX, it can be used to get a bold version of any
mathematical symbol (for other output formats, results are mixed):

.. math::
     \boldsymbol{abs(x) \pm \alpha \approx 3 \Gamma \quad \forall x \in R}




Arrows
------
.. class:: colwidths-auto

  ===================== =======================  ===================== =======================
  `\leftarrow`          ``\leftarrow``           `\Leftarrow`          ``\Leftarrow``
  `\rightarrow`         ``\rightarrow``          `\Rightarrow`         ``\Rightarrow``
  `\leftrightarrow`     ``\leftrightarrow``      `\Leftrightarrow`     ``\Leftrightarrow``
  `\uparrow`            ``\uparrow``             `\Uparrow`            ``\Uparrow``
  `\downarrow`          ``\downarrow``           `\Downarrow`          ``\Downarrow``
  `\updownarrow`        ``\updownarrow``         `\Updownarrow`        ``\Updownarrow``

  `\longleftarrow`      ``\longleftarrow``       `\Longleftarrow`      ``\Longleftarrow``
  `\longrightarrow`     ``\longrightarrow``      `\Longrightarrow`     ``\Longrightarrow``
  `\longleftrightarrow` ``\longleftrightarrow``  `\Longleftrightarrow` ``\Longleftrightarrow``

  `\nearrow`            ``\nearrow``             `\leftharpoonup`      ``\leftharpoonup``
  `\searrow`            ``\searrow``             `\rightharpoonup`     ``\rightharpoonup``
  `\swarrow`            ``\swarrow``             `\leftharpoondown`    ``\leftharpoondown``
  `\nwarrow`            ``\nwarrow``             `\rightharpoondown`   ``\rightharpoondown``

  `\mapsto`             ``\mapsto``              `\hookleftarrow`      ``\hookleftarrow``
  `\longmapsto`         ``\longmapsto``          `\hookrightarrow`     ``\hookrightarrow``
  ===================== =======================  ===================== =======================


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


Braces
------
.. class:: colwidths-auto

  ============  ============  ============  ==============  ========================
     `(` ``(``     `[` ``[``     `|` ``|``     `\{` ``\{``     `\langle` ``\langle``
     `)` ``)``     `]` ``]``     `|` ``|``     `\}` ``\}``     `\rangle` ``\rangle``
  ============  ============  ============  ==============  ========================


Greek letters
-------------
.. class:: colwidths-auto

  ========== ============  ========== ============  ========== ============  ============== ===============
  `\Gamma`   ``\Gamma``    `\alpha`   ``\alpha``    `\mu`      ``\mu``       `\omega`       ``\omega``
  `\Delta`   ``\Delta``    `\beta`    ``\beta``     `\nu`      ``\nu``       `\backepsilon` ``\backepsilon``
  `\Lambda`  ``\Lambda``   `\gamma`   ``\gamma``    `\xi`      ``\xi``       `\digamma`     ``\digamma``
  `\Omega`   ``\Omega``    `\delta`   ``\delta``    `\pi`      ``\pi``       `\varepsilon`  ``\varepsilon``
  `\Phi`     ``\Phi``      `\epsilon` ``\epsilon``  `\rho`     ``\rho``      `\varkappa`    ``\varkappa``
  `\Pi`      ``\Pi``       `\zeta`    ``\zeta``     `\sigma`   ``\sigma``    `\varphi`      ``\varphi``
  `\Psi`     ``\Psi``      `\eta`     ``\eta``      `\tau`     ``\tau``      `\varpi`       ``\varpi``
  `\Sigma`   ``\Sigma``    `\theta`   ``\theta``    `\upsilon` ``\upsilon``  `\varrho`      ``\varrho``
  `\Theta`   ``\Theta``    `\iota`    ``\iota``     `\phi`     ``\phi``      `\varsigma`    ``\varsigma``
  `\Upsilon` ``\Upsilon``  `\kappa`   ``\kappa``    `\chi`     ``\chi``      `\vartheta`    ``\vartheta``
  `\Xi`      ``\Xi``       `\lambda`  ``\lambda``   `\psi`     ``\psi``
  ========== ============  ========== ============  ========== ============  ============== ===============


Letterlike symbols
------------------
.. class:: colwidths-auto

======= ==========  ============= ===============  ========= ===========  ========== ============
`\Im`   ``\Im``     `\forall`     ``\forall``      `\aleph`  ``\aleph``   `\eth`     ``\eth``
`\Re`   ``\Re``     `\exists`     ``\exists``      `\beth`   ``\beth``    `\hbar`    ``\hbar``
`\mho`  ``\mho``    `\complement` ``\complement``  `\gimel`  ``\gimel``   `\hslash`  ``\hslash``
`\Bbbk` ``\Bbbk``   `\Finv`       ``\Finv``        `\daleth` ``\daleth``  `\imath`   ``\imath``
`\ell`    ``\ell``  `\Game`       ``\Game``        `\nabla`  ``\nabla``   `\partial` ``\partial``
`\wp`    ``\wp``
======= ==========  ============= ===============  ========= ===========  ========== ============


Miscellaneous symbols
---------------------
.. class:: colwidths-auto

======== ===============  =========== =============  ============== ================  ========== ============
`\angle` ``\angle``       `\emptyset` ``\emptyset``  `\clubsuit`    ``\clubsuit``     `\flat`    ``\flat``
`\colon` ``\colon`` [#]_  `\infty`    ``\infty``     `\diamondsuit` ``\diamondsuit``  `\natural` ``\natural``
`\cdots` ``\cdots``       `\neg`      ``\neg``       `\heartsuit`   ``\heartsuit``    `\sharp`   ``\sharp``
`\ddots` ``\ddots``       `\bot`      ``\bot``       `\spadesuit`   ``\spadesuit``
`\ddots` ``\ddots``       `\top`      ``\top``
======== ===============  =========== =============  ============== ================  ========== ============

.. [#] Punctuation (not ratio):
       Compare spacing in `a\colon b\to c` and `a:b = c`.


Named operators
---------------
.. class:: colwidths-auto

  ========= ===========  ====== ========  ====== ========  ======= =========
  `\arccos` ``\arccos``  `\csc` ``\csc``  `\ker` ``\ker``  `\sec`  ``\sec``
  `\arcsin` ``\arcsin``  `\deg` ``\deg``  `\lg`  ``\lg``   `\sin`  ``\sin``
  `\arctan` ``\arctan``  `\det` ``\det``  `\lim` ``\lim``  `\sinh` ``\sinh``
  `\arg`    ``\arg``     `\dim` ``\dim``  `\ln`  ``\ln``   `\sup`  ``\sup``
  `\cos`    ``\cos``     `\exp` ``\exp``  `\log` ``\log``  `\tan`  ``\tan``
  `\cosh`   ``\cosh``    `\gcd` ``\gcd``  `\max` ``\max``  `\tanh` ``\tanh``
  `\cot`    ``\cot``     `\hom` ``\hom``  `\min` ``\min``
  `\coth`   ``\coth``    `\inf` ``\inf``  `\Pr`  ``\Pr``
  ========= ===========  ====== ========  ====== ========  ======= =========

Named operators outside the above list can be typeset with
``\operatorname{name}``, e.g. `\operatorname{sgn}(-3) = -1`.


Relation symbols
----------------
.. class:: colwidths-auto

  ================================  ================================  ================================  ================================
     `\Join` ``\Join``                 `\approx` ``\approx``             `\asymp` ``\asymp``               `\bowtie` ``\bowtie``
     `\cong` ``\cong``                 `\dashv` ``\dashv``               `\doteq` ``\doteq``               `\equiv` ``\equiv``
     `\frown` ``\frown``               `\ge` ``\ge``                     `\geq` ``\geq``                   `\gg` ``\gg``
     `\in` ``\in``                     `\le` ``\le``                     `\leq` ``\leq``                   `\ll` ``\ll``
     `\mid` ``\mid``                   `\models` ``\models``             `\neq` ``\neq``                   `\ni` ``\ni``
     `\parallel` ``\parallel``         `\perp` ``\perp``                 `\prec` ``\prec``                 `\precsim` ``\precsim``
     `\propto` ``\propto``             `\sim` ``\sim``                   `\simeq` ``\simeq``               `\smile` ``\smile``
     `\sqsubset` ``\sqsubset``         `\sqsubseteq` ``\sqsubseteq``     `\sqsupset` ``\sqsupset``         `\sqsupseteq` ``\sqsupseteq``
     `\subset` ``\subset``             `\subseteq` ``\subseteq``         `\succ` ``\succ``                 `\succsim` ``\succsim``
     `\supset` ``\supset``             `\supseteq` ``\supseteq``         `\vdash` ``\vdash``
  ================================  ================================  ================================  ================================

negated relations
.. class:: colwidths-auto

  =============================  ===================
  `\not\in`     ``\not\in``      `\not =` ``\not =``
  `\not \equiv` ``\not \equiv``
  =============================  ===================


Variable-sized symbols
----------------------
.. class:: colwidths-auto

  =================  ===========================  =========================  =========================  =====================
  `\sum` ``\sum``    `\bigodot` ``\bigodot``      `\bigcap` ``\bigcap``      `\bigwedge` ``\bigwedge``  `\prod` ``\prod``
  `\int` ``\int``    `\bigoplus` ``\bigoplus``    `\bigcup` ``\bigcup``      `\bigvee` ``\bigvee``      `\coprod` ``\coprod``
  `\oint` ``\oint``  `\bigotimes` ``\bigotimes``  `\biguplus` ``\biguplus``
  =================  ===========================  =========================  =========================  =====================

Larger symbols are used in displayed formulas:

.. math::

   \sum\ \int\ \oint \bigcap \prod \ldots

Extensible delimiters
---------------------
Unless you indicate otherwise, delimiters in math formulas remain at the
standard size regardless of the height of the enclosed material. To get
adaptable sizes, use ``\left`` and ``\right`` prefixes.

.. math::

   g(A,B,Y) = f \left(A,B,X=h^{[X]}(Y)\right)

Use ``.`` for "empty" delimiters:

.. math::

   A = \left . \frac{1}{1-n}\, \right |_{n=0}^\infty


Top and bottom embellishments
-----------------------------

Visually similar to accents_ but generally applied to multiple symbols.

.. class:: colwidths-auto

  ========================== ============================  =========================== =============================
  `\widetilde{abi}`          ``\widetilde{abi}``           `\widehat{abi}`             ``\widehat{abi}``
  `\overline{abi}`           ``\overline{abi}``            `\underline{abi}`           ``\underline{abi}``
  `\overbrace{abi}`          ``\overbrace{abi}``           `\underbrace{abi}`          ``\underbrace{abi}``
  `\overleftarrow{abi}`      ``\overleftarrow{abi}``       `\underleftarrow{abi}`      ``\underleftarrow{abi}``
  `\overrightarrow{abi}`     ``\overrightarrow{abi}``      `\underrightarrow{abi}`     ``\underrightarrow{abi}``
  `\overleftrightarrow{abi}` ``\overleftrightarrow{abi}``  `\underleftrightarrow{abi}` ``\underleftrightarrow{abi}``
  ========================== ============================  =========================== =============================

Extensible arrows
-----------------

TODO


Text
====

The main use of the command ``\text`` is for words or phrases in a
display. It is similar to ``\mbox`` in its effects but, unlike ``\mbox``,
automatically produces subscript-size text if used in a subscript,
``k_{\text{B}}T`` becomes `k_{\text{B}}T`.

Whitespace is kept inside the argument:

.. Math:: f_{[x_{i-1},x_i]} \text{ is monotonic for }  i = 1,\,…,\,c+1


Currently, math in text is not supported by LaTeX2MathML.

Horizontal space
================

.. class:: colwidths-auto

  =================  ==================  =============
  :m:`|\qquad|`      ``|\qquad|``        2 em
  :m:`|\quad|`       ``|\quad|``         1 em
  :m:`|\;|`          ``|\;|``            thick
  :m:`|\ |`          ``|\ |``            standard
  :m:`|\:|`          ``|\:|``            medium
  :m:`|\,|`          ``|\,|``            thin
  :m:`|  |`          ``|  |``            none
  :m:`|\!|`          ``|\!|``            thin negative
  `|\hspace{1ex}|`   ``|\hspace{1ex}|``  custom
  =================  ==================  =============

ToDo
====

internal LaTeX2MathML

* Math inside text: ``n - 1 \text{if $n$ is odd}``.
* Remove circular refs.
* Decimal numbers.

* Shorthands for combined named operators

  ==========  ============  =============  ===============  ==================
  `\liminf`   ``\liminf``   `\varliminf`   ``\varliminf``   `\underline{\lim}`
  `\limsup`   ``\limsup``   `\varlimsup`   ``\varlimsup``   `\overline{\lim}`
  `\injlim`   ``\injlim``   `varinjlim`    ``\varinjlim``
  `\projlim`  ``\projlim``  `varprojlim`   ``\varprojlim``
  ==========  ============  =============  ===============  ==================

* Implement ``\circledS``? (in short-math-guide.pdf but not in mathematical Unicode characters)


Tests
==========

Font changes
------------

Math alphabet macros change the default alphabet ("mathvariant" in
MathML), leaving some symbols unchanged:

:normal: `abs(x) \pm \alpha \approx 3 \Gamma  \quad \forall x \in R`
:mathrm: `\mathrm{abs(x) \pm \alpha \approx 3 \Gamma  \quad \forall x \in R}`
:mathit: `\mathit{abs(x) \pm \alpha \approx 3 \Gamma  \quad \forall x \in R}`
:mathsf: `\mathsf{abs(x) \pm \alpha \approx 3 \Gamma  \quad \forall x \in R}`
:mathbb: `\mathbb{abs(x) \pm \alpha \approx 3 \Gamma  \quad \forall x \in R}`
:mathbf: `\mathbf{abs(x) \pm \alpha \approx 3 \Gamma  \quad \forall x \in R`

Unicode supports the following blackboard-bold characters:
`\mathbb{a \ldots z A \ldots Z 0 \ldots 9
\mathbb\Gamma \mathbb{\Pi} \mathbb {\Sigma}\mathbb\gamma \mathbb\pi}`.

The package mathrsfs_ (and some drop-in replacements) define the ``\mathscr``
macro that selects a differently shaped "script" alphabet.
Compare `\mathscr{A, B, …, Z, a, b, …, z}`
with `\mathcal{A, B, …, Z, a, b, …, z}`.

.. _rsfs: https://ctan.org/pkg/mathrsfs

Inferred <mrow>s in MathML
~~~~~~~~~~~~~~~~~~~~~~~~~~

The elements <msqrt>, <mstyle>, <merror>, <mpadded>, <mphantom>, <menclose>,
<mtd, mscarry>, and <math> treat their contents as a single inferred mrow
formed from all their children.

.. math:: a = \sqrt 2, b = \sqrt{1+x^2}, c = \sqrt\frac{sin(x)}{23}

inline: :math:`a = \sqrt 2, b = \sqrt{1+x^2}, c = \sqrt\frac{sin(x)}{23}`.

Accents vs. embellishments
--------------------------

MathML drops dots on "i" and "j" with accents:

.. math:: \vec i \ne \overrightarrow i
          \text{ and } \vec\lim \ne \overrightarrow\lim.

Accents should be nearer to the base (in Firefox 78, it's vice versa!):

.. math:: \vec a \vec l \ne \overrightarrow a \overrightarrow l

          \bar a \bar l \ne \overline a \overline l
