Math Conversion Tests
=====================

.. default-role:: math

Math alphabet / Mathvariant
~~~~~~~~~~~~~~~~~~~~~~~~~~~

TeX's *math alphabet* macros change font features of (some) letters and
digits. Non-alphanumerical symbols, function names, and mathematical text
are left unchanged.

:default:    `V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}`
:mathrm:     `\mathrm{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathbf:     `\mathbf{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathit:     `\mathit{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathbfit:   `\mathbfit{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathbb:     `\mathbb{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathcal:    `\mathcal{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathscr:    `\mathscr{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathfrak:   `\mathfrak{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathsf:     `\mathsf{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathsfit:   `\mathsfit{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathsfbfit: `\mathsfbfit{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathbfsfit: `\mathbfsfit{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`
:mathtt:     `\mathtt{V_R x \pm \cos(\alpha_\text{out}) \approx 3 \Gamma
             \quad \forall x \in \mathbb{R}}`

MathML 4 defines the *mathvariant* `style attribute`_, in MathML core,
characters from the `Mathematical Alphanumeric Symbols`_ Unicode block
are used.

The mathvariants *bold-script*, *bold-fraktur*, and *sans-bold* are
rarely used. They are currently not supported by Docutils's native MathML
output. In LaTeX, ``\mathbfscr`` and ``\mathbffrac`` is supported by the
`mathalpha`_ package.

Bold variants of any symbol can be achieved in combination with the
``\boldsymbol`` command, e.g.

:bold-fraktur: `\boldsymbol{\mathfrak{V_R x}}`

Note, that ``\boldsymbol`` emboldens also non-alphanumerical symbols.


Complex Example
"""""""""""""""

From the baskervaldx_ package documentation:

Simplest form of the *Central Limit Theorem*:
  Let `X_1, X_2,\cdots` be a sequence of iid random variables with mean `0`
  and variance `1` on a probability space `(\Omega,\mathcal{F},\mathbb{P})`.
  Then

  .. math:: \mathbb{P}\left(\frac{X_1+\cdots+X_n}{\sqrt{n}} \le y\right)
            \to\mathfrak{N}(y)
            \coloneq \int_{-\infty}^y
              \frac{\mathrm{e}^{-t^2/2}}{\sqrt{2\mathrm{\pi}}}\, \mathrm{d}t
            \quad\mbox{as $n\to\infty$,}

  or, equivalently, letting `S_n\coloneq\sum_1^n X_k`,

  .. math:: \mathbb{E} f\left(S_n/\sqrt{n}\right)
            \to \int_{-\infty}^\infty f(t)
              \frac{\mathrm{e}^{-t^2/2}}{\sqrt{2\mathrm{\pi}}}\, \mathrm{d}t
            \quad\mbox{as $n\to\infty$,
              for every $f\in\mathrm{b} \mathcal{C}(\mathbb{R})$.}


Inferred <mrow>s in MathML
~~~~~~~~~~~~~~~~~~~~~~~~~~

The elements <msqrt>, <mstyle>, <merror>, <mpadded>, <mphantom>, <menclose>,
<mtd>, <mscarry>, and <math> treat their contents as a single inferred mrow
formed from all their children.

.. math:: a = \sqrt 2 + x,\quad
          b = \sqrt{1+x^2},\quad
          c = \sqrt\frac{\sin(x)}{23},

inline: :math:`a = \sqrt 2 + x, b = \sqrt{1+x^2}, c = \sqrt\frac{\sin(x)}{23}`.


Scripts and Limits
~~~~~~~~~~~~~~~~~~

Accents should be nearer to the base:
`\bar a \overline a, \bar l \overline l, \bar i \overline i`,
`\vec{r}` `\overrightarrow{r}`.

Sub- and superscript may be given in any order:
`x_i^j = x^j_i` and `\int_0^1 = \int^1_0`.

Double exponent: `x^{10^4}`, `r_{T_\mathrm{in}}` and `x_i^{n^2}`.


Nested groups
~~~~~~~~~~~~~

tex-token returns "{" for nested groups:

.. math:: \text{das ist ein  {toller} text (unescaped \{ and \} is
                ignored by LaTeX)}


Big delimiters and symbols
~~~~~~~~~~~~~~~~~~~~~~~~~~
Compare automatic sizing with fixed sizes:

.. math:  \left( \frac{\frac1x}{\frac{1}{n}}\right) &= \Biggl(\text{Bigg}\Biggr)\\


.. math::
  \left( 3                          \right)
  \left( f(x)                       \right)
  \left( \bar x                     \right)
  \left( \overline x                \right)
  \left( n_i                        \right) &= () \\
  \left( \underline x               \right) &= \bigl(\text{big}\bigr)\\
  \left( 3^2                        \right)
  \left( \sqrt{3}                   \right)
  \left( \sqrt{3^2}                 \right)
  \left( \sum                       \right)
  \left( \bigotimes                 \right)
  \left( \prod                      \right) &= \Bigl(\text{Big}\Bigr)\\
  \left( \frac{3  }{2}              \right)
  \left( \frac{3^2}{2^4}            \right)
  \binom{3  }{2}
  \begin{pmatrix} a & b \\ c & d \end{pmatrix}
  \left( \frac{1}{\sqrt 2}          \right)
  \left( \int                       \right)
  \left( \int_0                     \right)
  \left( \int^1                     \right)
  \left( \int_0^1                   \right) &= \biggl(\text{bigg}\biggr)\\
  \left( \frac{\sqrt 2}{2}          \right)
  \left( \sum_0                     \right)
  \left( \sum^1                     \right)
  \left( \sum_0^1                   \right)
  \left( \frac{\frac1x}{\frac{1}{n}}\right) &= \Biggl(\text{Bigg}\Biggr)\\
  \left( \intop_0                   \right)
  \left( \intop^1                   \right)
  \left( \intop_0^1                 \right)

And in text:

:`()`:                        `\left(3                          \right)
                              \left( f(x)                       \right)
                              \left( \bar x                     \right)
                              \left( \overline x                \right)
                              \left( n_i                        \right)
                              \left( \sum                       \right)
                              \left( \sum_0                     \right)
                              \left( \prod                      \right)`


:`\bigl(\text{big}\bigr)`:    `\left(\underline x               \right)
                              \left( 3^2                        \right)
                              \binom{3}{2}
                              \left(\begin{smallmatrix} a & b \\
                              c & d \end{smallmatrix}           \right)
                              \left( \bigotimes                 \right)`

:`\Bigl(\text{Big}\Bigr)`:    `\left(\sqrt{3}                   \right)
                              \left( \sqrt{3^2}                 \right)
                              \left( \frac{3}{2}                \right)
                              \left( \frac{3^2}{2^4}            \right)
                              \left( \frac{\sqrt 2}{2}          \right)
                              \left( \int                       \right)
                              \left( \int_0                     \right)
                              \left( \int^1                     \right)
                              \left( \int_0^1                   \right)
                              \left( \sum^1                     \right)
                              \left( \sum_0^1                   \right)
                              \left( \frac{\frac1x}{\frac{1}{n}}\right)`





Test ``\left``, ``\right``, and the  \bigl/\bigr, … size commands
with all extensible delimiters.

pairing:

.. math::
   \left.(       b \right)       \ \bigl(       b \Bigr)       \ \biggl(       b \Biggr)
   \quad
   \left.[       b \right]       \ \bigl[       b \Bigr]       \ \biggl[       b \Biggr]
   \quad
   \left.\{      b \right\}      \ \bigl\{      b \Bigr\}      \ \biggl\{      b \Biggr\}
   \quad
   \left.\langle b \right\rangle \ \bigl\langle b \Bigr\rangle \ \biggl\langle b \Biggr\rangle

   \left.\lceil  b \right\rceil  \ \bigl\lceil  b \Bigr\rceil  \ \biggl\lceil  b \Biggr\rceil
   \quad
   \left.\lfloor b \right\rfloor \ \bigl\lfloor b \Bigr\rfloor \ \biggl\lfloor b \Biggr\rfloor
   \quad
   \left.\lvert  b \right\rvert  \ \bigl\lvert  b \Bigr\rvert  \ \biggl\lvert  b \Biggr\rvert
   \quad
   \left.\lVert  b \right\rVert  \ \bigl\lVert  b \Bigr\rVert  \ \biggl\lVert  b \Biggr\rVert

   \left.\lgroup b \right\rgroup \ \bigl\lgroup b \Bigr\rgroup \ \biggl\lgroup b \Biggr\rgroup
   \quad
   \left.\lmoustache b \right\rmoustache \ \bigl\lmoustache b \Bigr\rmoustache \ \biggl\lmoustache b \Biggr\rmoustache
   \quad
   \left./           b \right\backslash  \ \bigl/           b \Bigr\backslash  \ \biggl/           b \Biggr\backslash

non-pairing:

.. math::
   \left.|          b \right|          \ \bigl|          b \Bigr|          \ \biggl|          b \Biggr|
   \quad
   \left.\vert      b \right\vert      \ \bigl\vert      b \Bigr\vert      \ \biggl\vert      b \Biggr\vert
   \quad
   \left.\|         b \right\|         \ \bigl\|         b \Bigr\|         \ \biggl\|         b \Biggr\|
   \quad
   \left.\Vert      b \right\Vert      \ \bigl\Vert      b \Bigr\Vert      \ \biggl\Vert      b \Biggr\Vert

   \left.\arrowvert b \right\arrowvert \ \bigl\arrowvert b \Bigr\arrowvert \ \biggl\arrowvert b \Biggr\arrowvert
   \quad
   \left.\Arrowvert b \right\Arrowvert \ \bigl\Arrowvert b \Bigr\Arrowvert \ \biggl\Arrowvert b \Biggr\Arrowvert
   \quad
   \left.\bracevert b \right\bracevert \ \bigl\bracevert b \Bigr\bracevert \ \biggl\bracevert b \Biggr\bracevert

Variable-sized operators:

Inline: `\int\ \iint\ \iiint\ \iiiint\ \idotsint \oint\ \smallint\
\sum\ \prod\ \coprod\ \bigwedge\ \bigvee\ \bigcap\ \bigcup\ \biguplus\
\bigsqcup\ \bigodot\ \bigoplus\ \bigotimes` and Display:

.. math:: \int\ \iint\ \iiint\ \iiiint\ \idotsint\ \oint\ \smallint\
   \sum\ \prod\ \coprod\ \bigwedge\ \bigvee\ \bigcap\ \bigcup\
   \biguplus\ \bigsqcup\ \bigodot\ \bigoplus\ \bigotimes

.. math:: \int_1 f\ \intop_1 f\ \iint_1 f\ \smallint_1 f\ \sum_1\
   \prod_1\ \bigwedge_1\ \bigcap_1\ \biguplus_1\ \bigodot_1\ \int^N\
   \intop^N\ \iiiint^N\ \oint^N\ \smallint^N\ \sum^N\ \coprod^N\
   \bigvee^N\ \bigcup^N\ \bigsqcup^N\ \bigotimes^N

.. math:: \int_1^N\ \intop_1^N\ \iint_1^N\ \iiint_1^N\ \iiiint_1^N\
   \idotsint_1^N\ \oint_1^N\ \smallint_1^N\ \sum_1^N\ \prod_1^N\
   \coprod_1^N\ \bigwedge_1^N\ \bigvee_1^N\ \bigcap_1^N\ \bigcup_1^N
   \ \biguplus_1^N\ \bigsqcup_1^N\ \bigodot_1^N\ \bigoplus_1^N\
   \bigotimes_1^N


Text
~~~~

The text may contain non-ASCII characters: `n_\text{Stoß}`.

Some text-mode LaTeX commands are supported with math_output_ "html".
In other output formats, use literal Unicode: `\text{ç é è ë ê ñ ů ž ©}`
to get the result of the accent macros
`\text{\c{c} \'e \`e \"e \^e \~n \r{u} \v{z} \textcircled{c}}`.


.. _math_output:
   https://docutils.sourceforge.io/docs/user/config.html#math-output
.. _style attribute: https://www.w3.org/TR/mathml4/#presm_commatt
.. _Mathematical Alphanumeric Symbols:
    http://www.unicode.org/charts/PDF/U1EE00.pdf
.. _mathalpha: https://www.ctan.org/pkg/mathalpha
.. _baskervaldx: https://www.ctan.org/pkg/baskervaldx
