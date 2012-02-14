Document tests mathml

.. class:: asciimath

.. math::

 quad quad x = (-b +- sqrt(b^2-4ac))/(2a)

Docutils supports inline math with the prefix or postfix ``:math:`` 
role specificator, :math:`n! + sin(x_n^2)` and :math:`A_c=pi/4d^2`, 
as well as displayed math via the `math` directive:

.. math::

 f(epsi)=1/(1 + exp (epsi/(k_BT)))


Content may start on the first line of the directive, e.g.

.. math:: N = (text(Number of Apples))/7

The determinant of the matrix

.. math:: :name: eq:M

   M = ((a,b),(c,d))

is :math:`|f(M)| = ad - bc`.

More than one display math block can be put in one math directive.
For example, the following sum and integral with limits:

.. math::

          int_0^1 x^n dx = 1/(n + 1)

          sum_(n=1)^m n = (m(m+1))/2

LaTeX-supported Unicode math symbols can be used in math roles and
directives:

The Schrödinger equation

.. math:: :name: eq:schrödinger

   ɩħ ∂/(∂t)ψ = Ĥ∂


.. iotaħ ∂/(∂t)ψ = Ĥ∂

with the *wave function* :math:`Ψ`, describes how the quantum state of a
physical system changes in time.

.. can put all the info on save function in here to test things out

For a single particle with potential energy V in position space, the
Schrödinger equation takes the form:

.. math::

 ɩħ ∂/(∂t)ψ(r,t) = Ĥ∂ = (-ħ/(2m) grad^2 + V(r)) ψ(r,t) = -ħ/(2m)grad^2ψ(r,t) + V(r) ψ(r,t)

And for N particles, the difference is that the wavefunction is in 3N-dimensional configuration space, the space of all possible particle positions.

.. math::

 ɩħ ∂/(∂t)ψ(x_1,cdots,x_n,t)=ħ(grad_1^2/(2m_1) - grad_2^2/(2m_2)cdots -grad_1^2/(2m_n))ψ(x_1,cdots,x_n,t) + V(x_1,cdots,x_n,t)ψ(x_1,cdots,x_n,t)

Modulation Transfer Function:

This yields the **edge spread function (ESF).**

.. math::

 sigma = sqrt(sum_(i=0)^(n-1)(x_i-mu)^2)/n

The Fourier transform of the LSF can not be determined analytically by the following equations:

.. math::


 MTF=intf(x)e^(-i2pi xs)dx
