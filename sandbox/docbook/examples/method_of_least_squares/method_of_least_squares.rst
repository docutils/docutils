The Method of Least Squares 
++++++++++++++++++++++++++++

.. default-role:: math

:author: Steven J. Miller [*]_
:Address: Mathematics Department 
          Brown University 
          Providence, RI 02912
:Abstract:

 .. compound::  

        The Method of Least Squares  is a procedure to determine the best fit
        line to data; the proof uses simple calculus and linear algebra. The
        basic problem is to find the best fit straight line `y = ax + b` given
        that, for `n ∈ {1,...,N}`, the pairs `(x_n,y_n)` are observed. The method
        easily generalizes to finding the best fit of the form


        .. math::

          y = a_1f_1(x)+···+c_Kf_K(x); 

        it is not necessary for the functions fk to be linearly in x – all
        that is needed is that y is to be a linear combination of these
        functions.

Description of the Problem
===========================

Often in the real world one expects to find linear relationships
between variables. For example, the force of a spring linearly depends
on the displacement of the spring: `y = kx` (here y is the force, x is
the displacement of the spring from rest, and k is the spring
constant). To test the proposed relationship, researchers go to the
lab and measure what the force is for various displacements. Thus they
assemble data of the form `(x_n,y_n)` for `n ∈ {1,...,N};` here `y_n` is the
observed force in Newtons when the spring is displaced `x_n` meters.


.. figure:: force_displacment.png

   100 “simulated” observations of displacement and force (k = 5).

Unfortunately, it is extremely unlikely that we will observe a perfect
linear relationship. There are two reasons for this. The first is
experimental error; the second is that the underlying relationship may
not be exactly linear, but rather only approximately linear. See
Figure 1 for a simulated data set of displacements and forces for a
spring with spring constant equal to 5.

The Method of Least Squares is a procedure, requiring just some
calculus and linear algebra, to determine what the “best fit” line
is to the data. Of course, we need to quantify what we mean by “best
fit”, which will require a brief review of some probability and
statistics.

.. compound::  

 A careful analysis of the proof will show that the method is capable
 of great generaliza- tions. Instead of finding the best fit line, we
 could find the best fit given by any finite linear combinations of
 specified functions. Thus the general problem is given functions 
 `f_1 ,...,f_K` , find values of coefficients `a_1,...,a_K`
 such that the *linear* combination 



 .. math::

   y = a_1f_1(x)+···+a_Kf_K(x)  

 is the best approximation to the data.

Probability and Statistics Review
=====================================

We give a quick introduction to the basic elements of probability and
statistics which we need for the Method of Least Squares; for more
details see [BD, CaBe, Du, Fe, Kel, LF, MoMc].

.. compound::

 Given a sequence of data `x1,..., x_N` , we define the **mean** (or
 the **expected value**) to be 2 `(x_1 + ··· + x_N)//N`. We denote
 this by writing a line above *x*: thus 

 .. math::

     bar x = 1/N sum_(n=1)^N x_n.

 The mean is the average value of the data.

.. compound::

 Consider the following two sequences of data: `{10, 20, 30, 40, 50}` and
 `{30, 30, 30, 30, 30}`.  Both sets have the same mean; however, the
 first data set has greater variation about the mean. This leads to the
 concept of variance, which is a useful tool to quantify how much a set
 of data fluctuates about its mean. The variance of `{x1, ... , x_N }`,
 denoted by `σ_x^2`, is

 .. math::

   σ_x^2 = 1/N sum_(n=1)^N(x_i - bar x)^2;

 the standard deviation `σ_x` is the square root of the variance:

 .. math::

   σ_x^2 = sqrt(1/N sum_(n=1)^N(x_i - bar x)^2).

.. compound::

 Note that if the x’s have units of meters then the variance `σ_x^2` has
 units of meters\ :sup:`2`, and the standard deviation `σ_x` and the mean x have
 units of meters. Thus it is the standard deviation that gives a good
 measure of the deviations of the x’s around their mean.  There are, of
 course, alternate measures one can use. For example, one could
 consider 

 .. math::

   1/N sum_(n=1)^N(x_i - bar x).

Unfortunately this is a signed quantity, and large positive deviations
can cancel with large negatives. In fact, the definition of the mean
immediately implies the above is zero! This, then, would be a terrible
measure of the variability in data, as it is zero regardless of what
the values of the data are.

We can rectify this problem by using absolute values. This leads us to
consider

.. math::
   :name: absolute-method

   1/N sum_(n=1)^N|x_i - bar x|.

While this has the advantage of avoiding cancellation of errors (as
well as having the same units as the *x* ’s), the absolute value
function is not a good function analytically. It is not
differentiable. This is primarily why we consider the standard
deviation (the square root of the variance)–this will allow us to use
the tools from calculus.

.. compound::

 We can now quantify what we mean by “best fit”. If we believe 
 `y = ax+b`, then `y−(ax+b)` should be zero. Thus given observations

 .. math::
 
   {(x_1,y_1), ..., (x_N,y_N)},

 we look at

 .. math::

   {y_1 −(a_(x1) +b), ..., y_N −(ax_N +b)}.

 The mean should be small (if it is a good fit), and the variance will
 measure how good of a fit we have.

.. compound:: 

 Note that the variance for this data set is

 .. math::

   sigma_(y - (ax + b))^2 = 1/N sum_(n = 1)^N (y_n − (ax_n + b))^2 .

Large errors are given a higher weight than smaller errors (due to the
squaring). Thus our pro- cedure favors many medium sized errors over a
few large errors. If we used absolute values to measure the error
(see absolute-method_), then all errors are weighted equally; however, the
absolute value function is not differentiable, and thus the tools of
calculus become inaccessible.

.. [*] E-mail: sjmiller@math.brown.edu 

