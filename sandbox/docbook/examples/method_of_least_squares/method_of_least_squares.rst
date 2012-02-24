The Method of Least Squares 
++++++++++++++++++++++++++++

:author: Steven J. Miller [*]_
:Address: Mathematics Department 
          Brown University 
          Providence, RI 02912
:Abstract:

        The Method of Least Squares  is a procedure to determine the best fit
        line to data; the proof uses simple calculus and linear algebra. The
        basic problem is to find the best fit straight line :math:`y = ax + b` given
        that, for :math:`n ∈ {1,...,N}`, the pairs :math:`(x_n,y_n)` are observed. The method
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
on the displacement of the spring: :math:`y = kx` (here y is the force, x is
the displacement of the spring from rest, and k is the spring
constant). To test the proposed relationship, researchers go to the
lab and measure what the force is for various displacements. Thus they
assemble data of the form (xn,yn) for :math:`n ∈ {1,...,N};` here :math:`y_n` is the
observed force in Newtons when the spring is displaced :math:`x_n` meters.

.. [*] E-mail: sjmiller@math.brown.edu 

.. [*] two 
