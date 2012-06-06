Title
++++++++++++++++++++++++++++

.. default-role:: math

.. role:: page-break

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


:page-break:`empty`

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
   :alt: the title

   100 “simulated” observations of displacement and force (k = 5).

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



.. class:: long-metrics
.. csv-table:: Singulate 2010-2011
     :file: singles.csv
     :header-rows: 1

.. container:: caption

    Data for the singles process, 2010-2011. 

Another Table

.. class:: costs
.. csv-table:: Total Costs
   :header: "Item", "Cost"
   :widths: 15, 10
   :name: total-costs

    "IT Costs",403002
    "Labor",2454972
    "System Costs",1483027
    "SLAM Lines",1400000
    "Put-To-Light",900000
    "Total",6511518

.. container:: caption

    Total Costs. 

Table with date

.. class:: date
.. csv-table:: Total Costs
   :header: "Date", "Item", "Cost"
   :widths: 10, 15, 10
   :name: total-costs-dates

    "2012-05-01", "IT Costs",403002
    "2012-05-02", "Labor",2454972
    "2012-05-03", "System Costs",1483027
    "2012-05-04", "SLAM Lines",1400000
    "2012-05-05", "Put-To-Light",900000
    "2012-05-06", "Total",6511518

.. container:: caption

    Total Costs. 

.. class:: Glossary


========
Glossary
========

:CLT: Zappos Customer Loyalty Team, the team devoted to providing service to
      Zappos customers.

:Cutover: The transition from Zappos warehouse management to Amazon warehouse
          management.

.. raw:: xml
 :file: bibliography.xml

.. [*] E-mail: sjmiller@math.brown.edu 
