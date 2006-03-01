==============
 Latex README
==============

.. role:: m(latex)

Overview
========

The idea is to convert latex fragments into *png* for display in html
model.

Role and directive
==================

There is a role called ``latex`` that can be used for inline
latex expressions: ``:latex:`$\psi(r) = \exp(-2r)$``` will
produce :m:`$\psi(r)=\exp(-2r)$`.  Inside the back-tics you can write
anything you would write in a LaTeX ducument.

For producing displayed math (like an ``equation*`` environment in a
LaTeX document) there is a ``latex`` role.  If you write::

  .. latex-math::

     $\psi(r) = e^{-2r}$

you will get:

.. latex::

   $\psi(r) = e^{-2r}$



Example
=======

Role
----

You can use roles for inline stuff:  :latex:`$e=mc^2$`.  Lots of fun.

Or how about: :m:`$x = (-b \pm \sqrt{b^2 - 4ac}) / 2a$`

Directive
---------



.. latex::
   
   $$-1 = e^{i\pi}$$
   
More info
=========

No more info at this time.
   

