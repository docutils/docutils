..  WARNING TO PAUL: DON'T EDIT THIS FILE use update.py instead.
Example of reStructure document that can be converted to docbook
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. author: Paul Tremblay Won't work like that

:author:

  :first: Paul

  :surname: Tremblay

  :email: phthenry@earthlink.net


:revision:

  :revnumber: 01

  :date: 2003-04-24

  :revremark: When I first started writing this document.

:revision:
  
  :revnumber: 02

  :date: 2003-05-01

  :revremark: Made the following changes

.. start body

========================
Using snippets of code
========================

How to represent a snippet of code:

:example program:

  :title: Processing a variable

  ::

   if var = 5
     self.__process(var)


Keep in mind that the docbook should look like:

::

  <example>
    <title>A formal example</title>
    <programlisting>
      if var = 5
      self.__process(var)
    </programlisting>
  </example>

.. end of example



