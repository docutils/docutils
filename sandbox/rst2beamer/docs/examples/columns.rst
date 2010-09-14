===========================
Using columns in rst2beamer
===========================

Usage
-----

The LaTeX source for the corresponding Beamer example can be produced::

	rst2beamer columns.rst columns.tex


Simple columns
--------------

.. r2b-simplecolumns::
	:width: 0.95

	This is a demonstration of the rst2beamer simple column directive. It
	should turn every element underneath it into a column, side by side with
	each other.
	
	So here, we should end up with three columns of equal width, occupying 0.95
	of the page in total. The 'width' argument is optional and defaults to
	0.90.
	
	* A list or image element can be 
	* a
	* column


Containers as columns
---------------------

.. container:: r2b-simplecolumns

	The custom r2b directives won't be recognised by any writer other than
	rst2beamer. Therefore, we allow certain containers (which most other
	writers should recognise and at worst ignore) to act like column sets.

	Any container with the name 'r2b-simplecolumns' or 'r2b-simplecolumns' will
	be handled like the simple columns directive. 


Explicit columns
----------------

.. r2b-columnset::
	:width: 0.95

	.. r2b-column::
		:width: 0.60
		
		If you insist on setting columns explicitly, you can, grouping multiple
		elements.
		
		The width of the column set and individual columns can be given. This
		set and column are 0.95 and 0.60 wide respectively.
		
	.. r2b-column::
	
		Columns not given a width (like this one) share the remainder. 
	
	
A slide without columns
-----------------------

Just to make sure our "columnization" isn't persistent, this should show up
up as a plain normal slide.

Has it?

