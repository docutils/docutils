=================
Test all features
=================

Introduction
------------

Purpose
~~~~~~~

The document is intended to test most features of rst2beamer in one go, such
any new versions can be quickly verified by calling::

	rst2beamer test_all.rst test_all.tex
	
However testing certain options will require running rst2beamer with flags.
These are marked below.


Testing sections
----------------

About testing sections
~~~~~~~~~~~~~~~~~~~~~~

If the sections feature works, this should be the first (and only) slide
"About testing sections" in the section "Testing sections".

Note you may have to run LaTeX twice to get the section names to update
correctly.


Testing columns
----------------

Simple columns
~~~~~~~~~~~~~~

.. r2b-simplecolumns::

	This is a demonstration of the rst2beamer simple column directive. It
	should turn every element underneath it into a column, side by side with
	each other.
	
	So here, we should end up with three columns of equal width, occupying 0.90
	of the page in total (the default).
	
	* Does it
	* handle lists
	* properly?


Simple columns with a set width
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. r2b-simplecolumns::
	:width: 0.70

	This is a demonstration of the rst2beamer simple column directive with a
	set width.

	The total width has been set to 0.70.
		

Testing containers as columns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. container:: r2b-simplecolumns

	This uses a container to set out the columns.

	There should be two columns taking up 0.90 of the page. 


Testing explicit columns
~~~~~~~~~~~~~~~~~~~~~~~~

.. r2b-columnset::

	.. r2b-column::

		This tests the explicit column directive. No widths are given.

	.. r2b-column::

		There should be two columns sharing the default width of 0.90.

		Note this column should have two paragraphs. 


Testing explicit columns with widths
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. r2b-columnset::
	:width: 0.80

	.. r2b-column::
		:width: 0.50

		The column set has a width of 0.80, and this column 0.50.

	.. r2b-column::

		This column should get the remainder, 0.30.


Testing notes
-------------

Introduction
~~~~~~~~~~~~

The notes on the follwing pages wil only show up if rst2beamer is run with
the ``shownotes`` option. For example::

	rst2beamer --shownotes true test_all.rst test_all.tex


Testing the note directive
~~~~~~~~~~~~~~~~~~~~~~~~~~
		

There is a note on this page.

.. r2b-note::

	This is it.


Testing multiple note directives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are several notes on this page.

.. r2b-note::

	This is one.

Not that you should notice.

.. r2b-note::

	This is another.

Unless you use "shownotes".

.. r2b-note::

	This is a third.


Notes as containers
~~~~~~~~~~~~~~~~~~~

Notes can also be containers.

.. container:: r2b-note

   This is a note.

This helps with compatibility.

.. container:: r2b-note

   This is a second.


Other features
--------------

Bulletpoint overlays
~~~~~~~~~~~~~~~~~~~~

Normally the below list should appear as an overlay (i.e. point-by-point). It
will appear as a single unit if instead you call::

	rst2beamer --overlaybullets false test_all.rst test_all.tex


* Item one
* Item two


Preformatted
~~~~~~~~~~~~

The below should appear as indented Python code with a monospace font::

	for i in xrange (10):
		print "foo", i
		

Parsed literals
~~~~~~~~~~~~~~~

The below should appear as indented Python code with a monospace font, and
some keywords in italics:

.. parsed-literal::

	*for* i in *xrange* (10):
		*print* "foo", i


Codeblocks
~~~~~~~~~~

The below should appear as a simple literal blocks, or highlighted if you use pygments: 

.. code-block:: python

	def myfunc (arg1, arg2='foo'):
		global baz
		bar = unicode (quux)
		return 25
				