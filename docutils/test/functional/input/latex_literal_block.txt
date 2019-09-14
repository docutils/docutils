In LaTeX, literal blocks can be customized with the "literal-block-env"
setting. This test file exists to check if the LaTeX writer output compiles
and looks as expected. 

Start with a plain literal block::

  $\sin^2(x)$ and $\cos^2(x)$ equals one:

  \[
     \sin^2(x) + \cos^2(x) = 1 % for all x
  \]

A latex "code-block" (set with "literal-block-env", if syntax
highlight is "none"):

.. code:: latex

  $\sin^2(x)$ and $\cos^2(x)$ equals one:

  \[
     \sin^2(x) + \cos^2(x) = 1 % for all x
  \]

A literal block in a table:

====  ===========  ====
test  ::           test

        \sin^2 x
====  ===========  ====

A literal block in a table with auto-width columns:

.. table::
   :widths: auto

   ====  ===========  ====
   test  ::           test

           \sin^2 x
   ====  ===========  ====

.. note:: A literal block in an admonition::

    \sin^2 x

.. role:: custom
.. role:: custom-role

Parsed literal block with inline markup and leading whitespace:

.. parsed-literal::

      *emphasis*, **strong emphasis**, ``inline literals``,
   standalone hyperlinks (http://www.python.org),
   internal_ and external_ hyperlinks,
   _`internal` hyperlink targets,
   images via substitution references (|example|),
   footnote references [*]_,
   citation references ([CIT2002]_), and more.

      Here are some explicit interpreted text roles:
   a PEP reference (:PEP:`287`),
   an RFC reference (:RFC:`2822`),
   an abbreviation (:ab:`abb.`), an acronym (:ac:`reST`),
   code (:code:`print "hello world"`),
   maths :math:`\sin^2(x)`,
   :sub:`subscript` and :sup:`superscript`,
   :custom:`custom` :custom-role:`roles`, and explicit roles for
   :title:`Docutils`' :emphasis:`standard` :strong:`inline` :literal:`markup`.

.. [*] This footnote is referenced in a `parsed literal` block. 
   
   It contains a literal block::
   
     \sin^2 x

.. [CIT2002] Sample Citation, 2017.

.. _external: http://www.python.org/

.. |EXAMPLE| image:: ../../../docs/user/rst/images/biohazard.png
