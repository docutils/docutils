2003-02
-------

latex writer into main
~~~~~~~~~~~~~~~~~~~~~~

Done:

* move latex2e.py into docutils/writers
* move rst2latex.py into tools.
* add latex to writers/__init__ writer_aliases.
* documentation: tools documentation.

To do:

* commandline options, style.tex, ...

  Where to move the latex2e-README.txt.

* test: writer tests might be too simple, currently publish is used.
  test/test_writers
  test that the produced latex document is the same.


2002-12
-------

pdfsamples
~~~~~~~~~~

1. reduced tools_test.txt_ multirow and multicol tablecells donot work. 
  
2. docs_tools.txt_.

3. specs_pep-0256.txt_.

.. _tools_test.txt: test.pdf
.. _specs_pep-0256.txt: pep-0256.pdf
.. _docs_tools.txt: tools.pdf


2002-09
-------

latex enhanced by julien.

2002-07
-------

Menu changed now try for latex writer.

2002-05-11
----------

This month sandbox menu is a pdf writer using
reportlab lib.

I am unsure it it will ever work, justification might
need hyphenation and automatic sizing of table cells
seams to be nontrivial (considering the size of the
usual html browsers).

A better layout might be possible via the latex2e writer.
(on next month menu).

I try to approach davids structure and work and 
coding conventions but slowly as a morning sunrise.

cheers

  grubert@users.sourceforge.net

