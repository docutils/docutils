==================
LaTeX-math plug-in
==================

The plug-in adds a latex-math role and directive that transforms ReST
+ LaTeX-math to html + MathML.  The script (in the ``tools`` directory)
does exactly what rst2html.py does - only difference is that a
latex-math role and directive is added to the parser before parsing.

It works like this::

  python rst2mathml.py document.txt > document.xhtml

(try the test file from the ``test`` directory).

Documentation is in the ``docs`` directory.

The script is a quick hack that we can use for testing until we have
real plug-in support.

Any feedback is appreciated.
