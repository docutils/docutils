Mathhack Instructions
=====================

Formula syntax is everything LaTeX supports in math mode.  This is supported
for the LaTeX writer and for anything else by converting with LaTeX (and some
external_ commands) to *images*.

There are now other solutions (see the FAQ entry__) that employ dialects of
LaTeX for translation to MathML so you should consider limiting yourself to
the intersection of the syntaxes if you want to allow all possible convertions.

__ http://docutils.sourceforge.net/
   FAQ.html#how-can-i-include-mathematical-equations-in-documents

Just write::

   text... :texmath:`formula` ...text
   
or simply::

   text... `formula` ...text

for inline formulas; for display formulas use a directive::

   .. texmath:: formula

Inline formulas can also be written with substitution references::

   text... |name| ...text

   .. |name| texmath:: formula

Now you take this (in file foo.txt) and run::

   mathhack.py foo.txt | rst2latex.py - foo.tex

which converts the roles/directives to ``raw:: latex`` directives or::

   imgmathhack.py foo.txt | html.py - foo.html

which runs TeX (generating images into a subdirectory!) and converts
the roles/directives into ``img::`` directives.  Quick, dirty and
convenient ;-).

To allow including preprocessed files, do::

   mathhack.py included.txt > included.txt.mathhack
   imgmathhack.py included.txt > included.txt.imgmathhack

and include ``included.txt.mathhack`` (imgmathhack.py will mangle this to
include ``included.txt.imgmathhack`` automatically).  My makefile_ can do all
this for you (just set ENABLE_MATHHACK=1).

.. _makefile: ../make/Makefile.docutils

.. _external:

Note that the `<imgmathhack.py>`_ script relies on some external commands (see
the comments at its top).  `tex_to_images` seems to be separately availiable
from the `speech_tools CVS`__

__ http://cvs.sf.net/viewcvs.py/*checkout*/emu/speech_tools/scripts/tex_to_images.prl?rev=HEAD

Also note that the scripts use regexps to "parse" the roles/directives, so
expect some bugs (e.g. don't try to split a formula into multiple lines inside
a table cell...).
