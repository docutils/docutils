Mathhack Instructions
=====================

::

   Just write :texmath:`formula` or simply `formula` for inline
   formulas; for display formulas use a directive:

   .. texmath:: formula

   Inline formulas can also be written with |substitution| references:

   .. |substitution| formula
   
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

Note that the `<imgmathhack.py>`_ script relies on some external commands (see
the comments at its top).  `tex_to_images` seems to be separately availiable
from the `speech_tools CVS`__

__ http://cvs.sf.net/viewcvs.py/*checkout*/emu/speech_tools/scripts/tex_to_images.prl?rev=HEAD

Also note that the scripts use regexps to "parse" the roles/directives, so
expect some bugs (e.g. don't try to split a formula into multiple lines inside
a table cell...).
