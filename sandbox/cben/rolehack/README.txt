Mathhack Instructions
=====================

Grab and install all the .py files in
http://docutils.sourceforge.net/sandbox/cben/rolehack/. ::

   Just write :texmath:`formula` or simply `formula` for inline
   formulas; for display formulas use a directive:

   .. texmath:: formula

Now you take this (in file foo.txt) and run::

   mathhack.py foo.txt | rst2latex.py - foo.tex

which converts the roles/directives to ``raw:: latex`` directives or::

   imgmathhack.py foo.txt | html.py - foo.html

which runs TeX (generating images into a subdirectory!) and converts
the roles/directives into ``img::`` directives.  Quick, dirty and
convenient ;-).

Note that the later scripts relies on some external commands (see the
comments at its top).  `tex_to_images` seems to be separately
availiable from the `speech_tools CVS`__

__ http://cvs.sf.net/viewcvs.py/*checkout*/emu/speech_tools/scripts/tex_to_images.prl?rev=HEAD

Also note that the scripts you regexps to "parse" the
roles/directives, so expect some bugs (e.g. don't try to split a
formula into multiple lines inside a table cell...).
