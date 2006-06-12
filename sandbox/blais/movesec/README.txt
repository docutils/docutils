================
docutils-movesec
================

:Author: Martin Blais
:Contact: blais@furius.ca
:Date: $Date$


A script that can raise or lower (shift) the levels of the underline characters
defining the sections of a file.

.. important::

   This script is useless if you're using emacs with rst.el.  The current
   (2006) rst.el does this job pretty well already.


From an email on the list::

  > > i want to contribute my movesec script that rotates the section
  > > markers.  i use it every now and then and i'm finding it useful and
  > > stable enough that i think other should be able to enjoy it too.
  >
  > "Rotates"?

  yes, it raises or lowers the "levels" of the underline characters of a
  file.  it shifts them either up or down (in level), and introduces a
  new underline character for the new level.

  this is useful when you're reorganizing the sections of part of a
  file, for example.  you could use this on a section of a file only
  using shell-command-on-region in emacs (i still need to write a hook
  to do that).


.. important::  

   You can now do that within emacs using rest-adjust with an active region, see
   restructuredtext.el for details.
