.. -*- coding: iso-8859-1 -*-

=====================================
 Emacs Support for reStructuredText_
=====================================

:Date: $Date$

Since reStructuredText punts on the issue of character processing,
here are some useful resources for Emacs users in the Unicode world:

* `xmlunicode.el and unichars.el from Norman Walsh
  <http://nwalsh.com/emacs/xmlchars/index.html>`__

* `An essay by Tim Bray, with example code
  <http://www.tbray.org/ongoing/When/200x/2003/09/27/UniEmacs>`__

* For Emacs users on Mac OS X, here are some useful useful additions
  to your .emacs file.  To get direct keyboard input of Latin-1
  characters (like "option-e e" resulting in "é" [eacute]), use this::

      (setq mac-keyboard-text-encoding kTextEncodingISOLatin1)

  For this to work, you must use the command key as your meta key::

      (setq mac-command-key-is-meta t) ;; nil for option key

  Other useful resources are here:
  <http://members.shaw.ca/akochoi-emacs/stories/faq.html>.

.. _reStructuredText: http://docutils.sf.net/rst.html
