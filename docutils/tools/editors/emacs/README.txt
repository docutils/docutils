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
  to your .emacs file.

  - To get direct keyboard input of non-ASCII characters (like
    "option-e e" resulting in "é" [eacute]), first enable the option
    key by setting the command key as your meta key::

        (setq mac-command-key-is-meta t) ;; nil for option key

    Next, use one of these lines::

        (set-keyboard-coding-system 'mac-roman)
        (setq mac-keyboard-text-encoding kTextEncodingISOLatin1)

    I prefer the first line, because it enables non-Latin-1 characters
    as well (em-dash, curly quotes, etc.).

  - To enable the display of all characters in the Mac-Roman charset,
    first create a fontset listing the fonts to use for each range of
    characters using charsets that Emacs understands::

      (create-fontset-from-fontset-spec
       "-apple-monaco-medium-r-normal--10-*-*-*-*-*-fontset-monaco,
        ascii:-apple-monaco-medium-r-normal--10-100-75-75-m-100-mac-roman,
        latin-iso8859-1:-apple-monaco-medium-r-normal--10-100-75-75-m-100-mac-roman,
        mule-unicode-0100-24ff:-apple-monaco-medium-r-normal--10-100-75-75-m-100-mac-roman")

    Latin-1 doesn't cover characters like em-dash and curly quotes, so
    "mule-unicode-0100-24ff" is needed.

    Next, use that fontset::

        (set-frame-font "fontset-monaco")

  Other useful resources are in `Andrew Choi's Emacs 21 for Mac OS X
  FAQ <http://members.shaw.ca/akochoi-emacs/stories/faq.html>`__.

No matter what platform (or editor) you're using, I recommend the
ProFont__ programmer's font.  It's monospaced, small but readable,
similar characters are visually distinctive (like "1lI|", "0O", "ao",
and ".,"), and free.

__ http://www.tobias-jung.de/seekingprofont/
.. _reStructuredText: http://docutils.sf.net/rst.html
