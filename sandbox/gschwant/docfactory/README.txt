=====================
 README: DocFactory_
=====================

:author:    Dr. Gunnar Schwant
:contact:   g.schwant@gmx.de
:date:      $Date$
:version:   0.1.3
:copyright: Copyright (c) 2002 Dr. Gunnar Schwant (g.schwant@gmx.de).
            All rights reserved.  See LICENSE.txt_ for license details.

:abstract: This is a short introduction to **DocFactory**.  Hopefully,
           more documentation will be available soon.

.. _DocFactory: http://docutils.sf.net/sandbox/gschwant/docfactory/

.. contents::


What is it?
===========

**DocFactory** is a kind of *integrated publishing environment* for
documentation, tied to Docutils_.  In fact, the intention of
**DocFactory** is to be a GUI for Docutils.  At present it can be used
to edit a set of one or more reStructuredText_ files simultaneously
and publish them as HTML.  Support for other markups and output
formats is planned for future releases; see `Future Directions`_.

**DocFactory** was built using Docutils_, Python_ and wxPython_.
*Many thanks to all developers!*


System requirements
===================

**DocFactory** is distributed as a subpackage of Docutils
(``docutils.factory``).  In order to run it in a Python-environment
you'll need the following:

* Python 2.1.1 or later (http://www.python.org).

* wxPython 2.3.2.1 or later (http://wxpython.org).  Be sure to get the
  build matching the version of Python you're using.

* Docutils 0.2.2 or later (http://docutils.sourceforge.net).  Use the
  CVS snapshot; improvements are being made almost daily.


Installation
============

Win32
-----

Download and run the Windows installer "DocFactory-0.1.3.win32.exe".


GNU/Linux, Unix, MacOS X, etc.
------------------------------

Extract the ``.tar.gz`` or ``.tgz`` archive.  It contains a distutils
setup file "setup.py".  Run this python script with argument
"install"::

    python setup.py install

If the python executable isn't on your path, you'll have to specify
the complete path, such as ``/usr/local/bin/python``.  You may need
root permissions to complete this step.


Usage
=====

A short introductory example
----------------------------

After successful installation you'll find "docfactory.py" in the
scripts-directory of your Python environment.  Run this file in the
same way as you do it with any other Python-program on your system.
The main window will appear.

Select "Project -> New" to create a new project.  This will bring up
the "Project Settings"-dialog where you have to specify some basic
information about your project:

*Title*
  Enter a title for your
  project here.

*Author*
  Enter your name here. It will
  be used as default value for
  author in new files.

*Contact*
  Enter your e-mail-address here.
  It will be used as default value
  for contact in new files.

*Output-Directory*
  This is the directory where HTML-files
  will be created.

*Stylesheet*
  Specify a stylesheet (``.css``) in the
  output-directory which will be used by
  HTML-output. You can choose an arbitrary
  stylesheet by pressing "Select". (Please note
  that the selected stylesheet will be copied 
  to the output-directory.)

If you press the dialog's "OK"-button a project tree will be created
in the left splitter window and the editor page will appear.  Now you
have two options:

1. Add an existing reStructuredText file to your project.

   *or*

2. Create a new reStructuredText file which will be added to your
   project.

Let's proceed with option 2: Type ``Ctrl+N``.  You will be asked for a
location and title of the new file.  Afterwards the file will be
created and added to the project tree on the left.  Select the file in
the project tree to load it into the editor.  As you will notice the
titles and some bibliographic fields have been inserted already :-).

Press ``F7`` to publish the file as HTML.  As soon as the processing
is finished the HTML-file will be displayed in a preview page.  Please
note that this preview does **not** support stylesheets.  If you want
to have a look at your document in your web browser (which hopefully
does support stylesheets) click on "View In Browser".  [#]_

Now, this should be enough for the beginning.  Start playing with
**DocFactory**, have fun, report bugs, contribute, ...  Any kind of
feedback is welcome.

.. [#] Up to now this is supported on win32 only.  Please contribute
   if you are running DocFactory on a non-win32 OS.


Keyboard Shortcuts
------------------

For rapid development **DocFactory** features Windows-style keyboard
shortcuts:

==============================  ======================================
Action                          Keyboard Shortcut
==============================  ======================================
**File Menu**
----------------------------------------------------------------------
New File                        Ctrl+N
------------------------------  --------------------------------------
Save File                       Ctrl+S
------------------------------  --------------------------------------
Exit Application                Alt+X
------------------------------  --------------------------------------

**Process Menu**
----------------------------------------------------------------------
To HTML                         F7
------------------------------  --------------------------------------

**Project Menu**
----------------------------------------------------------------------
Open Project                    Ctrl+O
------------------------------  --------------------------------------

**Editing**
----------------------------------------------------------------------
Cursor movement                 Arrow keys or mouse
------------------------------  --------------------------------------
Beginning of line               Home
------------------------------  --------------------------------------
End of line                     End
------------------------------  --------------------------------------
Beginning of buffer             Ctrl+Home
------------------------------  --------------------------------------
End of the buffer               Ctrl+End
------------------------------  --------------------------------------
Select text                     Hold down Shift while moving the
                                cursor
------------------------------  --------------------------------------
Select all text                 Ctrl+A
------------------------------  --------------------------------------
Copy                            Ctrl+Ins, Ctrl+C
------------------------------  --------------------------------------
Cut                             Shift+Del, Ctrl+X
------------------------------  --------------------------------------
Paste                           Shift+Ins, Ctrl+V
------------------------------  --------------------------------------
Undo                            Ctrl+Z
------------------------------  --------------------------------------
Redo                            Ctrl+Y
------------------------------  --------------------------------------
Zoom-In                         Ctrl+L
------------------------------  --------------------------------------
Zoom-Out                        Ctrl+K
==============================  ======================================

Right mouse-click brings up a popup-dialog with most of the above
editor commands.  Moreover, you can use the mouse to move selected
text.


Future Directions
=================

Future releases of **DocFactory** will support any markup and output
formats which are supported by Docutils.  Some other useful things
will be implemented as well.  For example:

* more stylesheets (please contribute!)
* preferences dialog
* find & replace dialog
* ...

.. _LICENSE.txt:      LICENSE.html
.. _reStructuredText: http://docutils.sourceforge.net/rst.html
.. _Python:           http://www.python.org
.. _wxPython:         http://wxpython.org
.. _py2exe:           http://py2exe.sourceforge.net
.. _Docutils:         http://docutils.sourceforge.net


..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
