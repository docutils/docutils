=====================
 README: DocFactory_
=====================

:author:    Dr. Gunnar Schwant
:contact:   g.schwant@gmx.de
:date:      $Date$
:version:   0.1.4
:copyright: Copyright (c) 2002 Dr. Gunnar Schwant (g.schwant@gmx.de).
            All rights reserved.  See LICENSE.txt_ for license details.

:abstract: This is a short introduction to DocFactory.  Hopefully,
           more documentation will be available soon.

.. _DocFactory: http://docutils.sf.net/sandbox/gschwant/docfactory/

.. sectnum::
.. contents::


What is it?
===========

DocFactory is a kind of *integrated publishing environment* for
documentation, tied to Docutils_.  In fact, the intention of
DocFactory is to be a GUI for Docutils.  At present it can be used
to edit a set of one or more reStructuredText_ files simultaneously
and publish them as HTML.  Support for other markups and output
formats is planned for future releases; see `Future Directions`_.

DocFactory was built using Docutils_, Python_ and wxPython_.
*Many thanks to all developers!*


System requirements
===================

DocFactory is distributed as a subpackage of Docutils
(``docutils.factory``).  In order to run it in a Python-environment
you'll need the following:

* Python 2.1.1 or later (http://www.python.org).

* wxPython 2.3.2.1 or later (http://wxpython.org).  Be sure to get the
  build matching the version of Python you're using.

* Docutils 0.2.2 or later (http://docutils.sourceforge.net).  Use the
  CVS snapshot; improvements are being made almost daily.


Installation
============

The first step is to expand the ``.tar.gz`` or ``.tgz`` archive.  It
contains a distutils setup file "setup.py".  OS-specific installation
instructions follow.

Win32
-----

1. Open a DOS box (Command Shell, MSDOS Prompt, or whatever they're
   calling it these days).

2. Go to the directory created by expanding the archive::

       cd <archive_directory_path>

3. Install the package::

       <path_to_python.exe>\python setup.py install


GNU/Linux, Unix, MacOS X, etc.
------------------------------

1. Open a shell.

2. Go to the directory created by expanding the archive::

       cd <archive_directory_path>

3. Install the package::

       python setup.py install

   If the python executable isn't on your path, you'll have to specify
   the complete path, such as /usr/local/bin/python.  You may need
   root permissions to complete this step.


Usage
=====

How to start DocFactory
-----------------------

After successful installation you'll find "docfactory.py" in the
scripts-directory of your Python environment.  Run this file in the
same way as you do it with any other Python-program on your system.
The main window will appear.

Files
-----

Create a new file
~~~~~~~~~~~~~~~~~

Type ``Ctrl+N`` to create a new text file.  You will be asked for a
location and title of the new file.  Afterwards the file will be
created, added to the tree on the left and loaded it into the editor.  
As you will notice the title has been inserted at the top of the
file.

Open an existing file
~~~~~~~~~~~~~~~~~~~~~

To open an already existing text file simply type ``Ctrl+O`` and
select the file. It will be added to the tree and loaded into
the editor.

Publish a file as HTML
~~~~~~~~~~~~~~~~~~~~~~

A file which has been loaded into the editor can be published as HTML
by pressing ``F7``.  The processing happens in a standard Docutils way: 
The HTML file is created in the directory of your text file and you can 
customize processing by setting up Docutils configuration files. [1]_

As soon as the processing is finished the HTML-file will be displayed 
in a preview page.  Please note that this preview does **not** support 
stylesheets.  If you want to have a look at the document in your default
webbrowser (which hopefully does support stylesheets) click on "View 
In Browser".

Projects
--------

If you want

* to edit and publish one or more text files in the same way 

* to use DocFactory to edit the same files more than once *or* 

* to setup a Docutils configuration file easily

a project may be the right choice for you. 

Setting up a project
~~~~~~~~~~~~~~~~~~~~

To setup a project select "Project -> New".  The **Project Settings** 
dialog will appear.  It consists of two pages: 

**DocFactory-page**
  On this page you specify an *output-directory* and a *title* for your 
  project. The output-directory is the place where any HTML-files will 
  be created.

**Docutils-page**
  As part of your project a Docutils configuration file ``docutils.conf``
  will be created in the output-directory. (If there is already a 
  ``docutils.conf`` file in the output-directory this one will be used.) 
  On the Docutils-page of the "Project Settings" dialog you can set 
  the values of certain configuration file entries (*stylesheet*, 
  *output-encoding*, *datestamp*, ...).  Please have a closer look at 
  the Docutils documentation on configuration file entries to learn about 
  the effect of these settings. [2]_

Press the "OK"-button to finish the setup.  The project appears as part
of the tree on the left. 

Adding files to a project
~~~~~~~~~~~~~~~~~~~~~~~~~

In order to create a new file within a project or add an existing file 
to a project simply activate the project's item in the tree on the left 
(double-click) and press ``Ctrl+N`` or ``Ctrl+O`` as described above. 
The behaviour is like this: If a project is active any file which is 
created and/or opened will be added to the active project.  (If no 
project is active files will be created/opened outside of projects.)

Keyboard Shortcuts
------------------

For rapid development DocFactory features Windows-style keyboard
shortcuts:

==============================  ======================================
Action                          Keyboard Shortcut
==============================  ======================================
**File Menu**
----------------------------------------------------------------------
New File                        Ctrl+N
------------------------------  --------------------------------------
Open File                       Ctrl+O
------------------------------  --------------------------------------
Save File                       Ctrl+S
------------------------------  --------------------------------------
Exit Application                Alt+X
------------------------------  --------------------------------------

**Process Menu**
----------------------------------------------------------------------
To HTML                         F7
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


*Now, this should be enough for the beginning.  Start playing with
DocFactory, have fun, report bugs, contribute, ...  Any kind of
feedback is welcome.*


Future Directions
=================

Future releases of DocFactory will support any markup and output
formats which are supported by Docutils.  Some other useful things
will be implemented as well.  For example:

* more stylesheets (please contribute!)
* preferences dialog
* find & replace dialog
* ...

.. Footnotes

.. [1] http://docutils.sourceforge.net/docs/tools.html#configuration-files

.. [2] http://docutils.sourceforge.net/docs/tools.html#configuration-file-entries

.. Hyperlinks

.. _LICENSE.txt:           LICENSE.html
.. _reStructuredText:      http://docutils.sourceforge.net/rst.html
.. _Python:                http://www.python.org
.. _wxPython:              http://wxpython.org
.. _Docutils:              http://docutils.sourceforge.net
