====================
 DocFactory README
====================

:author:    Dr. Gunnar Schwant
:contact:   g.schwant@gmx.de
:date:      2004/05/07
:copyright: Copyright (c) 2002 Dr. Gunnar Schwant (g.schwant@gmx.de).
            All rights reserved.  See LICENSE.txt_ for license details.

:abstract: This is a short introduction to DocFactory.  Hopefully,
           more documentation will be available soon.

.. contents::
.. sectnum::

What is it?
===========

DocFactory is a kind of *integrated publishing environment* for
documentation, tied to |Docutils|.  In fact, the intention of
DocFactory is to be a GUI for Docutils.  At present it can be used
to edit a set of one or more |ReST| files simultaneously
and publish them as HTML.  Support for other markups and output
formats is planned for future releases; see `Future Directions`_.

DocFactory was built using |Docutils|, |Python| and |wxPython|.
*Many thanks to all developers!*


System requirements
===================

DocFactory is distributed as a subpackage of Docutils
(``docutils.factory``).  In order to run it in a Python-environment
you'll need the following:

* Python 2.3.2 or later (|http://www.python.org|).

* wxPython 2.4.2.4 or later (|http://wxpython.org|).  Be sure to get the
  build matching the version of Python you're using.

* Docutils 0.3 or later (|http://docutils.sourceforge.net|).  Use the
  CVS snapshot; improvements are being made almost daily.


Installation
============

The first step is to expand the ``.tar.gz`` or ``.tgz`` archive. [1]_
It contains a distutils setup file "setup.py".  OS-specific
installation instructions follow.

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

.. note:: To create and browse the documentation of DocFactory please
          run the script ``createdoc.py``. This script converts all the
          ``.txt``-files of the tarball to ``.html``-files and opens
          ``doc/index.html`` with your default webbrowser.

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
the editor.  Moreover, you can open a file from the command-line 
if you specify the file as first parameter.  A command like ``python 
[Scripts-directory]/docfactory.py [<file>]`` should launch DocFactory 
and open the file for editing.  (Of course, this command depends on your 
system.  For example::

    C:\>python Python22\Scripts\docfactory.py temp\test.txt

works on my windows machine.  In particular, the Python executable has
to be on your path.)


Publish a file
~~~~~~~~~~~~~~

To "publish" a file means to process the file into useful formats, such 
as HTML, XML or TeX.

A file which has been loaded into the editor can be published by pressing 
``F7``.  You will be asked for a docutils-writer, an output-directory and 
a filename:

================= =========================================================
Dialogue Item     Description
================= ========================================================= 
Docutils-Writer   The docutils-writer which will be used for publishing.
Output-Directory  The directory where the output-file will be created.
Output-File       The name of the output-file. (Existing files will be 
                  overwritten!) 
================= =========================================================

Press "OK" to start the publisher. 

If you choose the HTML-writer the HTML-file will be displayed on 
DocFactory's HTML-viewer-page as soon as it is finished.  Please note that 
this viewer does support stylesheets on Windows platforms only (including 
Windows 95, 98, ME, 2000, NT, XP).  On any other platform you will have to 
check the final layout with a webbrowser of your choice (which hopefully 
does support stylesheets). Click on "View In Browser" to open the file 
with your system's default webbrowser.

.. note:: Please note that publishing happens in a standard Docutils way 
          and can be customized by setting up Docutils configuration files.
          [2]_

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
dialog will appear.  Within this dialog you set the *output-directory*
and a *title* for your project.  The output-directory is the default
directory for any output-files, that will be created.

If you press the "Edit docutils.conf"-button, DocFactory will display
a dialog, which allows you to edit the docutils.conf-file, which
resides in the output-directory.  (If there is no such file in the
output-directory, DocFactory will create one for you.)  Please have a
closer look at the Docutils documentation on configuration files, if
you are not familiar with docutils.conf-files. [3]_

Press the "OK"-buttons to finish the setup.  The project appears as part
of the tree on the left. 

Adding files to a project
~~~~~~~~~~~~~~~~~~~~~~~~~

In order to create a new file within a project or add an existing file 
to a project simply activate the project's item in the tree on the left 
(double-click) and press ``Ctrl+N`` or ``Ctrl+O``.  The behaviour is like 
this: If a project is active any file which is created and/or opened will 
be added to the active project.  (If no project is active files will be 
created/opened outside of projects.)


Editing
-------

Keyboard Shortcuts
~~~~~~~~~~~~~~~~~~

For rapid development DocFactory features Windows-style keyboard
shortcuts:

==============================  ===========================================
Action                          Keyboard Shortcut
==============================  ===========================================
**File Menu**
---------------------------------------------------------------------------
New File                        ``Ctrl+N``
------------------------------  -------------------------------------------
Open File                       ``Ctrl+O``
------------------------------  -------------------------------------------
Save File                       ``Ctrl+S``
------------------------------  -------------------------------------------
Publish File                    ``F7``
------------------------------  -------------------------------------------
Exit Application                ``Alt+X``
------------------------------  -------------------------------------------
**Edit Menu**
---------------------------------------------------------------------------
Undo                            ``Ctrl+Z``
------------------------------  -------------------------------------------
Redo                            ``Ctrl+Y``
------------------------------  -------------------------------------------
Cut                             ``Shift+Del``, ``Ctrl+X``
------------------------------  -------------------------------------------
Copy                            ``Ctrl+Ins``, ``Ctrl+C``
------------------------------  -------------------------------------------
Paste                           ``Shift+Ins``, ``Ctrl+V``
------------------------------  -------------------------------------------
Select all                      ``Ctrl+A``
------------------------------  -------------------------------------------
Find & Replace                  ``Ctrl+F``
------------------------------  -------------------------------------------
Goto line                       ``Ctrl+G``
------------------------------  -------------------------------------------
**Toolbox Menu**
---------------------------------------------------------------------------
Open toolbox                    ``F8``
------------------------------  -------------------------------------------
**Further editing commands**
---------------------------------------------------------------------------
Cursor movement                 Arrow keys or mouse
------------------------------  -------------------------------------------
Beginning of line               ``Home``
------------------------------  -------------------------------------------
End of line                     ``End``
------------------------------  -------------------------------------------
Beginning of buffer             ``Ctrl+Home``
------------------------------  -------------------------------------------
End of the buffer               ``Ctrl+End``
------------------------------  -------------------------------------------
Select text                     Hold down ``Shift`` while moving the cursor
------------------------------  -------------------------------------------
Zoom-In                         ``Ctrl+L``
------------------------------  -------------------------------------------
Zoom-Out                        ``Ctrl+K``
==============================  ===========================================

Right mouse-click brings up a popup-dialog with most of the above
editor commands.  Moreover, you can use the mouse to move selected
text.


Find & Replace
~~~~~~~~~~~~~~

The "Find & Replace"-dialog was introduced with DocFactory release 0.2
and is special in the sense that it uses |regular expression syntax| 
for searching as defined by Python's re-module.  This offers a great deal
of flexibility in searching.  For example: Searching for ``Fred|Ted`` will
search the file for appearances of ``Fred`` as well as for appearances
of ``Ted``.  However, it also requires a certain kind of familiarity with
Python's regular expression syntax.  For example: In order to find 
appearances of ``C:\temp\test.txt`` you will have to search for 
``C:\\temp\\test.txt``.


Toolbox
-------

The Toolbox allows you to run operating system commands from within 
DocFactory. In the menu bar select "Toolbox -> Configure" to set up
your personal toolbox. Press the "+"-button to add a new tool. You will
be asked for a **name**, a **command** and an **initial directory**:

================= =========================================================
Dialogue Item     Description
================= ========================================================= 
Name              An individual name of the tool.
Command           The operating system command which will be executed
                  when you run the tool.
Initial Directory The initial working directory of the tool. 
================= =========================================================

Command and initial directory of a tool may include **tool macros** which
will be replaced on tool-execution as follows:

================= =========================================================
Tool Macro        Description 
================= =========================================================
``$[FilePath]``   The full pathname of the active file. 
``$[FileDir]``    The drive and directory of the active file. 
``$[FileName]``   The filename of the active file. 
``$[FileBase]``   The filename without the extension. 
``$[ProjectDir]`` The output-directory of the active project.
================= =========================================================

To launch a tool select "Toolbox -> Open..." or press ``F8``.

Data Storage
============

DocFactory uses the Python's |ConfigParser|-module to store any information 
about projects, tools, etc. which has to be saved between runs. [4]_ 
As default the data storage file is located in the package's directory. [5]_ 
The default name of the file is ``docfactory.dat``. 

You can change the name and location of the data storage file by creating 
a file called ``conf.pth`` in the package's directory.  This file should 
consist of one line only which contains the complete data storage path. 
For example:  If the first line of ``conf.pth`` is 

::

    /home/fred/.docfactory

DocFactory will store its data in ``/home/fred/.docfactory``.
 

Future Directions
=================

Future releases of DocFactory will support any markup and output
formats which are supported by Docutils.  Some other useful things
will be implemented as well.  For example:

* more stylesheets (please contribute!)
* preferences dialog
* [...]

*Now, this should be enough for the beginning.  Start playing with
DocFactory, have fun, report bugs, contribute, ...  Any kind of
feedback is welcome.*

---------

.. Footnotes

.. [1] The latest DocFactory-tarball is available at 
       http://docutils.sf.net/docfactory-snapshot.tgz.
 
.. [2] http://docutils.sourceforge.net/docs/config.html

.. [3] http://docutils.sourceforge.net/docs/config.html

.. [4] The storage file has a ``[docfactory_project: <name of project>]`` 
       section for each project. (``<name of project>`` is replaced by the 
       project's name.) Each section header is followed by two ``name=value`` 
       entries: One for all project files and one for the outputdirectory. 
       For example: The section for a project called ``project_01`` which 
       consists of two files ``C:\file1.txt``, ``C:\file2.txt`` and whose 
       output-directory is ``C:\project1`` looks like this::

         [docfactory_project: project_01]
         files=C:\file1.txt;C:\file2.txt
         outputdirectory=C:\project1

.. [5] ``os.path.dirname(docutils.factory.__file__)``

.. Hyperlinks

.. _LICENSE.txt:                 LICENSE.html

.. |ReST| raw:: html

   <a class="reference" href="http://docutils.sourceforge.net/rst.html" target="_top">reStructuredText</a>
 
.. |Python| raw:: html

  <a class="reference" href="http://www.python.org" target="_top">Python</a>

.. |http://www.python.org| raw:: html

  <a class="reference" href="http://www.python.org" target="_top">http://www.python.org</a>

.. |wxPython| raw:: html

  <a class="reference" href="http://wxpython.org" target="_top">wxPython</a>

.. |http://wxPython.org| raw:: html

  <a class="reference" href="http://wxpython.org" target="_top">http://wxPython.org</a>

.. |Docutils| raw:: html

  <a class="reference" href="http://docutils.sourceforge.net" target="_top">Docutils</a>

.. |http://docutils.sourceforge.net| raw:: html

  <a class="reference" href="http://docutils.sourceforge.net" target="_top">http://docutils.sourceforge.net</a>

.. |regular expression syntax| raw:: html

  <a class="reference" href="http://www.python.org/doc/current/lib/re-syntax.html" target="_top">regular expression syntax</a>

.. |ConfigParser| raw:: html

  <a class="reference" href="http://www.python.org/doc/current/lib/module-ConfigParser.html" target="_top">ConfigParser</a>
