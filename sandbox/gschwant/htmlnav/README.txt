==================
 README: htmlnav_
==================

----------------------------------------------------------
An HTML writer for docutils which supports navigation-bars
----------------------------------------------------------

:Author:    Gunnar Schwant
:Contact:   g.schwant@gmx.de
:Date:      $Date$

.. contents::

Introduction
============

This is the writer which I use to build my homepage: http://schwant.gmxhome.de. 
[#]_ The name **htmlnav** is a short term for *HTML with navigation-bars*.

System Requirements
===================

Docutils and everything needed to use docutils has to be
installed.

Installation
============

Perform the usual ``setup.py install`` procedure.

Usage
=====

Most parts of the writer are inherited from David Goodger's ``html4css1``.
In fact, if no ``.nav``-files do exist in the destination directory, it
produces the same output as David's writer.

Short example
-------------

1. Go to docutils' "``tools``"-directory and create a file called 
   "``left.nav``" with the following contents::

     colors  | #000000  | #8E8E8E
     section | Home
     link    | Python   | http://www.python.org
     link    | Docutils | http://docutils.sf.net
     raw     | <br>
     raw     | &copy; 2003 MyCompany

2. Process "``test.txt``" to HTML using "``rst2htmlnav.py``"::

    rst2htmlnav.py test.txt test.html

   Open "``test.html``" in your browser. You will notice the left
   navigation-bar.

3. Create a file called "``right.nav``" in docutils' "``tools``"-directory.
   This time the contents are::

     colors  | #000000 | #8E8E8E
     section | Contact
     link    | me@MyCompany.org | mailto:me@mycompany.org

4. Now perform step 2 again and open "``test.html``" in your browser.
   You will notice that there is a right navigation-bar now.

5. Create a file called "``top.nav``" in docutils' "``tools``"-directory
   with the following contents::

    color     | #C8DBEB
    cornerpic | ../docs/rst/images/ball1.gif
    link      | Home     | test.html
    link      | Python   | http://www.python.org
    link      | Docutils | http://docutils.sf.net
    link      | Search   | http://www.google.com

6. Once again perform step 2 and open "``test.html``" in your browser.
   Now a top navigation-bar is there, too.

.nav-files
----------

In order to get navigation-bars in the output-page you have to create
``.nav``-files in the destination directory. There are 4 types of 
``.nav``-files:

[file].nav
  This defines the left navigation-bar of the output-file ``[file].html``.
  (``[file]`` is the name of the output-file without extension.) 

left.nav
  This defines the left navigation-bar of all output-files for which
  no ``[file].nav``-file is present.

right.nav
  This defines the right navigation-bar of all output-files.

top.nav
  This defines the top navigation-bar of all output-files.

.. important:: At least one of the files ``[file].nav`` and ``left.nav`` 
               has to be present. Otherwise no navigation-bars will be
               added to the output file.

.nav-file-entries
-----------------

``.nav``-files contain a one-liner for each navigation-bar-entry.
In general such a one-liner is of the form

::

  parameter | value 1 | value 2

These are the different types of parameters:

color
~~~~~

:Used in: ``top.nav``
:Value 1: Background-color of all navigation-bars.
:Value 2: Not used.
:Example: ``color | #8E8E8E`` sets the navigation-bars' background-color 
          to ``#8E8E8E``. (The default color is white.)

colors
~~~~~~

:Used in: ``[file].nav``, ``left.nav``, ``right.nav``
:Value 1: Background-color of section entries.
:Value 2: Background-color of any other (non-section) entry.
:Example: ``colors | #000000 | #8E8E8E`` sets the background-color to be 
          ``#000000`` (black) for sections to be ``#8E8E8E`` (gray) for 
          any other entry.

cornerpic
~~~~~~~~~

:Used in: ``top.nav``
:Value 1: Path to the graphics file which will be displayed in the upper 
          left corner of the top navigation-bar. (The *width* of a 
          corner-picture should be 150 pixel.)
:Value 2: Not used.
:Example: ``cornerpic | pics/home.png`` sets the path of the corner-picture
          to ``pics/home.png``. 

raw
~~~

:Used in: ``[file].nav``, ``left.nav``, ``right.nav``
:Value 1: Any kind of text.
:Value 2: Not used.
:Example: ``raw | <br>`` will create an empty cell (a spacer) in 
          the navigation-bar.

section
~~~~~~~

:Used in: ``[file].nav``, ``left.nav``, ``right.nav``
:Value 1: Title of the section.
:Value 2: Not used.
:Example: ``section | Home`` will create a section with title *Home* in 
          the navigation-bar.

link
~~~~

:Used in: ``[file].nav``, ``left.nav``, ``right.nav``, ``top.nav``
:Value 1: Text to be displayed.
:Value 2: URL
:Example: ``link | Docutils | http://docutils.sf.net`` creates a link to 
          Docutils' homepage in the navigation-bar.

Footnotes
=========

.. [#] Sorry, my homepage is in german language. However, even if you 
       don't understand what the text says, you'll get an impression of the
       writers output.

.. _htmlnav: writer/htmlnav.py