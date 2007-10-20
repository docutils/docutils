----------
rst2wordml
----------

`rst2wordml <http://s3.amazonaws.com/hobbit-hole/rst2wordml.zip>`__ is a `reStructuredText
<http://docutils.sf.net/rst>`__ (reST) to `WordprocessingML <http://www.microsoft.com>`__ (WordML) converter.

You can use rst2wordml to convert reST text files to WordML.  The resulting WordML file may then be loaded
into MS Word 2003 or later.  You can then edit, save, or print it as any format that MS Word supports, such as
PDF.

Installation
------------

Docutils should already be installed before installing rst2wordml.  This guide assumes that Python is
installed at C:\\Python25 and that Docutils has been installed in its default location,
C:\\Python25\\Libs\\site-packages\\docutils.

rst2wordml consists of 2 .py files and a template.xml file:

+--------------------+-----------------------------------------------------+
| **File**           | **Location**                                        |
+====================+=====================================================+
| docutils_wordml.py | C:\\Python25\\Lib\\site-packages\\docutils\\writers |
+--------------------+-----------------------------------------------------+
| rst2wordml.py      | C:\\Python25\\Scripts                               |
+--------------------+-----------------------------------------------------+
| template.xml       | Wherever the .rst file(s) to convert are.           |
+--------------------+-----------------------------------------------------+

The template.xml file is used by rst2wordml to give default styles and formatting options to its output.  See
Options_ for information on custom template files.

Features
--------

Currently not all the features supported by reST are converted by the rst2wordml converter.  Here is a list of
features and their level of support:

- **Impl** means a feature is already implemented.
- **Unknown** means a feature might work but hasn't been tested.
- **Next** means that it should be coming in the near future.
- **None** means that I have no intention of supporting this feature, although I will take patches to support
  the feature.

+-------------------------------------+--------+-----------+--------+--------+
|**Feature**                          |**Impl**|**Unknown**|**Next**|**None**|
+=====================================+========+===========+========+========+
|Paragraphs                           |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Inline markup (bold and italic)      |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Hyperlinks                           |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Inline literals                      |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Bullet Lists                         |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Enumerated Lists                     |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Definition Lists                     |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Field Lists                          |        |           |   X    |        |
+-------------------------------------+--------+-----------+--------+--------+
|Option Lists                         |        |           |        |   X    |
+-------------------------------------+--------+-----------+--------+--------+
|Literal Blocks                       |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Line Blocks                          |        |           |   X    |        |
+-------------------------------------+--------+-----------+--------+--------+
|Block Quotes                         |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Doctest Blocks                       |        |           |        |   X    |
+-------------------------------------+--------+-----------+--------+--------+
|Tables                               |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Images [#]_                          |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Footnotes [#]_                       |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Citations                            |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Directives                           |        |    X      |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Substitutions                        |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+
|Comments                             |   X    |           |        |        |
+-------------------------------------+--------+-----------+--------+--------+

This table isn't necessarily exhaustive, though I did go through the specification to come up with this list.

Options
-------

For now rst2wordml only supports a single option, --template=<template xml file>.  The default is a file
called template.xml.  See Implementation_ for information how to create a template file of your own if
template.xml doesn't meet your needs.

Unit Tests
----------

Included in this release are the beginnings of unit tests, included in the test subdirectory.  Each test
stands on its own as a script and uses doctest to generate a document from the default template.xml file.  To
run each test, just type python <test name>.  If the test generates no output, it passed.

Implementation
--------------

There really is no magic to the implementation of rst2wordml.  The good folks at docutils have factored reST
into a nice set of classes.  To format a new output type for a reST doc, one only need create a new subclass
of Writer class, which I did in the docutils_wordml.py file.

Also, rst2wordml.py is a simple front end for instantiating the conversion process from the command line.

rst2wordml uses a template file similar in concept to the CSS file used by the HTML converter.  The template
file contains the default formatting properties for items such as styles, tables, and lists.  It is
theoretically easy to create a new look for your documents by opening template.xml in MS Word, adjusting the
styles and then saving the file.  Unfortunately, theory doesn't apply here.

If you open template.xml, you'll perhaps notice buried in all the XML, three unique tags not supported by
WordML: <w:rest>, <w:rstlists>, and <w:rstlistdefs>.  These are used by rst2wordml to insert generated data.
Otherwise, the generated output is identical to the template.xml file.

The template file should contain the styles Normal, DefaultParagraphFont, Heading 1, Heading 2, Heading 3,
LiteralBlock, Endnote Text, and Endnote Reference.  Also, list styles for bullet lists and enumerated lists
need to be included.

If you should wish to create your own template.xml file, the easiest thing to do is create a document in MS
Word with these styles and save it out as WordML.  Next, open it in a text editor, and look for the places
where the special tags appear in the original template.xml and place the same tags in your file.

Literate Programming with reST
------------------------------

One of the reasons that I started writing this converter was to help me document one of the (other) projects
I'm working on.  As I was writing the docs for that project, I was consistently copying and pasting code
samples into literal blocks.  Of course, as the code changed, so did the amount of work keeping it in sync
with the docs.  Literate programming seemed to provide exactly what I needed, code and docs together.

I had seen literate programming used in the book `Physically Based Rendering`_ and I thought it would be cool
to have the ability to write documentation and code in a single file.  After looking at some of the literate
programming tools, I found PyLit_, a *semi-literate programming* module that works with reST.  reST has
everything I need in a markup without having to really learn a markup language.  PyLit can do the code
extraction, rst2html does the conversion to web pages, and rst2wordml can convert to Word files, from which I
can easily make printed copoies or even create PDF files.


.. [#] Images are centered and the dimensions are extracted if the Python Imaging Library is installed.
       Otherwise they must be supplied by the author or they default to 100x100.

.. [#] Footnotes in rst2wordml are implemented as end notes to better match the output of the HTML converter.

.. _`Physically Based Rendering`: http://www.amazon.com/Physically-Based-Rendering-Implementation-Interactive/dp/012553180X/ref=pd_bbs_sr_1/103-3091427-3166253?ie=UTF8

.. _`PyLit`: http://pylit.berlios.de/index.html
