====================================
 Docutils_ for python documentation
====================================

:Contact: grubert@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

This tries to explore the posibilities to use docutils as format for python
library documentation as specified in 
`Documenting Python <http://docs.python.org/dev/doc/doc.html>`_.

This version is based on a script from Greg Ward, hacked by Thomas Heller.

There is also 

* `Edward Loper's sandbox
  <http://docutils.sf.net/sandbox/edloper/docpy/>`__.  The example 
  "asyncore.rst" file was originally adapted by Steve Holden and Bill
  Sconce.

* `Dave Kuhlman's sandbox
  <http://docutils.sf.net/sandbox/dkuhlman/>`__ and `his homepage
  <http://www.rexx.com/~dkuhlman/rstpythonlatex_intro.html>`__.


Module information
''''''''''''''''''

Maybe make title "ctypes --- A foreign function library for Python"
or use pep reader ?

For example ::

  :Module: ctypes
  :Summary: A foreign function library for Python.
  :Module Type: standard
  :Author: Thomas Heller <theller@python.net>
  :Synopsis: A foreign function library for Python.
  :Version Added: 2.5


Module and Summary are mandatory.

Problems
''''''''

* Tables 

  - For python documentation are typeset using ``\tableiii`` or
    ``\longtableiii``. 
    
    a. Only tables with two to five columns are supported.
    b. longtables should only be used for longer tables.
    c. Neither multicolumn nor multirow seam to be possible.
    d. All contents are left aligned.
    e. Which options for colfont are there: textrm, exception .
 
  - An unpatched latex2html might be unable to handle ``longtable`` options.
    Maybe remove the ``[c]`` and put the longtable into a center environment.
* (th) feature request: it would be very nice if it were possible to generate links
  into the index for functions and types from the rest sources.
* Document ``markup.py`` and ``missing.py``.

Link to Indexes
---------------

::

  :funcdesc: sizeof(type_or_object)

      Returns the size in bytes of a ctypes type or instance memory
	  buffer.  Does the same as the C sizeof() function.

should become ::

  \begin{funcdesc}{sizeof}{type_or_object}
      Returns the size in bytes of a ctypes type or instance memory
	  buffer.  Does the same as the C sizeof() function.
  \end{funcdesc}

the easy way is (supported) ::

  .. topic:: sizeof(type_or_object)
     :class: funcdesc

other definition lists (supported). Plain definition lists dont work, but are
written as datadescni, this might result in too much white space. ::

  sizeof(type_or_object) : funcdesc
      Returns the size in bytes of a ctypes type or instance memory
	  buffer.  Does the same as the C sizeof() function.

Environments (sorted by parameters)::

  \begin{datadesc} {name} 
  \begin{datadescni} {name} 
  \begin{excdesc} {name} 
  \begin{classdesc*} {name} 
  \begin{csimplemacrodesc} {name} 

  \begin{ctypedesc} [tag]{name} 
  \begin{memberdesc} [type name]{name} 
  \begin{memberdescni} [type name]{name} 
  
  \begin{cvardesc} {type}{name} 
  \begin{funcdesc} {name}{parameters} 
  \begin{funcdescni} {name}{parameters} 
  \begin{classdesc} {name}{constructor parameters} 

  \begin{methoddesc} [type name]{name}{parameters} 
  \begin{methoddescni} [type name]{name}{parameters} 

  \begin{cmemberdesc} {container}{type}{name}
  \begin{excclassdesc} {name}{constructor parameters} 
  \begin{cfuncdesc} {type}{name}{args}

The ``ni``-variants do not create index entries.

``classdesc*`` describes a class without describing the constructor.


Change log
''''''''''

2006-05-13

* mkpydoc.py:

  - Take moduleinfo from docinfo fields.

2006-05-08

* mkpydoc.py:

  - Change to generate pydoc tableii.

2006-04-28 

* mkpydoc.py:

  - Patch for python 2.3.
  - Filenames from command line.
  - Guard definition of ``locallinewidth`` against redefinition.
  - latex needs definition of ``locallinewidth``.

2006-04-xx  

* mkpydoc.py - from theller, ctypes repository.




.. _Docutils: http://docutils.sourceforge.net/
