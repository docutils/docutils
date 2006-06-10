====================================
 Docutils_ for python documentation
====================================

:Contact: grubert@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

This tries to explore the posibilities to use docutils as format for python
library documentation as specified in `Documenting Python`_ .

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

* Document ``markup.py`` and ``missing.py``.

* Inline markup: email, note, pep, rfc, strong, ulink, url, warning
  might be already possible others require manual markup ?
  Or maybe seepep, ...
  
* see also environment.

* standard definition lists for or something else ?::

    Arguments:
      file: a file name

      encoding: a encoding

* if a definition has no classifier and starts with ``class`` use classdesc ?

* funcdesc arguments with default values should be marked as optional.

  

Link to Indexes
'''''''''''''''

This is done by *informational units* (see `Documenting Python`_).
::

  sizeof(type_or_object)

      Returns the size in bytes of a ctypes type or instance memory
      buffer.  Does the same as the C sizeof() function.

should become ::

  \begin{funcdesc}{sizeof}{type_or_object}
      Returns the size in bytes of a ctypes type or instance memory
	  buffer.  Does the same as the C sizeof() function.
  \end{funcdesc}

this is done with definition lists with classifiers.
Plain definition lists are only supported by transaltion to 
``datadescni``, this might result in too much white space. ::

  sizeof(type_or_object) : funcdesc
      Returns the size in bytes of a ctypes type or instance memory
      buffer.  Does the same as the C sizeof() function.

Note: the classifier must be preceeded by blank colon blank (`` : ``)
  and no blank line before the descriptional text.

  And the definition, e.g. ``sizeof(...)`` is processed as text only, 
  any markup like bold or literal is lost.

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
  \begin{excclassdesc} {name}{constructor parameters} 

  \begin{methoddesc} [type name]{name}{parameters} 
  \begin{methoddescni} [type name]{name}{parameters} 

  \begin{cmemberdesc} {container}{type}{name}
  \begin{cfuncdesc} {type}{name}{args}

The ``ni``-variants do not create index entries.

``classdesc*`` describes a class without describing the constructor.

More beautiful would be to add some translation from rst-classifier 
to docpy markup. The desc can be skipped, "ni" should be written as 
"noindex" and the ``*`` variant should be detected as it has no 
parameter. e.g. ::

  SerialAvail : function noindex
    this ...

This requires some thought interrest and feedback.

Change log
''''''''''

* mkpydoc.py:

  - 2006-06-07: rename mkpydoc.py to rst2docpy.py and
                remove informational units via topics.
  - 2006-06-03: add cmemberdesc.
  - 2006-06-02: support more informational units (cmemberdesc missing)
  - 2006-05-25: notice environment.
  - 2006-05-27: 
  
    - parameters in ``[]`` are marked optional.
    - definition lists with classifiers for information units.
  - 2006-05-13: Take moduleinfo from docinfo fields.
  - 2006-05-08: Change to generate pydoc tableii.
  - 2006-04-28:

    - Patch for python 2.3.
    - Filenames from command line.
    - Guard definition of ``locallinewidth`` against redefinition.
    - latex needs definition of ``locallinewidth``.

  - 2006-04-xx: from theller, ctypes repository.

.. _Docutils: http://docutils.sourceforge.net/
.. _Documenting Python: <http://docs.python.org/dev/doc/doc.html>
