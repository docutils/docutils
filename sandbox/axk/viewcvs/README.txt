rst "markup streamer" for ViewCVS_
==================================

:Author: Axel Kollmorgen
:Contact: axel(at)kollmorgen[dot]net
:Version: $Id$
:Web site: http://docutils.sourceforge.net/sandbox/ax-/viewcvs/

this is a modification of ViewCVS_ that enables rst .txt files in ViewCVS_ 
repositories to be viewed as .html ("markup").  this is achieved by adding a new 
"markup streamer" ``markup_stream_rst()`` to viewcvs.py that basically does the 
same as docutils_' tools/html.py [DOC]_ [SRC]_.  once installed, any .txt file 
w/ a link ending w/ view=auto or view=markup will be rendered as html.

do you think this is useful? please `send feedback`_. much appreciated!


requirements
~~~~~~~~~~~~

ViewCVS_ / docutils_ CVS 2003-Oct-28.  it will probably work with any 
docutils_ 0.3 or newer and with newer (and a little older) versions of ViewCVS_.  
if you use a different version of ViewCVS_, do a diff of viewcvs.py before 
overwriting it - the source changes frequently.


install
~~~~~~~

* install docutils
 
* get ViewCVS_ from `CVS 
  <http://sourceforge.net/cvs/?group_id=18760>`__ (`when prompted for a password 
  for anonymous, simply press the Enter key`):: 

    cvs -d:pserver:anonymous@cvs.sourceforge.net:/cvsroot/viewcvs login
    cvs -z3 -d:pserver:anonymous@cvs.sourceforge.net:/cvsroot/viewcvs co -D28-Oct-2003 viewcvs
      
* overwrite lib/viewcvs.py with the one supplied
* install viewcvs as usual (``./viewcvs-install``)


todo
~~~~

* error handling (.txt may not be in rst format)
* review the code (i'm just starting w/ python ...)

.. _send feedback: mailto:axel(at)kollmorgen[dot]net
.. _ViewCVS: http://viewcvs.sourceforge.net/
.. _docutils: http://docutils.sourceforge.net/
.. [DOC] http://docutils.sourceforge.net/docs/tools.html#html-py
.. [SRC] http://docutils.sourceforge.net/tools/html.py
