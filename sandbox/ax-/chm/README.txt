docutils documentation in Microsoft HTML Help (chm) format
==========================================================

:Author: Axel Kollmorgen
:Contact: axel(at)kollmorgen[dot]net
:Date: $Date$
:Web site: http://docutils.sourceforge.net/sandbox/ax-/chm/

this is the complete docutils_ documentation - all \*.txt files and 
`docs/rst/quickref.html`_ - in a single, toc'ed, and full-text searchable 
chm file. the `first version`_ was handmade (and incomplete). the 
current version uses Ollie Rutherfurd's lovely rst2chm_::

  rst2chm.py --title=reStructuredText --default-topic=spec\rst\introduction.html 
             --no-clean rst.chm
             spec\rst\*.txt docs\rst\*.txt docs\*.txt *.txt spec\*.txt spec\howto\*.txt

and a few manual improvements (inclusion and "sectioning" of 
`docs/rst/quickref.html`_, some rearrangements of toc entries, and an 
improved stylesheet) (and is complete). and probably won't see an update 
any soon.

.. _docutils: http://docutils.sourceforge.net/
.. _docs/rst/quickref.html: http://docutils.sourceforge.net/docs/rst/quickref.html
.. _first version: http://mail.python.org/pipermail/doc-sig/2003-July/003033.html
.. _rst2chm: http://www.rutherfurd.net/software/rst2chm/
