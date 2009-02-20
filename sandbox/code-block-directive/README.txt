..                            -*- rst-mode -*-

===============================================
Proposal for a code-block directive in docutils
===============================================

:Author: Günter Milde
:Contact: milde@users.berlios.de
:Date: $Date$

This sandbox project contains experimental code and documentation related to
the proposal for syntax highlight of source code in docutils using a 
"code-block" directive.

See `<docs/syntax-highlight.html>`_ for a full description.

`<rst2html-highlight>`_ 
   front end for reStructuredText -> HTML conversion supporting the
   "code-block" directive.

`<data>`_
   Style sheets

`<docs>`_
   Documentation, concepts, discussion, examples...

   `<docs/pygments_code_block_directive.py>`_ 
     Working example: defines and registers a
     code-block directive using the Pygments_  syntax highlighter. 
     
   `<docs/pygments_code_block_directive-bunt.py.htm>`_
     Colourful literal code (maybe outdated).
   
`<tools>`_
   Alternative (legacy) front ends, 
   script for interactive testing.


.. References

.. _pygments: http://pygments.org/




