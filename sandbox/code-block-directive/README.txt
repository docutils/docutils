..                            -*- rst-mode -*-

===============================================
Proposal for a code-block directive in docutils
===============================================

:Author: Guenter Milde <milde@users.berlios.de>
:Date: $Date$


This sandbox project contains experimental code and documentation related to
the proposal for syntax highlight of source code in docutils using a 
"code-block" directive.

See `syntax-highlight.html`_ for the full picture.

`pygments_code_block_directive.py`_ 
   Working example: defines and registers a
   code-block directive using the Pygments_  syntax highlighter. 
   
   colourful literal code (maybe outdated):
   `pygments_code_block_directive-bunt.py.html`_

`rst2html-highlight`_ 
   front end for reStructuredText -> HTML conversion supporting the
   "code-block" directive.

data_
   Style sheets

docs_
   Documentation, concepts, discussion, examples...
   
tools_
   Alternative (legacy) front ends, 
   script for interactive testing.


.. References

.. _pygments: http://pygments.org/

.. _pygments_code_block_directive.py: pygments_code_block_directive.py
.. _pygments_code_block_directive-bunt.py.html: 
     docs/pygments_code_block_directive-bunt.py.html
.. _rst2html-highlight: rst2html-highlight
.. _data: data
.. _docs: docs
.. _tools: tools
.. _syntax-highlight.html: docs/syntax-highlight.html




