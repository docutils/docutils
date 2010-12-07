The newlatex2e writer
*********************

Date: 2010-12-07

The "newlatex2e" writer used to be in the Docutils core until
version 0.7. It is, however orphaned for a long time, deprecated
since version 0.6, and now moved to this sandbox directory.

Please report documents that worked with "newlatex2e" but fail with
"latex2e" to the docutils mail list or the bug tracker.

Installation
============

To get back a working "newlatex" writer, copy/move/link the directory
``newlatex2e`` to ``docutils/writers/`` in the standard Docutils
package and ``rst2newlatex.py`` to a place in the binary PATH.

The test input file ``standalone_rst_newlatex.txt`` works if placed
in ``docutils/test/functional/input``.
