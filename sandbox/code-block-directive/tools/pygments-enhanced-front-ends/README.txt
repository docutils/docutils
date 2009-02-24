.. -*- rst-mode -*-

Pygments enhanced docutils front-ends
-------------------------------------

The example code in "`Using Pygments in ReST documents`_" defines a new
"sourcecode" directive. The directive takes one argument `language` and uses
the `Pygments`_ source highlighter to parse and render its content as a
colourful source code block. 

Combining the pygments_ example code with the standard docutils_ front-ends,
results in front-end scripts generating output documents with syntax colour.
For consistency with the majority of existing add-ons, the directive is
renamed to "code-block".

`rst2html-pygments`_ 
  enhances the standard docutils ``rst2html`` front-end to
  generate a HTML rendering with syntax highlight. 
  
`rst2latex-pygments`_ 
  enhances docutils' ``rst2latex`` to generate LaTeX with syntax highlight.

Advantages:
  + Easy implementation with no changes to the stock docutils_. 
  + Separation of code blocks and ordinary literal blocks.

Disadvantages:
  - "code-block" content is formatted by `pygments`_ and inserted in the
    document tree as a "raw" node making the approach writer-dependant.
  - documents are incompatible with the standard docutils because of the
    locally defined directive.
  - more "invasive" markup distracting from content
  - no "minimal" code block marker -- three additional lines per code block


The disadvantages lead to the alternative implementation with the
demonstrator front ends `rst2html-highlight`_ and `rst2latex-highlight`_.


Example
"""""""

Python script:
  :text source: `for-else-test.py.txt`_
  :HTML:   `for-else-test.py.htm`_
  :LaTeX:  `for-else-test.py.tex`_
  :PDF:    `for-else-test.py.pdf`_

Stylesheets:
  :CSS stylesheet:  `pygments-default.css`_
  :LaTeX style:     `pygments-default.sty`_

.. References

.. _pygments: http://pygments.org/
.. _docutils: http://docutils.sourceforge.net/
.. _Using Pygments in ReST documents: http://pygments.org/docs/rstdirective/

.. _rst2html-pygments:    rst2html-pygments
.. _rst2latex-pygments:   rst2latex-pygments
.. _rst2html-highlight:    ../../rst2html-highlight
.. _rst2latex-highlight:   ../../rst2latex-highlight
.. _for-else-test:
.. _for-else-test.py.htm: for-else-test.py.htm
.. _for-else-test.py.txt: for-else-test.py.txt
.. _for-else-test.py.tex: for-else-test.py.tex
.. _for-else-test.py.pdf: for-else-test.py.pdf
.. _pygments-default.css: ../../data/pygments-default.css
.. _pygments-default.sty: ../../data/pygments-default.sty
