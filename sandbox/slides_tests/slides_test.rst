=========================
Test for Slides Writers
=========================

--------
Subtitle
--------

:author: Au Thor
:date: January 1, 2525
:organization: Cu U


Section 1
~~~~~~~~~

Subsection 1.1
``````````````

Slide 1
=======

- These slides have a title, subtitle, sections, and subsections.
- rst2beamer correctly identifies this sectioning
- rst2pdf does not automatically break this into slides,
  but the ``-b3`` option can cause it to do so

Slide 2
=======

A slide using a math directive:

.. math::

   a = b

Only rst2pdf handles this at the moment
(although rst2beamer can work around this by
using raw latex).


Slide 3
=======

More tests will be added later,
as this document is already too big a challenge for existing writers.
See the other test documents for less demanding tests.


Slide 4
=======

A slide.


Slide 5
=======

A slide.


Slide 6
=======

A slide.


