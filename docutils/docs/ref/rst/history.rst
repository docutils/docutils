.. include:: ../../header2.rst

=============================
 History of reStructuredText
=============================
:Author: David Goodger
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

reStructuredText_, the specification, is based on StructuredText_ and
Setext_.  StructuredText was developed by Jim Fulton of `Zope
Corporation`_ (formerly Digital Creations) and first released in 1996.
It is now released as a part of the open-source "Z Object Publishing
Environment" (ZOPE_).  Ian Feldman's and Tony Sanders' earlier Setext_
specification was either an influence on StructuredText or, by their
similarities, at least evidence of the correctness of this approach.

I discovered StructuredText_ in late 1999 while searching for a way to
document the Python modules in one of my projects.  Version 1.1 of
StructuredText was included in Daniel Larsson's pythondoc_.  Although
I was not able to get pythondoc to work for me, I found StructuredText
to be almost ideal for my needs.  I joined the Python Doc-SIG_
(Documentation Special Interest Group) mailing list and found an
ongoing discussion of the shortcomings of the StructuredText
"standard".  This discussion has been going on since the inception of
the mailing list in 1996, and possibly predates it.

I decided to modify the original module with my own extensions and
some suggested by the Doc-SIG members.  I soon realized that the
module was not written with extension in mind, so I embarked upon a
general reworking, including adapting it to the "re" regular
expression module (the original inspiration for the name of this
project).  Soon after I completed the modifications, I discovered that
StructuredText.py was up to version 1.23 in the ZOPE distribution.
Implementing the new syntax extensions from version 1.23 proved to be
an exercise in frustration, as the complexity of the module had become
overwhelming.

In 2000, development on StructuredTextNG ("Next Generation") began at
`Zope Corporation`_ (then Digital Creations).  It seems to have many
improvements, but still suffers from many of the problems of classic
StructuredText.

I decided that a complete rewrite was in order, and even started a
`reStructuredText SourceForge project`_ (now inactive).  My
motivations (the "itches" I aim to "scratch") are as follows:

- I need a standard format for inline documentation of the programs I
  write.  This inline documentation has to be convertible to other
  useful formats, such as HTML.  I believe many others have the same
  need.

- I believe in the Setext/StructuredText idea and want to help
  formalize the standard.  However, I feel the current specifications
  and implementations have flaws that desperately need fixing.

- reStructuredText could form part of the foundation for a
  documentation extraction and processing system, greatly benefitting
  Python.  But it is only a part, not the whole.  reStructuredText is
  a markup language specification and a reference parser
  implementation, but it does not aspire to be the entire system.  I
  don't want reStructuredText or a hypothetical Python documentation
  processor to die stillborn because of over-ambition.

- Most of all, I want to help ease the documentation chore, the bane
  of many a programmer.

Unfortunately I was sidetracked and stopped working on this project.
In November 2000 I made the time to enumerate the problems of
StructuredText and possible solutions, and complete the first draft of
a specification.  This first draft was posted to the Doc-SIG in three
parts:

- `A Plan for Structured Text`__
- `Problems With StructuredText`__
- `reStructuredText: Revised Structured Text Specification`__

__ https://mail.python.org/pipermail/doc-sig/2000-November/001239.html
__ https://mail.python.org/pipermail/doc-sig/2000-November/001240.html
__ https://mail.python.org/pipermail/doc-sig/2000-November/001241.html

In March 2001 a flurry of activity on the Doc-SIG spurred me to
further revise and refine my specification, the result of which you
are now reading.  An offshoot of the reStructuredText project has been
the realization that a single markup scheme, no matter how well
thought out, may not be enough.  In order to tame the endless debates
on Doc-SIG, a flexible `Docstring Processing System framework`_ needed
to be constructed.  This framework has become the more important of
the two projects; reStructuredText_ has found its place as one
possible choice for a single component of the larger framework.

The project web site and the first project release were rolled out in
June 2001, including posting the second draft of the spec [#spec-2]_
and the first draft of PEPs 256, 257, and 258 [#peps-1]_ to the
Doc-SIG.  These documents and the project implementation proceeded to
evolve at a rapid pace.  Implementation history details can be found
in the `project history file`_.

In November 2001, the reStructuredText parser was nearing completion.
Development of the parser continued with the addition of small
convenience features, improvements to the syntax, the filling in of
gaps, and bug fixes.  After a long holiday break, in early 2002 most
development moved over to the other Docutils components, the
"Readers", "Writers", and "Transforms".  A "standalone" reader
(processes standalone text file documents) was completed in February,
and a basic HTML writer (producing HTML 4.01, using CSS-1) was
completed in early March.

`PEP 287`_, "reStructuredText Standard Docstring Format", was created
to formally propose reStructuredText as a standard format for Python
docstrings, PEPs, and other files.  It was first posted to
comp.lang.python_ and the Python-dev_ mailing list on 2002-04-02.

Version 0.4 of the reStructuredText__ and `Docstring Processing
System`_ projects were released in April 2002.  The two projects were
immediately merged, renamed to "Docutils_", and a 0.1 release soon
followed.

.. __: `reStructuredText SourceForge project`_

.. [#spec-2] The second draft of the spec:

   - `An Introduction to reStructuredText`__
   - `Problems With StructuredText`__
   - `reStructuredText Markup Specification`__
   - `Python Extensions to the reStructuredText Markup
     Specification`__

   __ https://mail.python.org/pipermail/doc-sig/2001-June/001858.html
   __ https://mail.python.org/pipermail/doc-sig/2001-June/001859.html
   __ https://mail.python.org/pipermail/doc-sig/2001-June/001860.html
   __ https://mail.python.org/pipermail/doc-sig/2001-June/001861.html

.. [#peps-1] First drafts of the PEPs:

   - `PEP 256: Docstring Processing System Framework`__
   - `PEP 258: DPS Generic Implementation Details`__
   - `PEP 257: Docstring Conventions`__

   Current working versions of the PEPs can be found in
   https://docutils.sourceforge.io/docs/peps/, and official versions
   can be found in the `master PEP repository`_.

   __ https://mail.python.org/pipermail/doc-sig/2001-June/001855.html
   __ https://mail.python.org/pipermail/doc-sig/2001-June/001856.html
   __ https://mail.python.org/pipermail/doc-sig/2001-June/001857.html


.. _reStructuredText: https://docutils.sourceforge.io/rst.html
.. _StructuredText: https://zopestructuredtext.readthedocs.org/
.. _Setext: https://docutils.sourceforge.io/mirror/setext.html
.. _Docutils: https://docutils.sourceforge.io/
.. _Doc-SIG: https://www.python.org/sigs/doc-sig/
.. _Zope Corporation: http://www.zope.com
.. _ZOPE: https://www.zope.dev
.. _reStructuredText SourceForge project:
   http://structuredtext.sourceforge.net/
.. _pythondoc: http://starship.python.net/crew/danilo/pythondoc/
.. _project history file: ../../../HISTORY.html
.. _PEP 287: ../../peps/pep-0287.html
.. _Docstring Processing System framework: ../../peps/pep-0256.html
.. _comp.lang.python: news:comp.lang.python
.. _Python-dev: https://mail.python.org/pipermail/python-dev/
.. _Docstring Processing System: http://docstring.sourceforge.net/
.. _master PEP repository: https://peps.python.org/
