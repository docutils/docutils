.. The following block contains definitions of external targets. Some of them
   are explicit and some are defined through substitutions.

This is a simple test file for |xrefwrt|_. The |xrefwrt|_ is part of trip_,
a parser and writer for |rst| written in Perl_.

.. |xrefwrt| replace:: the xref writer
.. _xrefwrt: ../../../../../src/writers/xref.wrt
.. _trip: ../../../../../doc
.. |rst| replace:: reStructuredText_
.. _reStructuredText: http://docutils.sourceforge.net/rst.html
.. _Perl: http://www.perl.org

Xref Writer Tests
=================

.. _rst:

The |xrefwrt|_ dumps internal targets in the source |rst|. The output is
in |rst| format and includes the following:

+ All |nait|_, which could be `direct`_ or `indirect`_ or `inline`_.
+ All citations_, in this case [citation1]_ and [citation2]_.
+ Any `substitution definition`_ that has the same name as a |nait|_.
+ All `raw`_ or `image`_ elements embedded in the above definitions.
  These elements are exported as `substitution definition`_ themselves.

No `anonymous internal target`__ is exported. Footnotes_, for example
[#]_ and [#]_ are not exported.

__ anonymous_

.. Here are some internal targets: indirect is indirect. nait is the name of
   both an internal target and a substitution.

.. _indirect: indirect2_

.. |nait| replace:: non-anonymous **internal** targets


Direct Target Tests
--------------------

.. _nait:

Non-Anonymous Target Tests
***************************

.. _direct: 

Direct Targets
**************

.. _indirect2:

Indirect Target Tests
----------------------

.. _anonymous:

Anonymous Target Tests
----------------------

.. _supersub:

Other Tests
------------

.. _big one:
.. _smiley_gif:
.. _image:

.. image:: smiley.gif
   :scale: 100
   :alt: large smiley face wizard

_`Inline` internal targets tests are here.

.. _supersub2:

_`Substitution definition` tests are here. Here is "|supersub2|", built from
a substitution. Unlike |supersub|_, it's not a pointer itself.

Here is _`another internal target`. It will be exported even though
it's not referred to in this document.

.. _fpointer:

Here is |fpointer|.

.. |fpointer| replace:: a pointer to footnote [3]_

.. |supersub| replace:: another |smiley| |smiley_gif2|, **smaller** than 
   the `big one`_

.. |supersub2| replace:: another |smiley| |smiley_gif|_, ``larger``
   than the `big one`_

.. |smiley| replace:: *smiley face*

.. |smiley_gif2| image:: smiley.gif
   :scale: 50
   :alt: small smiley face wizard

.. |smiley_gif| image:: smiley.gif
   :scale: 150
   :alt: extra large smiley face wizard

.. _citations:

.. [citation1] first citation
.. [citation2] first citation

.. _footnotes:

.. [#] foot note 1
.. [#] foot note 2
.. [3] foot note 3

.. _raw:
.. _rawdata1:
.. _rawdata2:

Here is a piece of raw html data.

.. raw:: html

   <pre>
	A literal block.
   </pre>

This, |supersub3|, is also raw html.

.. |supersub3| replace:: |rawdata1|

.. |rawdata1| raw:: html

   <br><b> this should be bold </b><br>

This |rawdata2| contains raw html and an internal target.

.. |rawdata2| replace:: |rawdata1|, _`yet another internal target`

