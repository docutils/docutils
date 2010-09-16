#! /usr/bin/env python

# Author: Jon Waltman <jonathan.waltman@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Tests for Texinfo writer.
"""

import os
import os.path
import sys

import texinfo

sys.path.insert(0, os.path.abspath('../../docutils/test'))
import DocutilsTestSupport

def suite():
    settings = {}
    s = DocutilsTestSupport.PublishTestSuite('texinfo', suite_settings=settings)
    s.generateTests(totest)
    return s

template = texinfo.TEMPLATE

parts = dict(
    filename='<string>.info',
    direntry='',
    title='<untitled>',
    paragraphindent='2',
    exampleindent='4',
    preamble='',
    body='\n',
    )

totest = {}

totest['blank'] = [
# input
["",
## # expected output
template % dict(parts)],
]


totest['escaping'] = [
# input
[r"""
Escaping -- @,{}:
*****************

@commands{ use braces }.

@ { } @@ {{ }}

@} @@}}

@{ @@{{

""",
## # expected output
template % dict(parts,
                title='Escaping --',
                body=
r"""@@commands@{ use braces @}.

@@ @{ @} @@@@ @{@{ @}@}

@@@} @@@@@}@}

@@@{ @@@@@{@{
""")],
]



totest['table_with_markup'] = [
# input
[r"""
+------------+---------------------+--------------------+
|  emphasis  |   strong emphasis   |   inline literal   |
+============+=====================+====================+
| *emphasis* | **strong emphasis** | ``inline literal`` |
+------------+---------------------+--------------------+
""",
## # expected output
template % dict(parts,
                body=
r"""@multitable {xxxxxxxxxxxxxx} {xxxxxxxxxxxxxxxxxxxxxxx} {xxxxxxxxxxxxxxxxxxxxxx}
@headitem emphasis
@tab strong emphasis
@tab inline literal
@item @emph{emphasis}
@tab @strong{strong emphasis}
@tab @code{inline literal}
@end multitable
""")],
]

totest['sections'] = [
# input
[r"""
Top
***

This tests sectioning.

Section 1
=========

SubSection 1.1
--------------

SubSubSection 1.1.1
~~~~~~~~~~~~~~~~~~~

Section 2
=========

""",
## # expected output
template % dict(parts,
                title='Top',
                body=
r"""This tests sectioning.


@menu
* Section 1::
* Section 2::

@detailmenu
 --- The Detailed Node Listing ---

Section 1

* SubSection 1 1::
@end detailmenu
@end menu


@node Section 1,Section 2,Top,Top
@anchor{section 1}@anchor{0}@anchor{section-1}
@chapter Section 1

@menu
* SubSection 1 1::
@end menu


@node SubSection 1 1,,,Section 1
@anchor{subsection 1 1}@anchor{1}@anchor{subsection-1-1}
@section SubSection 1.1

@menu
* SubSubSection 1 1 1::
@end menu


@node SubSubSection 1 1 1,,,SubSection 1 1
@anchor{subsubsection 1 1 1}@anchor{2}@anchor{subsubsection-1-1-1}
@subsection SubSubSection 1.1.1

@node Section 2,,Section 1,Top
@anchor{section 2}@anchor{3}@anchor{section-2}
@chapter Section 2
""")],
]

totest['duplicate_sections'] = [
# input
[r"""
Duplicate
=========

Duplicate
=========

Duplicate
=========

""",
 ## # expected output
template % dict(parts,
                title='Duplicate',
                body=
r"""@menu
* Duplicate: Duplicate<2>.
* Duplicate: Duplicate<3>.
@end menu


@node Duplicate<2>,Duplicate<3>,Top,Top
@anchor{duplicate<2>}@anchor{0}@anchor{1}@anchor{id1}
@unnumbered Duplicate

@node Duplicate<3>,,Duplicate<2>,Top
@anchor{duplicate<3>}@anchor{2}@anchor{id2}
@unnumbered Duplicate
""")],
]


totest['comments'] = [
# input
[r"""
.. Foo
   bar

""",
 ## # expected output
template % dict(parts,
                body=
r"""@c Foo
@c bar
""")],
]

totest['docinfo'] = [
# input
[r"""
Doc Info Test
=============

:Author: Jon
:Copyright: This document has been placed in the public domain.

Example...
""",
 ## # expected output
template % dict(parts,
                title='Doc Info Test',
                body=
r"""@itemize @w
@item Author:
Jon

@item Copyright:
This document has been placed in the public domain.
@end itemize

Example...
""")],
]


totest['admonitions'] = [
# input
[r"""
Admonitions
===========

.. Note::

   Hooty hoo!

""",
 ## # expected output
template % dict(parts,
                title='Admonitions',
                body=
r"""@cartouche
@quotation Note
Hooty hoo!
@end quotation
@end cartouche
""")],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
