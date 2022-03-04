#! /usr/bin/env python3

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for writer_aux transforms.
"""

if __name__ == '__main__':
    import __init__
from test_transforms import DocutilsTestSupport  # before importing docutils!
from docutils.transforms import writer_aux
from docutils.parsers.rst import Parser

def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s


totest = {}

totest['admonitions'] = ((writer_aux.Admonitions,), [
["""\
.. note::

   These are the note contents.

   Another paragraph.
""",
"""\
<document source="test data">
    <admonition classes="note">
        <title>
            Note
        <paragraph>
            These are the note contents.
        <paragraph>
            Another paragraph.
"""],
["""\
.. admonition:: Generic

   Admonitions contents...
""",
"""\
<document source="test data">
    <admonition classes="admonition-generic admonition">
        <title>
            Generic
        <paragraph>
            Admonitions contents...
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
