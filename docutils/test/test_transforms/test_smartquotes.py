#!/usr/bin/env python
# -*- coding: utf8 -*-

# $Id$

# :Copyright: © 2011 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: http://www.spdx.org/licenses/BSD-2-Clause

"""
Test module for universal.SmartQuotes transform.
"""


from __init__ import DocutilsTestSupport # must be imported before docutils
from docutils.transforms.universal import SmartQuotes
from docutils.parsers.rst import Parser

def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(
        parser, suite_settings={'smart_quotes': True})
    s.generateTests(totest)
    return s


totest = {}

totest['transitions'] = ((SmartQuotes,), [
["""\
Test "smart quotes", 'single smart quotes',
"'nested' smart" quotes
-- and ---also long--- dashes.
""",
u"""\
<document source="test data">
    <paragraph>
        Test “smart quotes”, ‘single smart quotes’,
        “‘nested’ smart” quotes
        – and —also long— dashes.
"""],
["""\
Do not "educate" quotes ``inside "literal" text`` and ::
 
  "literal" blocks.

Keep quotes straight in code and math: 
:code:`print "hello"` :math:`1' 12"`.

.. code::

   print "hello"
   
.. math::

   f'(x) = df(x)/dx
  
""",
u"""\
<document source="test data">
    <paragraph>
        Do not “educate” quotes 
        <literal>
            inside "literal" text
         and
    <literal_block xml:space="preserve">
        "literal" blocks.
    <paragraph>
        Keep quotes straight in code and math:
        <literal classes="code">
            print "hello"
         
        <math>
            1' 12"
        .
    <literal_block classes="code" xml:space="preserve">
        print "hello"
    <math_block xml:space="preserve">
        f'(x) = df(x)/dx
"""],
["""\
Quotes and inline-elements:

* Around "_`targets`", "*emphasized*" or "``literal``" text
  and links to "targets_".                                          

* Inside *"emphasized"* or other `inline "roles"`
""",
u"""\
<document source="test data">
    <paragraph>
        Quotes and inline-elements:
    <bullet_list bullet="*">
        <list_item>
            <paragraph>
                Around “
                <target ids="targets" names="targets">
                    targets
                ”, “
                <emphasis>
                    emphasized
                ” or “
                <literal>
                    literal
                ” text
                and links to “
                <reference name="targets" refname="targets">
                    targets
                ”.
        <list_item>
            <paragraph>
                Inside \n\
                <emphasis>
                    “emphasized”
                 or other \n\
                <title_reference>
                    inline “roles”\
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
