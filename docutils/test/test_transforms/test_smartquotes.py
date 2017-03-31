#!/usr/bin/env python
# -*- coding: utf-8 -*-
# $Id$
#
# :Copyright: © 2011 Günter Milde.
# :Maintainer: docutils-develop@lists.sourceforge.net
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
    settings = {'smart_quotes': True}
    s = DocutilsTestSupport.TransformTestSuite(
        parser, suite_settings=settings)
    s.generateTests(totest)
    settings['language_code'] = 'de'
    s.generateTests(totest_de)
    settings['smart_quotes'] = 'alternative'
    s.generateTests(totest_de_alt)
    return s


totest = {}
totest_de = {}
totest_de_alt = {}

totest['transitions'] = ((SmartQuotes,), [
["""\
Test "smart quotes", 'secondary smart quotes',
"'nested' smart" quotes
-- and ---also long--- dashes.
""",
u"""\
<document source="test data">
    <paragraph>
        Test “smart quotes”, ‘secondary smart quotes’,
        “‘nested’ smart” quotes
        – and —also long— dashes.
"""],
[r"""Escaped \\"smart quotes\\", \\'secondary smart quotes\\',
\\"\\'nested\\' smart\\" quotes
\\-- and -\\--also long-\\-- dashes.
""",
u"""\
<document source="test data">
    <paragraph>
        Escaped "smart quotes", 'secondary smart quotes',
        "'nested' smart" quotes
        -- and ---also long--- dashes.
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

Do not drop characters from intra-word inline markup like
*re*\ ``Structured``\ *Text*.
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
                    inline “roles”
    <paragraph>
        Do not drop characters from intra-word inline markup like
        <emphasis>
            re
        <literal>
            Structured
        <emphasis>
            Text
        .\
"""],
["""\
.. class:: language-de

German "smart quotes" and 'secondary smart quotes'.

.. class:: language-en-UK-x-altquot

British "primary quotes" use single and
'secondary quotes' double quote signs.

.. class:: language-foo

"Quoting style" for unknown languages is 'ASCII'.

.. class:: language-de-x-altquot

Alternative German "smart quotes" and 'secondary smart quotes'.
""",
u"""\
<document source="test data">
    <paragraph classes="language-de">
        German „smart quotes“ and ‚secondary smart quotes‘.
    <paragraph classes="language-en-uk-x-altquot">
        British ‘primary quotes’ use single and
        “secondary quotes” double quote signs.
    <paragraph classes="language-foo">
        "Quoting style" for unknown languages is 'ASCII'.
    <paragraph classes="language-de-x-altquot">
        Alternative German »smart quotes« and ›secondary smart quotes‹.
    <system_message level="2" line="12" source="test data" type="WARNING">
        <paragraph>
            No smart quotes defined for language "foo".
"""],
])

totest_de['transitions'] = ((SmartQuotes,), [
["""\
German "smart quotes" and 'secondary smart quotes'.

.. class:: language-en

English "smart quotes" and 'secondary smart quotes'.
""",
u"""\
<document source="test data">
    <paragraph>
        German „smart quotes“ and ‚secondary smart quotes‘.
    <paragraph classes="language-en">
        English “smart quotes” and ‘secondary smart quotes’.
"""],
])

totest_de_alt['transitions'] = ((SmartQuotes,), [
["""\
Alternative German "smart quotes" and 'secondary smart quotes'.

In this case, the apostrophe isn't a closing secondary quote!

.. class:: language-en-UK

British "quotes" use single and 'secondary quotes' double quote signs
(there are no alternative quotes defined).

.. class:: language-ro

Romanian "smart quotes" and 'secondary' smart quotes.
""",
u"""\
<document source="test data">
    <paragraph>
        Alternative German »smart quotes« and ›secondary smart quotes‹.
    <paragraph>
        In this case, the apostrophe isn’t a closing secondary quote!
    <paragraph classes="language-en-uk">
        British ‘quotes’ use single and “secondary quotes” double quote signs
        (there are no alternative quotes defined).
    <paragraph classes="language-ro">
        Romanian „smart quotes” and «secondary» smart quotes.
"""],
])

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
