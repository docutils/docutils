#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for html.py meta directives.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['meta'] = [
["""\
.. meta::
   :description: The reStructuredText plaintext markup language
   :keywords: plaintext,markup language
""",
"""\
<document>
    <meta content="The reStructuredText plaintext markup language" name="description">
    <meta content="plaintext,markup language" name="keywords">
"""],
["""\
.. meta::
   :description lang=en: An amusing story
   :description lang=fr: Un histoire amusant
""",
"""\
<document>
    <meta content="An amusing story" lang="en" name="description">
    <meta content="Un histoire amusant" lang="fr" name="description">
"""],
["""\
.. meta::
   :http-equiv=Content-Type: text/html; charset=ISO-8859-1
""",
"""\
<document>
    <meta content="text/html; charset=ISO-8859-1" http-equiv="Content-Type">
"""],
["""\
.. meta::
   :name: content
     over multiple lines
""",
"""\
<document>
    <meta content="content over multiple lines" name="name">
"""],
["""\
Paragraph

.. meta::
   :name: content
""",
"""\
<document>
    <paragraph>
        Paragraph
    <meta content="content" name="name">
"""],
["""\
.. meta::
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Empty meta directive at line 1.
"""],
["""\
.. meta::
   :empty:
""",
"""\
<document>
    <system_message level="1" type="INFO">
        <paragraph>
            No content for meta tag "empty".
        <literal_block>
            :empty:
    <meta content="" name="empty">
"""],
["""\
.. meta::
   not a field list
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Invalid meta directive at line 2.
        <literal_block>
            .. meta::
               not a field list
"""],
["""\
.. meta::
   :name: content
   not a field
""",
"""\
<document>
    <meta content="content" name="name">
    <system_message level="3" type="ERROR">
        <paragraph>
            Invalid meta directive at line 3.
        <literal_block>
            .. meta::
               :name: content
               not a field
"""],
["""\
.. meta::
   :name notattval: content
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Error parsing meta tag attribute "notattval": missing "="
        <literal_block>
            :name notattval: content
    <meta content="content" name="name">
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
