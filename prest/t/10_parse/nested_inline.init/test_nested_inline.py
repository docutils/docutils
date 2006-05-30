#! /usr/bin/env python

# Copy this file to docutils/test/test_parsers/test_rst/ and do
# ``chmod +x test_inline_markup.py``, then execute this file to test.

# To be added (later) to
# docutils/test/test_parsers/test_rst/test_inline_markup.py?

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['nested'] = [
["""\
*emphasis **strong***
""",
"""\
<document source="test data">
    <paragraph>
        <emphasis>
            emphasis \n\
            <strong>
                strong
"""],
["""\
*emphasis **strong*
""",
"""\
<document source="test data">
    <paragraph>
        <emphasis>
            emphasis \n\
            <problematic ids="id2" refid="id1">
                **
            strong
    <system_message backrefs="id2" ids="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline strong start-string without end-string.
"""],
["""\
*emphasis **strong**
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="id2" refid="id1">
            *
        emphasis \n\
        <strong>
            strong
    <system_message backrefs="id2" ids="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
"""],
["""\
*emphasized ``literal`` and |substitution ref| and ref_*
""",
"""\
<document source="test data">
    <paragraph>
        <emphasis>
            emphasized \n\
            <literal>
                literal
             and \n\
            <substitution_reference refname="substitution ref">
                substitution ref
             and \n\
            <reference name="ref" refname="ref">
                ref
"""],
["""\
Explicit roles for standard inline markup:

:emphasis:`emphasis including :strong:`strong
including :literal:`inline literal text```.
""",
"""\
<document source="test data">
    <paragraph>
        Explicit roles for standard inline markup:
    <paragraph>
        <emphasis>
            emphasis including \n\
            <strong>
                strong
                including \n\
                <literal>
                    inline literal text
        .
"""],
["""\
Suffix-based nested explicit roles:

`\ `\ `inline literal text`:literal: inside
strong`:strong: within emphasis`:emphasis:.
""",
"""\
<document source="test data">
    <paragraph>
        Suffix-based nested explicit roles:
    <paragraph>
        <emphasis>
            <strong>
                <literal>
                    inline literal text
                 inside
                strong
             within emphasis
        .
"""],
["""\
``literal *doesn't* **get** `parsed```
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            literal *doesn't* **get** `parsed`
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
