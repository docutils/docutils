#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils/readers/python/moduleparser.py.
"""

from __init__ import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.PythonModuleParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['module'] = [
['''\
''',
'''\
<Module filename="test data">
'''],
['''\
"""docstring"""
''',
'''\
<Module filename="test data">
    <Docstring>
        docstring
'''],
['''\
u"""Unicode docstring"""
''',
'''\
<Module filename="test data">
    <Docstring>
        Unicode docstring
'''],
['''\
"""docstring"""
"""additional docstring"""
''',
'''\
<Module filename="test data">
    <Docstring>
        docstring
    <Docstring lineno="2">
        additional docstring
'''],
['''\
"""docstring"""
# comment
"""additional docstring"""
''',
'''\
<Module filename="test data">
    <Docstring>
        docstring
    <Docstring lineno="3">
        additional docstring
'''],
['''\
"""docstring"""
1
"""not an additional docstring"""
''',
'''\
<Module filename="test data">
    <Docstring>
        docstring
'''],
]

totest['import'] = [
['''\
import module
''',
'''\
<Module filename="test data">
    <Import lineno="1">
        module
'''],
['''\
import module as local
''',
'''\
<Module filename="test data">
    <Import lineno="1">
        module as local
'''],
['''\
import module.name
''',
'''\
<Module filename="test data">
    <Import lineno="1">
        module.name
'''],
['''\
import module.name as local
''',
'''\
<Module filename="test data">
    <Import lineno="1">
        module.name as local
'''],
['''\
import module
"""not documentable"""
''',
'''\
<Module filename="test data">
    <Import lineno="1">
        module
'''],
]

totest['from'] = [
['''\
from module import name
''',
'''\
<Module filename="test data">
    <Import from="module" lineno="1">
        name
'''],
['''\
from module import name as local
''',
'''\
<Module filename="test data">
    <Import from="module" lineno="1">
        name as local
'''],
['''\
from module import name1, name2 as local2
''',
'''\
<Module filename="test data">
    <Import from="module" lineno="1">
        name1
        name2 as local2
'''],
['''\
from module.sub import name
''',
'''\
<Module filename="test data">
    <Import from="module.sub" lineno="1">
        name
'''],
['''\
from module.sub import name as local
''',
'''\
<Module filename="test data">
    <Import from="module.sub" lineno="1">
        name as local
'''],
['''\
from module import *
''',
'''\
<Module filename="test data">
    <Import from="module" lineno="1">
        *
'''],
['''\
from __future__ import division
''',
'''\
<Module filename="test data">
    <Import from="__future__" lineno="1">
        division
'''],
]

totest['assign'] = [
['''\
a = 1
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            1
'''],
['''a = 1''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            1
'''],
['''\
a = 1
"""a's docstring"""
''', #"
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            1
        <Docstring lineno="2">
            a's docstring
'''], #'
['''\
a = 1
"""a's docstring"""
"""additional docstring"""
''', #"
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            1
        <Docstring lineno="2">
            a's docstring
        <Docstring lineno="3">
            additional docstring
'''], #'
['''\
a = 1 + 2 * 3 / 4 ** 5
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            1 + 2 * 3 / 4 ** 5
'''],
['''\
a = 1 \\
    + 2
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            1 + 2
'''],
['''\
a = not 1 and 2 or 3
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            not 1 and 2 or 3
'''],
['''\
a = ~ 1 & 2 | 3 ^ 4
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            ~ 1 & 2 | 3 ^ 4
'''],
['''\
a = `1 & 2`
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            `1 & 2`
'''],
['''\
very_long_name = \\
    x
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="very_long_name">
        <Expression lineno="1">
            x
'''],
['''\
very_long_name \\
    = x
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="very_long_name">
        <Expression lineno="2">
            x
'''],
['''\
very_long_name = \\
    another_long_name = \\
    x
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="very_long_name">
        <Expression lineno="1">
            x
    <Attribute lineno="2" name="another_long_name">
        <Expression lineno="1">
            x
'''],
['''\
a = (1
     + 2)
b = a.b[1 +
        fn(x, y,
           z, {'key': (1 + 2
                       + 3)})][4]
c = """first line
second line
    third"""
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            (1 + 2)
    <Attribute lineno="3" name="b">
        <Expression lineno="3">
            a.b[1 + fn(x, y, z, {'key': (1 + 2 + 3)})][4]
    <Attribute lineno="7" name="c">
        <Expression lineno="7">
            """first line\\nsecond line\\n    third"""
'''],
['''\
a, b, c = range(3)
(d, e,
 f) = a, b, c
g, h, i = j = a, b, c
k.a, k.b.c, k.d.e.f = a, b, c
''',
'''\
<Module filename="test data">
    <AttributeTuple lineno="1" names="a b c">
        <Expression lineno="1">
            range(3)
    <AttributeTuple lineno="2" names="d e f">
        <Expression lineno="3">
            a, b, c
    <AttributeTuple lineno="4" names="g h i">
        <Expression lineno="4">
            a, b, c
    <Attribute lineno="4" name="j">
        <Expression lineno="4">
            a, b, c
    <AttributeTuple lineno="5" names="k.a k.b.c k.d.e.f">
        <Expression lineno="5">
            a, b, c
'''],
['''\
a = 1 ; b = 2
print ; c = 3
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a">
        <Expression lineno="1">
            1
    <Attribute lineno="1" name="b">
        <Expression lineno="1">
            2
    <Attribute lineno="2" name="c">
        <Expression lineno="2">
            3
'''],
['''\
a.b = 1
"""This assignment is noted but ignored unless ``a`` is a function."""
''',
'''\
<Module filename="test data">
    <Attribute lineno="1" name="a.b">
        <Expression lineno="1">
            1
        <Docstring lineno="2">
            This assignment is noted but ignored unless ``a`` is a function.
'''],
['''\
a[b] = 1
"""This assignment is noted but ignored unless ``a`` is a function."""
''',
'''\
<Module filename="test data">
'''],
]

totest['def'] = [
['''\
def f():
    """Function f's docstring"""
    """Additional docstring"""
    local = 1
    """Not a docstring, since ``local`` is local."""
''', # "
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
        <Docstring lineno="1">
            Function f's docstring
        <Docstring lineno="3">
            Additional docstring
'''], # '
['''\
def f(a, b):
    local = 1
''',
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
        <ParameterList lineno="1">
            <Parameter lineno="1" name="a">
            <Parameter lineno="1" name="b">
'''],
['''\
def f(a=None, b=1):
    local = 1
''',
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
        <ParameterList lineno="1">
            <Parameter lineno="1" name="a">
                <Default lineno="1">
                    None
            <Parameter lineno="1" name="b">
                <Default lineno="1">
                    1
'''],
['''\
def f(a, (b, c, d)=range(3),
      e=None):
    local = 1
''',
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
        <ParameterList lineno="1">
            <Parameter lineno="1" name="a">
            <ParameterTuple lineno="1" names="(b, c, d)">
                <Default lineno="1">
                    range(3)
            <Parameter lineno="1" name="e">
                <Default lineno="1">
                    None
'''],
['''\
def f(*args):
    local = 1
''',
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
        <ParameterList lineno="1">
            <ExcessPositionalArguments lineno="1" name="args">
'''],
['''\
def f(**kwargs):
    local = 1
''',
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
        <ParameterList lineno="1">
            <ExcessKeywordArguments lineno="1" name="kwargs">
'''],
['''\
def f(a, b=None, *args, **kwargs):
    local = 1
''',
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
        <ParameterList lineno="1">
            <Parameter lineno="1" name="a">
            <Parameter lineno="1" name="b">
                <Default lineno="1">
                    None
            <ExcessPositionalArguments lineno="1" name="args">
            <ExcessKeywordArguments lineno="1" name="kwargs">
'''],
['''\
def f():
    pass
f.attrib = 1
"""f.attrib's docstring"""
''', # "
# @@@ When should the Attribute move inside the Function?
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
    <Attribute lineno="3" name="f.attrib">
        <Expression lineno="3">
            1
        <Docstring lineno="4">
            f.attrib's docstring
'''], # '
['''\
def f():
    def g():
        pass
    local = 1
''',
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
'''],
]

totest['ignore'] = [
['''\
1 + 2
''',
'''\
<Module filename="test data">
'''],
['''\
del a
''',
'''\
<Module filename="test data">
'''],
]

"""
['''\
''',
'''\
'''],
"""

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
