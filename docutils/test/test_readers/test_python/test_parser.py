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
    """Not a docstring"""
    local = 1
''',
'''\
<Module filename="test data">
    <Function lineno="1" name="f">
'''],
]

totest['class'] = [
['''\
class C:
    """class C's docstring"""
''',
'''\
<Module filename="test data">
    <Class lineno="1" name="C">
        <Docstring lineno="1">
            class C's docstring
'''],
['''\
class C(Super):
    pass

class D(SuperD, package.module.SuperD):
    pass
''',
'''\
<Module filename="test data">
    <Class bases="Super" lineno="1" name="C">
    <Class bases="SuperD package.module.SuperD" lineno="4" name="D">
'''],
['''\
class C:
    class D:
        pass
    """Not a docstring"""
''',
'''\
<Module filename="test data">
    <Class lineno="1" name="C">
'''],
['''\
class C:
    def f(self):
        self.local = 1
        local = 1
''',
'''\
<Module filename="test data">
    <Class lineno="1" name="C">
        <Method lineno="2" name="f">
            <ParameterList lineno="2">
                <Parameter lineno="2" name="self">
'''],
['''\
class C:
    def __init__(self):
        self.local = 1
        local = 1
''',
'''\
<Module filename="test data">
    <Class lineno="1" name="C">
        <Method lineno="2" name="__init__">
            <ParameterList lineno="2">
                <Parameter lineno="2" name="self">
            <Attribute lineno="3" name="self.local">
                <Expression lineno="3">
                    1
            <Attribute lineno="4" name="local">
                <Expression lineno="4">
                    1
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

# @@@ no comments yet
totest['everything'] = [
['''\
# comment

"""Docstring"""

"""Additional docstring"""

__docformat__ = 'reStructuredText'

a = 1
"""Attribute docstring"""

class C(Super):

    """C's docstring"""

    class_attribute = 1
    """class_attribute's docstring"""

    def __init__(self, text=None):
        """__init__'s docstring"""

        self.instance_attribute = (text * 7
                                   + ' whaddyaknow')
        """instance_attribute's docstring"""


def f(x,                            # parameter x
      y=a*5,                        # parameter y
      *args):                       # parameter args
    """f's docstring"""
    return [x + item for item in args]

f.function_attribute = 1
"""f.function_attribute's docstring"""
''',
'''\
<Module filename="test data">
    <Docstring>
        Docstring
    <Docstring lineno="5">
        Additional docstring
    <Attribute lineno="7" name="__docformat__">
        <Expression lineno="7">
            'reStructuredText'
    <Attribute lineno="9" name="a">
        <Expression lineno="9">
            1
        <Docstring lineno="10">
            Attribute docstring
    <Class bases="Super" lineno="12" name="C">
        <Docstring lineno="12">
            C's docstring
        <Attribute lineno="16" name="class_attribute">
            <Expression lineno="16">
                1
            <Docstring lineno="17">
                class_attribute's docstring
        <Method lineno="19" name="__init__">
            <Docstring lineno="19">
                __init__'s docstring
            <ParameterList lineno="19">
                <Parameter lineno="19" name="self">
                <Parameter lineno="19" name="text">
                    <Default lineno="19">
                        None
            <Attribute lineno="22" name="self.instance_attribute">
                <Expression lineno="22">
                    (text * 7 + ' whaddyaknow')
                <Docstring lineno="24">
                    instance_attribute's docstring
    <Function lineno="27" name="f">
        <Docstring lineno="27">
            f's docstring
        <ParameterList lineno="27">
            <Parameter lineno="27" name="x">
            <Parameter lineno="27" name="y">
                <Default lineno="27">
                    a * 5
            <ExcessPositionalArguments lineno="27" name="args">
    <Attribute lineno="33" name="f.function_attribute">
        <Expression lineno="33">
            1
        <Docstring lineno="34">
            f.function_attribute's docstring
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
