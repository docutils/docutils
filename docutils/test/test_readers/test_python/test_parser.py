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

# totest['assign'] = [
# ['''\
# a = 1
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="a">
#         <Expression lineno="1">
#             1
# '''],
# ['''\
# a = 1
# """a's docstring"""
# ''', #"
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="a">
#         <Expression lineno="1">
#             1
#         <Docstring lineno="2">
#             a's docstring
# '''], #'
# ['''\
# a = 1
# """a's docstring"""
# """additional docstring"""
# ''', #"
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="a">
#         <Expression lineno="1">
#             1
#         <Docstring lineno="2">
#             a's docstring
#         <Docstring lineno="3">
#             additional docstring
# '''], #'
# ['''\
# a = 1 + 2 * 3 / 4 ** 5
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="a">
#         <Expression lineno="1">
#             1 + 2 * 3 / 4 ** 5
# '''],
# ['''\
# a = 1 \\
#     + 2
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="a">
#         <Expression lineno="1">
#             1 + 2
# '''],
# ['''\
# a = not 1 and 2 or 3
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="a">
#         <Expression lineno="1">
#             not 1 and 2 or 3
# '''],
# ['''\
# a = ~ 1 & 2 | 3 ^ 4
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="a">
#         <Expression lineno="1">
#             ~ 1 & 2 | 3 ^ 4
# '''],
# ['''\
# a = `1 & 2`
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="a">
#         <Expression lineno="1">
#             `1 & 2`
# '''],
# ['''\
# very_long_name = \\
#     x
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="very_long_name">
#         <Expression lineno="1">
#             x
# '''],
# ['''\
# very_long_name \\
#     = x
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="very_long_name">
#         <Expression lineno="1">
#             x
# '''],
# ['''\
# very_long_name = \\
#     another_long_name = \\
#     x
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="very_long_name">
#         <Expression lineno="1">
#             x
# '''],
# ['''\
# a = (1
#      + 2)
# b = a.b[1 +
#         fn(x, y,
#            z, {'key': (1 + 2
#                        + 3)})]
# c = """first line
# second line
#    third"""
# ''',
# '''\
# <Module filename="test data">
#     <Attribute lineno="1" name="a">
#         <Expression lineno="1">
#             (1 + 2)
#     <Attribute lineno="3" name="b">
#         <Expression lineno="3">
#             a.b[1 + fn(x, y, z, {'key': (1 + 2 + 3)})]
#     <Attribute lineno="7" name="c">
#         <Expression lineno="7">
#             """first line\\nsecond line\\n    third"""
# '''],
# ['''\
# a, b, c = range(3)
# (d, e,
#  f) = a, b, c
# ''',
# '''\
# '''],
# ]

# totest['def'] = [
# ['''\
# def f():
#     pass
# ''',
# '''\
# '''],
# ]

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
