#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Test module for nodes.py.
"""

import unittest
from types import ClassType
from DocutilsTestSupport import nodes, utils

debug = 0


class TextTests(unittest.TestCase):

    def setUp(self):
        self.text = nodes.Text('Line 1.\nLine 2.')

    def test_repr(self):
        self.assertEquals(repr(self.text), r"<#text: 'Line 1.\nLine 2.'>")

    def test_str(self):
        self.assertEquals(str(self.text), 'Line 1.\nLine 2.')

    def test_astext(self):
        self.assertEquals(self.text.astext(), 'Line 1.\nLine 2.')

    def test_pformat(self):
        self.assertEquals(self.text.pformat(), 'Line 1.\nLine 2.\n')


class ElementTests(unittest.TestCase):

    def test_empty(self):
        element = nodes.Element()
        self.assertEquals(repr(element), '<Element: >')
        self.assertEquals(str(element), '<Element/>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(), '<Element/>')
        dom.unlink()
        element['attr'] = '1'
        self.assertEquals(repr(element), '<Element: >')
        self.assertEquals(str(element), '<Element attr="1"/>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(), '<Element attr="1"/>')
        dom.unlink()
        self.assertEquals(element.pformat(), '<Element attr="1">\n')
        del element['attr']
        element['mark'] = u'\u2022'
        self.assertEquals(repr(element), '<Element: >')
        self.assertEquals(str(element), '<Element mark="\\u2022"/>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(), u'<Element mark="\u2022"/>')
        dom.unlink()

    def test_withtext(self):
        element = nodes.Element('text\nmore', nodes.Text('text\nmore'))
        self.assertEquals(repr(element), r"<Element: <#text: 'text\nmore'>>")
        self.assertEquals(str(element), '<Element>text\nmore</Element>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(), '<Element>text\nmore</Element>')
        dom.unlink()
        element['attr'] = '1'
        self.assertEquals(repr(element), r"<Element: <#text: 'text\nmore'>>")
        self.assertEquals(str(element),
                          '<Element attr="1">text\nmore</Element>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(),
                          '<Element attr="1">text\nmore</Element>')
        dom.unlink()
        self.assertEquals(element.pformat(),
                          '<Element attr="1">\n    text\n    more\n')

    def test_clear(self):
        element = nodes.Element()
        element += nodes.Element()
        self.assert_(len(element))
        element.clear()
        self.assert_(not len(element))


class MiscTests(unittest.TestCase):

    def test_node_class_names(self):
        node_class_names = []
        for x in dir(nodes):
            c = getattr(nodes, x)
            if type(c) is ClassType and issubclass(c, nodes.Node) \
                   and len(c.__bases__) > 1:
                node_class_names.append(x)
        node_class_names.sort()
        nodes.node_class_names.sort()
        self.assertEquals(node_class_names, nodes.node_class_names)

    ids = [('a', 'a'), ('A', 'a'), ('', ''), ('a b \n c', 'a-b-c'),
           ('a.b.c', 'a-b-c'), (' - a - b - c - ', 'a-b-c'), (' - ', ''),
           (u'\u2020\u2066', ''), (u'a \xa7 b \u2020 c', 'a-b-c'),
           ('1', ''), ('1abc', 'abc')]

    def test_make_id(self):
        for input, output in self.ids:
            normed = nodes.make_id(input)
            self.assertEquals(normed, output)

    def test_has_children(self):
        self.assert_(not nodes.Text('some text').has_children())
        self.assert_(not nodes.Node().has_children())
        e = nodes.TextElement()
        self.assert_(not e.has_children())
        e += nodes.Text('some text')
        self.assert_(e.has_children())
        self.assert_(not e[0].has_children())

    def getlist(self, n, **kwargs):
        r = []
        while n is not None:
            n = n.next_node(**kwargs)
            r.append(n)
        return r[:-1]

    def test_next_node(self):
        getlist = self.getlist
        e = nodes.Element()
        e += nodes.Element()
        e[0] += nodes.Element()
        e[0] += nodes.Element()
        e[0][1] += nodes.Text('some text')
        e += nodes.Element()
        e += nodes.Element()
        i = e
        l = []
        self.assertEquals(getlist(e),
                          [e[0], e[0][0], e[0][1], e[0][1][0], e[1], e[2]])
        self.assertEquals(getlist(e, descend=0), [])
        self.assertEquals(getlist(e[0], descend=0), [e[1], e[2]])
        self.assertEquals(getlist(e[0][0], descend=0), [e[0][1], e[1], e[2]])
        self.assertEquals(getlist(e, ascend=0),
                          [e[0], e[0][0], e[0][1], e[0][1][0]])
        self.assertEquals(getlist(e[0][0], descend=0, ascend=0), [e[0][1]])
        self.assertEquals(getlist(e, cond=lambda x: x not in e[0:2]),
                          [e[0][0], e[0][1], e[0][1][0], e[2]])


class TreeCopyVisitorTests(unittest.TestCase):

    def setUp(self):
        document = utils.new_document('test data')
        document += nodes.paragraph('', 'Paragraph 1.')
        blist = nodes.bullet_list()
        for i in range(1, 6):
            item = nodes.list_item()
            for j in range(1, 4):
                item += nodes.paragraph('', 'Item %s, paragraph %s.' % (i, j))
            blist += item
        document += blist
        self.document = document

    def compare_trees(self, one, two):
        self.assertEquals(one.__class__, two.__class__)
        self.assertNotEquals(id(one), id(two))
        children1 = one.get_children()
        children2 = two.get_children()
        self.assertEquals(len(children1), len(children2))
        for i in range(len(children1)):
            self.compare_trees(children1[i], children2[i])

    def test_copy_whole(self):
        visitor = nodes.TreeCopyVisitor(self.document)
        self.document.walkabout(visitor)
        newtree = visitor.get_tree_copy()
        self.assertEquals(self.document.pformat(), newtree.pformat())
        self.compare_trees(self.document, newtree)


class MiscFunctionTests(unittest.TestCase):

    names = [('a', 'a'), ('A', 'a'), ('A a A', 'a a a'),
             ('A  a  A  a', 'a a a a'),
             ('  AaA\n\r\naAa\tAaA\t\t', 'aaa aaa aaa')]

    def test_normalize_name(self):
        for input, output in self.names:
            normed = nodes.fully_normalize_name(input)
            self.assertEquals(normed, output)


if __name__ == '__main__':
    unittest.main()
