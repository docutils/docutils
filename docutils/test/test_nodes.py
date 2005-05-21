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

    def test_normal_attributes(self):
        element = nodes.Element()
        self.assert_(not element.has_key('foo'))
        self.assertRaises(KeyError, element.__getitem__, 'foo')
        element['foo'] = 'sometext'
        self.assertEquals(element['foo'], 'sometext')
        del element['foo']
        self.assertRaises(KeyError, element.__getitem__, 'foo')

    def test_default_attributes(self):
        element = nodes.Element()
        self.assertEquals(element['ids'], [])
        self.assertEquals(element.non_default_attributes(), {})
        self.assert_(not element.is_not_default('ids'))
        self.assert_(element['ids'] is not nodes.Element()['ids'])
        element['ids'].append('someid')
        self.assertEquals(element['ids'], ['someid'])
        self.assertEquals(element.non_default_attributes(),
                          {'ids': ['someid']})
        self.assert_(element.is_not_default('ids'))


class MiscTests(unittest.TestCase):

    def test_node_class_names(self):
        node_class_names = []
        for x in dir(nodes):
            c = getattr(nodes, x)
            if isinstance(c, ClassType) and issubclass(c, nodes.Node) \
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

    def test_traverse(self):
        e = nodes.Element()
        e += nodes.Element()
        e[0] += nodes.Element()
        e[0] += nodes.TextElement()
        e[0][1] += nodes.Text('some text')
        e += nodes.Element()
        e += nodes.Element()
        self.assertEquals(list(e.traverse()),
                          [e, e[0], e[0][0], e[0][1], e[0][1][0], e[1], e[2]])
        self.assertEquals(list(e.traverse(include_self=0)),
                          [e[0], e[0][0], e[0][1], e[0][1][0], e[1], e[2]])
        self.assertEquals(list(e.traverse(descend=0)),
                          [e])
        self.assertEquals(list(e[0].traverse(descend=0, ascend=1)),
                          [e[0], e[1], e[2]])
        self.assertEquals(list(e[0][0].traverse(descend=0, ascend=1)),
                          [e[0][0], e[0][1], e[1], e[2]])
        self.assertEquals(list(e[0][0].traverse(descend=0, siblings=1)),
                          [e[0][0], e[0][1]])
        self.testlist = e[0:2]
        self.assertEquals(list(e.traverse(condition=self.not_in_testlist)),
                          [e, e[0][0], e[0][1], e[0][1][0], e[2]])
        # Return siblings despite siblings=0 because ascend is true.
        self.assertEquals(list(e[1].traverse(ascend=1, siblings=0)),
                          [e[1], e[2]])
        self.assertEquals(list(e[0].traverse()),
                          [e[0], e[0][0], e[0][1], e[0][1][0]])
        self.testlist = [e[0][0], e[0][1]]
        self.assertEquals(list(e[0].traverse(condition=self.not_in_testlist)),
                               [e[0], e[0][1][0]])
        self.testlist.append(e[0][1][0])
        self.assertEquals(list(e[0].traverse(condition=self.not_in_testlist)),
                               [e[0]])

    def test_next_node(self):
        e = nodes.Element()
        e += nodes.Element()
        e[0] += nodes.Element()
        e[0] += nodes.TextElement()
        e[0][1] += nodes.Text('some text')
        e += nodes.Element()
        e += nodes.Element()
        self.testlist = [e[0], e[0][1], e[1]]
        compare = [(e, e[0][0]),
                   (e[0], e[0][0]),
                   (e[0][0], e[0][1][0]),
                   (e[0][1], e[0][1][0]),
                   (e[0][1][0], e[2]),
                   (e[1], e[2]),
                   (e[2], None)]
        for node, next_node in compare:
            self.assertEquals(node.next_node(self.not_in_testlist, ascend=1),
                              next_node)
        self.assertEquals(e[0][0].next_node(ascend=1), e[0][1])
        self.assertEquals(e[2].next_node(), None)

    def not_in_testlist(self, x):
        return x not in self.testlist


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
        self.assertEquals(len(one.children), len(two.children))
        for i in range(len(one.children)):
            self.compare_trees(one.children[i], two.children[i])

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

    def test_set_id_default(self):
        # Default prefixes.
        document = utils.new_document('test')
        # From name.
        element = nodes.Element(names=['test'])
        document.set_id(element)
        self.assertEquals(element['ids'], ['test'])
        # Auto-generated.
        element = nodes.Element()
        document.set_id(element)
        self.assertEquals(element['ids'], ['id1'])

    def test_set_id_custom(self):
        # Custom prefixes.
        document = utils.new_document('test')
        # Change settings.
        document.settings.id_prefix = 'prefix'
        document.settings.auto_id_prefix = 'auto'
        # From name.
        element = nodes.Element(names=['test'])
        document.set_id(element)
        self.assertEquals(element['ids'], ['prefixtest'])
        # Auto-generated.
        element = nodes.Element()
        document.set_id(element)
        self.assertEquals(element['ids'], ['prefixauto1'])
        

if __name__ == '__main__':
    unittest.main()
