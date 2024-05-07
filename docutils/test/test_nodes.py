#! /usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Test module for nodes.py.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from docutils import nodes, utils

debug = False


class NodeTests(unittest.TestCase):

    def not_in_testlist(self, x):
        # function to use in `condition` argument in findall() and next_node()
        return x not in self.testlist

    def test_findall(self):
        # `findall()` is defined in class Node,
        # we test with a tree of Element instances (simpler setup)
        e = nodes.Element()
        e += nodes.Element()
        e[0] += nodes.Element()
        e[0] += nodes.TextElement()
        e[0][1] += nodes.Text('some text')
        e += nodes.Element()
        e += nodes.Element()
        self.assertEqual(list(e.findall()),
                         [e, e[0], e[0][0], e[0][1], e[0][1][0], e[1], e[2]])
        self.assertEqual(list(e.findall(include_self=False)),
                         [e[0], e[0][0], e[0][1], e[0][1][0], e[1], e[2]])
        self.assertEqual(list(e.findall(descend=False)),
                         [e])
        self.assertEqual(list(e[0].findall(descend=False, ascend=True)),
                         [e[0], e[1], e[2]])
        self.assertEqual(list(e[0][0].findall(descend=False, ascend=True)),
                         [e[0][0], e[0][1], e[1], e[2]])
        self.assertEqual(list(e[0][0].findall(descend=False, siblings=True)),
                         [e[0][0], e[0][1]])
        self.testlist = e[0:2]
        self.assertEqual(list(e.findall(condition=self.not_in_testlist)),
                         [e, e[0][0], e[0][1], e[0][1][0], e[2]])
        # Return siblings despite siblings=False because ascend is true.
        self.assertEqual(list(e[1].findall(ascend=True, siblings=False)),
                         [e[1], e[2]])
        self.assertEqual(list(e[0].findall()),
                         [e[0], e[0][0], e[0][1], e[0][1][0]])
        self.testlist = [e[0][0], e[0][1]]
        self.assertEqual(list(e[0].findall(condition=self.not_in_testlist)),
                         [e[0], e[0][1][0]])
        self.testlist.append(e[0][1][0])
        self.assertEqual(list(e[0].findall(condition=self.not_in_testlist)),
                         [e[0]])
        self.assertEqual(list(e.findall(nodes.TextElement)), [e[0][1]])

    def test_findall_duplicate_texts(self):
        e = nodes.Element()
        e += nodes.TextElement()
        e[0] += nodes.Text('one')  # e[0][0]
        e[0] += nodes.Text('two')  # e[0][1]
        e[0] += nodes.Text('three')  # e[0][2]
        e[0] += nodes.Text('two')  # e[0][3]    same value as e[0][1]
        e[0] += nodes.Text('five')  # e[0][4]
        full_list = list(e[0][0].findall(siblings=True))
        self.assertEqual(len(full_list), 5)
        for i in range(5):
            self.assertIs(full_list[i], e[0][i])

        partial_list = list(e[0][3].findall(siblings=True))
        self.assertEqual(len(partial_list), 2)
        self.assertIs(partial_list[0], e[0][3])
        self.assertIs(partial_list[1], e[0][4])

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
            self.assertEqual(node.next_node(self.not_in_testlist, ascend=True),
                             next_node)
        self.assertEqual(e[0][0].next_node(ascend=True), e[0][1])
        self.assertEqual(e[2].next_node(), None)


class TextTests(unittest.TestCase):

    text = nodes.Text('Line 1.\n\x00rad två.')
    longtext = nodes.Text('Mary had a little lamb '
                          'whose fleece was white as snow '
                          'and everwhere that Mary went '
                          'the lamb was sure to go.')

    def test_value_type_check(self):
        # data must by `str` instance, no `bytes` allowed
        with self.assertRaises(TypeError):
            nodes.Text(b'hol')

    def test_Text_rawsource_deprection_warning(self):
        with self.assertWarnsRegex(DeprecationWarning,
                                   '"rawsource" is ignored'):
            nodes.Text('content', rawsource='content')

    def test_str(self):
        self.assertEqual(str(self.text), 'Line 1.\n\x00rad två.')

    def test_repr(self):
        self.assertEqual(repr(self.text), r"<#text: 'Line 1.\n\x00rad två.'>")
        self.assertEqual(self.text.shortrepr(),
                         r"<#text: 'Line 1.\n\x00rad två.'>")

    def test_repr_long_text(self):
        self.assertEqual(repr(self.longtext), r"<#text: 'Mary had a "
                         r"little lamb whose fleece was white as snow "
                         r"and everwh ...'>")
        self.assertEqual(self.longtext.shortrepr(),
                         r"<#text: 'Mary had a lit ...'>")

    def test_astext(self):
        self.assertEqual(self.text.astext(), 'Line 1.\nrad två.')

    def test_pformat(self):
        self.assertTrue(isinstance(self.text.pformat(), str))
        self.assertEqual(self.text.pformat(), 'Line 1.\nrad två.\n')

    def test_strip(self):
        text = nodes.Text(' was noch ')
        stripped = text.lstrip().rstrip()
        stripped2 = text.lstrip(' wahn').rstrip(' wahn')
        self.assertEqual(stripped, 'was noch')
        self.assertEqual(stripped2, 's noc')

    def test_comparison(self):
        # Text nodes are compared by value
        self.assertEqual(self.text, nodes.Text('Line 1.\n\x00rad två.'))


class ElementTests(unittest.TestCase):

    def test_empty(self):
        element = nodes.Element()
        self.assertEqual(repr(element), '<Element: >')
        self.assertEqual(str(element), '<Element/>')
        dom = element.asdom()
        self.assertEqual(dom.toxml(), '<Element/>')
        dom.unlink()
        element['attr'] = '1'
        self.assertEqual(repr(element), '<Element: >')
        self.assertEqual(str(element), '<Element attr="1"/>')
        dom = element.asdom()
        self.assertEqual(dom.toxml(), '<Element attr="1"/>')
        dom.unlink()
        self.assertEqual(element.pformat(), '<Element attr="1">\n')
        del element['attr']
        element['mark'] = '\u2022'
        self.assertEqual(repr(element), '<Element: >')
        self.assertEqual(str(element), '<Element mark="\u2022"/>')
        dom = element.asdom()
        self.assertEqual(dom.toxml(), '<Element mark="\u2022"/>')
        dom.unlink()
        element['names'] = ['nobody', 'имя', 'näs']
        self.assertEqual(repr(element), '<Element "nobody; имя; näs": >')
        self.assertTrue(isinstance(repr(element), str))

    def test_withtext(self):
        element = nodes.Element('text\nmore', nodes.Text('text\nmore'))
        uelement = nodes.Element('grün', nodes.Text('grün'))
        self.assertEqual(repr(element), r"<Element: <#text: 'text\nmore'>>")
        self.assertEqual(repr(uelement), "<Element: <#text: 'grün'>>")
        self.assertTrue(isinstance(repr(uelement), str))
        self.assertEqual(str(element), '<Element>text\nmore</Element>')
        self.assertEqual(str(uelement), '<Element>gr\xfcn</Element>')
        dom = element.asdom()
        self.assertEqual(dom.toxml(), '<Element>text\nmore</Element>')
        dom.unlink()
        element['attr'] = '1'
        self.assertEqual(repr(element), r"<Element: <#text: 'text\nmore'>>")
        self.assertEqual(str(element),
                         '<Element attr="1">text\nmore</Element>')
        dom = element.asdom()
        self.assertEqual(dom.toxml(),
                         '<Element attr="1">text\nmore</Element>')
        dom.unlink()
        self.assertEqual(element.pformat(),
                         '<Element attr="1">\n    text\n    more\n')

    def test_index(self):
        # Element.index() behaves like list.index() on the element's children
        e = nodes.Element()
        e += nodes.Element()
        e += nodes.Text('sample')
        e += nodes.Element()
        e += nodes.Text('other sample')
        e += nodes.Text('sample')
        # return element's index for the first four children:
        for i in range(4):
            self.assertEqual(e.index(e[i]), i)
        # Caution: mismatches are possible for Text nodes
        # as they are compared by value (like `str` instances)
        self.assertEqual(e.index(e[4]), 1)
        self.assertEqual(e.index(e[4], start=2), 4)

    def test_previous_sibling(self):
        e = nodes.Element()
        c1 = nodes.Element()
        c2 = nodes.Element()
        e += [c1, c2]
        # print(c1 == c2)
        self.assertEqual(e.previous_sibling(), None)
        self.assertEqual(c1.previous_sibling(), None)
        self.assertEqual(c2.previous_sibling(), c1)

    def test_clear(self):
        element = nodes.Element()
        element += nodes.Element()
        self.assertTrue(len(element))
        element.clear()
        self.assertTrue(not len(element))

    def test_normal_attributes(self):
        element = nodes.Element()
        self.assertTrue('foo' not in element)
        self.assertRaises(KeyError, element.__getitem__, 'foo')
        element['foo'] = 'sometext'
        self.assertEqual(element['foo'], 'sometext')
        del element['foo']
        self.assertRaises(KeyError, element.__getitem__, 'foo')

    def test_default_attributes(self):
        element = nodes.Element()
        self.assertEqual(element['ids'], [])
        self.assertEqual(element.non_default_attributes(), {})
        self.assertTrue(not element.is_not_default('ids'))
        self.assertTrue(element['ids'] is not nodes.Element()['ids'])
        element['ids'].append('someid')
        self.assertEqual(element['ids'], ['someid'])
        self.assertEqual(element.non_default_attributes(),
                         {'ids': ['someid']})
        self.assertTrue(element.is_not_default('ids'))

    def test_update_basic_atts(self):
        element1 = nodes.Element(ids=['foo', 'bar'], test=['test1'])
        element2 = nodes.Element(ids=['baz', 'qux'], test=['test2'])
        element1.update_basic_atts(element2)
        # 'ids' are appended because 'ids' is a basic attribute.
        self.assertEqual(element1['ids'], ['foo', 'bar', 'baz', 'qux'])
        # 'test' is not overwritten because it is not a basic attribute.
        self.assertEqual(element1['test'], ['test1'])

    def test_update_all_atts(self):
        # Note: Also tests is_not_list_attribute and is_not_known_attribute
        # and various helpers
        # Test for full attribute replacement
        element1 = nodes.Element(ids=['foo', 'bar'], parent_only='parent',
                                 all_nodes='mom')
        element2 = nodes.Element(ids=['baz', 'qux'], child_only='child',
                                 all_nodes='dad', source='source')

        # Test for when same fields are replaced as well as source...
        element1.update_all_atts_consistantly(element2, True, True)
        # 'ids' are appended because 'ids' is a basic attribute.
        self.assertEqual(element1['ids'], ['foo', 'bar', 'baz', 'qux'])
        # 'parent_only' should remain unaffected.
        self.assertEqual(element1['parent_only'], 'parent')
        # 'all_nodes' is overwritten due to the second parameter == True.
        self.assertEqual(element1['all_nodes'], 'dad')
        # 'child_only' should have been added.
        self.assertEqual(element1['child_only'], 'child')
        # 'source' is also overwritten due to the third parameter == True.
        self.assertEqual(element1['source'], 'source')

        # Test for when same fields are replaced but not source...
        element1 = nodes.Element(ids=['foo', 'bar'], parent_only='parent',
                                 all_nodes='mom')
        element1.update_all_atts_consistantly(element2)
        # 'ids' are appended because 'ids' is a basic attribute.
        self.assertEqual(element1['ids'], ['foo', 'bar', 'baz', 'qux'])
        # 'parent_only' should remain unaffected.
        self.assertEqual(element1['parent_only'], 'parent')
        # 'all_nodes' is overwritten due to the second parameter default True.
        self.assertEqual(element1['all_nodes'], 'dad')
        # 'child_only' should have been added.
        self.assertEqual(element1['child_only'], 'child')
        # 'source' remains unset due to the third parameter default of False.
        self.assertEqual(element1.get('source'), None)

        # Test for when fields are NOT replaced but source is...
        element1 = nodes.Element(ids=['foo', 'bar'], parent_only='parent',
                                 all_nodes='mom')
        element1.update_all_atts_consistantly(element2, False, True)
        # 'ids' are appended because 'ids' is a basic attribute.
        self.assertEqual(element1['ids'], ['foo', 'bar', 'baz', 'qux'])
        # 'parent_only' should remain unaffected.
        self.assertEqual(element1['parent_only'], 'parent')
        # 'all_nodes' is preserved due to the second parameter == False.
        self.assertEqual(element1['all_nodes'], 'mom')
        # 'child_only' should have been added.
        self.assertEqual(element1['child_only'], 'child')
        # 'source' is added due to the third parameter == True.
        self.assertEqual(element1['source'], 'source')
        element1 = nodes.Element(source='destination')
        element1.update_all_atts_consistantly(element2, False, True)
        # 'source' remains unchanged due to the second parameter == False.
        self.assertEqual(element1['source'], 'destination')

        # Test for when same fields are replaced but not source...
        element1 = nodes.Element(ids=['foo', 'bar'], parent_only='parent',
                                 all_nodes='mom')
        element1.update_all_atts_consistantly(element2, False)
        # 'ids' are appended because 'ids' is a basic attribute.
        self.assertEqual(element1['ids'], ['foo', 'bar', 'baz', 'qux'])
        # 'parent_only' should remain unaffected.
        self.assertEqual(element1['parent_only'], 'parent')
        # 'all_nodes' is preserved due to the second parameter == False.
        self.assertEqual(element1['all_nodes'], 'mom')
        # 'child_only' should have been added.
        self.assertEqual(element1['child_only'], 'child')
        # 'source' remains unset due to the third parameter default of False.
        self.assertEqual(element1.get('source'), None)

        # Test for List attribute merging
        # Attribute Concatination
        element1 = nodes.Element(ss='a', sl='1', ls=['I'], ll=['A'])
        element2 = nodes.Element(ss='b', sl=['2'], ls='II', ll=['B'])
        element1.update_all_atts_concatenating(element2)
        # 'ss' is replaced because non-list
        self.assertEqual(element1['ss'], 'b')
        # 'sl' is replaced because they are both not lists
        self.assertEqual(element1['sl'], ['2'])
        # 'ls' is replaced because they are both not lists
        self.assertEqual(element1['ls'], 'II')
        # 'll' is extended because they are both lists
        self.assertEqual(element1['ll'], ['A', 'B'])

        # Attribute Coercion
        element1 = nodes.Element(ss='a', sl='1', ls=['I'], ll=['A'])
        element2 = nodes.Element(ss='b', sl=['2'], ls='II', ll=['B'])
        element1.update_all_atts_coercion(element2)
        # 'ss' is replaced because non-list
        self.assertEqual(element1['ss'], 'b')
        # 'sl' is converted to a list and appended because element2 has a list
        self.assertEqual(element1['sl'], ['1', '2'])
        # 'ls' has element2's value appended to the list
        self.assertEqual(element1['ls'], ['I', 'II'])
        # 'll' is extended because they are both lists
        self.assertEqual(element1['ll'], ['A', 'B'])

        # Attribute Conversion
        element1 = nodes.Element(ss='a', sl='1', ls=['I'], ll=['A'])
        element2 = nodes.Element(ss='b', sl=['2'], ls='II', ll=['B'])
        element1.update_all_atts_convert(element2)
        # 'ss' is converted to a list with the values from each element
        self.assertEqual(element1['ss'], ['a', 'b'])
        # 'sl' is converted to a list and appended
        self.assertEqual(element1['sl'], ['1', '2'])
        # 'ls' has element2's value appended to the list
        self.assertEqual(element1['ls'], ['I', 'II'])
        # 'll' is extended
        self.assertEqual(element1['ll'], ['A', 'B'])

    def test_copy(self):
        # Shallow copy:
        grandchild = nodes.Text('grandchild text')
        child = nodes.emphasis('childtext', grandchild, att='child')
        e = nodes.Element('raw text', child, att='e')
        e_copy = e.copy()
        self.assertTrue(e is not e_copy)
        # Internal attributes (like `rawsource`) are also copied.
        self.assertEqual(e.rawsource, 'raw text')
        self.assertEqual(e_copy.rawsource, e.rawsource)
        self.assertEqual(e_copy['att'], 'e')
        self.assertEqual(e_copy.document, e.document)
        self.assertEqual(e_copy.source, e.source)
        self.assertEqual(e_copy.line, e.line)
        # Children are not copied.
        self.assertEqual(len(e_copy), 0)

    def test_deepcopy(self):
        # Deep copy:
        grandchild = nodes.Text('grandchild text')
        child = nodes.emphasis('childtext', grandchild, att='child')
        e = nodes.Element('raw text', child, att='e')
        e_deepcopy = e.deepcopy()
        self.assertEqual(e_deepcopy.rawsource, e.rawsource)
        self.assertEqual(e_deepcopy['att'], 'e')
        # Children are copied recursively.
        self.assertEqual(e_deepcopy[0][0], grandchild)
        self.assertTrue(e_deepcopy[0][0] is not grandchild)
        self.assertEqual(e_deepcopy[0]['att'], 'child')

    def test_system_message_copy(self):
        e = nodes.system_message('mytext', att='e', rawsource='raw text')
        # Shallow copy:
        e_copy = e.copy()
        self.assertTrue(e is not e_copy)
        # Internal attributes (like `rawsource`) are also copied.
        self.assertEqual(e.rawsource, 'raw text')
        self.assertEqual(e_copy.rawsource, e.rawsource)
        self.assertEqual(e_copy['att'], 'e')

    def test_replace_self(self):
        parent = nodes.Element(ids=['parent'])
        child1 = nodes.Element(ids=['child1'])
        grandchild = nodes.Element(ids=['grandchild'])
        child1 += grandchild
        child2 = nodes.Element(ids=['child2'])
        twins = [nodes.Element(ids=['twin%s' % i]) for i in (1, 2)]
        child2 += twins
        child3 = nodes.Element(ids=['child3'])
        child4 = nodes.Element(ids=['child4'])
        parent += [child1, child2, child3, child4]
        self.assertEqual(parent.pformat(), """\
<Element ids="parent">
    <Element ids="child1">
        <Element ids="grandchild">
    <Element ids="child2">
        <Element ids="twin1">
        <Element ids="twin2">
    <Element ids="child3">
    <Element ids="child4">
""")
        # Replace child1 with the grandchild.
        child1.replace_self(child1[0])
        self.assertEqual(parent[0], grandchild)
        # Assert that 'ids' have been updated.
        self.assertEqual(grandchild['ids'], ['grandchild', 'child1'])
        # Replace child2 with its children.
        child2.replace_self(child2[:])
        self.assertEqual(parent[1:3], twins)
        # Assert that 'ids' have been propagated to first child.
        self.assertEqual(twins[0]['ids'], ['twin1', 'child2'])
        self.assertEqual(twins[1]['ids'], ['twin2'])
        # Replace child3 with new child.
        newchild = nodes.Element(ids=['newchild'])
        child3.replace_self(newchild)
        self.assertEqual(parent[3], newchild)
        self.assertEqual(newchild['ids'], ['newchild', 'child3'])
        # Crazy but possible case: Substitute child4 for itself.
        child4.replace_self(child4)
        # Make sure the 'child4' ID hasn't been duplicated.
        self.assertEqual(child4['ids'], ['child4'])
        self.assertEqual(len(parent), 5)

    def test_set_class_deprecation_warning(self):
        node = nodes.Element('test node')
        with self.assertWarns(DeprecationWarning):
            node.set_class('parrot')

    def test_validate(self):
        node = nodes.paragraph('', 'plain text', classes='my test classes')
        node.append(nodes.emphasis('', 'emphasised text', ids='emphtext'))
        node.validate()

    def test_validate_attributes(self):
        # Convert to expected data-type, normalize values,
        # cf. AttributeTypeTests below for attribute validating function tests.
        node = nodes.image(classes='my  test-classes',
                           names='My teST\n\\ \xA0classes',
                           width='30 mm')
        node.validate_attributes()
        self.assertEqual(node['classes'], ['my', 'test-classes'])
        self.assertEqual(node['names'], ['My', 'teST classes'])
        self.assertEqual(node['width'], '30mm')

    def test_validate_wrong_attribute(self):
        node = nodes.paragraph('', 'text', id='test-paragraph')
        with self.assertRaisesRegex(ValueError,
                                    'Element <paragraph> invalid:\n'
                                    'Attribute "id" not one of "ids '):
            node.validate()

    def test_validate_wrong_attribute_value(self):
        node = nodes.image(uri='test.png', width='20 inch')  # invalid unit
        with self.assertRaisesRegex(ValueError,
                                    'Element <image> invalid:\n'
                                    '.*"width" has invalid value "20 inch".\n'
                                    '.*Valid units: em ex '):
            node.validate()


class MiscTests(unittest.TestCase):

    def test_node_class_names(self):
        node_class_names = []
        for x in dir(nodes):
            c = getattr(nodes, x)
            if (isinstance(c, type)
                and issubclass(c, nodes.Node)
                and len(c.__bases__) > 1):
                node_class_names.append(x)
        node_class_names.sort()
        nodes.node_class_names.sort()
        self.assertEqual(node_class_names, nodes.node_class_names)


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
        self.assertEqual(one.__class__, two.__class__)
        self.assertNotEqual(id(one), id(two))
        self.assertEqual(len(one.children), len(two.children))
        for i in range(len(one.children)):
            self.compare_trees(one.children[i], two.children[i])

    def test_copy_whole(self):
        visitor = nodes.TreeCopyVisitor(self.document)
        self.document.walkabout(visitor)
        newtree = visitor.get_tree_copy()
        self.assertEqual(self.document.pformat(), newtree.pformat())
        self.compare_trees(self.document, newtree)


class SetIdTests(unittest.TestCase):

    def setUp(self):
        self.document = utils.new_document('test')
        self.elements = [nodes.Element(names=['test']),
                         nodes.section(),  # Name empty
                         nodes.section(names=['Test']),  # duplicate id
                         nodes.footnote(names=['2019-10-30']),  # id empty
                         ]

    def test_set_id_default(self):
        # Default prefixes.
        for element in self.elements:
            self.document.set_id(element)
        ids = [element['ids'] for element in self.elements]
        self.assertEqual(ids, [['test'], ['section-1'],
                               ['test-1'], ['footnote-1']])

    def test_set_id_custom(self):
        # Custom prefixes.

        # Change settings.
        self.document.settings.id_prefix = 'P-'
        self.document.settings.auto_id_prefix = 'auto'

        for element in self.elements:
            self.document.set_id(element)
        ids = [element['ids'] for element in self.elements]
        self.assertEqual(ids, [['P-test'],
                               ['P-auto1'],
                               ['P-auto2'],
                               ['P-2019-10-30']])

    def test_set_id_descriptive_auto_id(self):
        # Use name or tag-name for auto-id.

        # Change setting.
        self.document.settings.auto_id_prefix = '%'

        for element in self.elements:
            self.document.set_id(element)
        ids = [element['ids'] for element in self.elements]
        self.assertEqual(ids, [['test'],
                               ['section-1'],
                               ['test-1'],
                               ['footnote-1']])

    def test_set_id_custom_descriptive_auto_id(self):
        # Custom prefixes and name or tag-name for auto-id.

        # Change settings.
        self.document.settings.id_prefix = 'P:'
        self.document.settings.auto_id_prefix = 'a-%'

        for element in self.elements:
            self.document.set_id(element)
        ids = [element['ids'] for element in self.elements]
        self.assertEqual(ids, [['P:test'],
                               ['P:a-section-1'],
                               ['P:test-1'],
                               ['P:2019-10-30']])


class NodeVisitorTests(unittest.TestCase):
    def setUp(self):
        self.document = utils.new_document('test')
        self.element = nodes.Element()
        self.visitor = nodes.NodeVisitor(self.document)

    def test_dispatch_visit_unknown(self):
        # raise exception if no visit/depart methods are defined for node class
        with self.assertRaises(NotImplementedError):
            self.visitor.dispatch_visit(self.element)

    def test_dispatch_visit_optional(self):
        # silently skip nodes of a calss in tuple nodes.NodeVisitor.optional
        rv = self.visitor.dispatch_visit(nodes.meta())
        self.assertIsNone(rv)


class MiscFunctionTests(unittest.TestCase):

    ids = [('a', 'a'), ('A', 'a'), ('', ''), ('a b \n c', 'a-b-c'),
           ('a.b.c', 'a-b-c'), (' - a - b - c - ', 'a-b-c'), (' - ', ''),
           ('\u2020\u2066', ''), ('a \xa7 b \u2020 c', 'a-b-c'),
           ('1', ''), ('1abc', 'abc'),
           ]

    ids_unicode_all = [
            ('\u00f8 o with stroke', 'o-o-with-stroke'),
            ('\u0111 d with stroke', 'd-d-with-stroke'),
            ('\u0127 h with stroke', 'h-h-with-stroke'),
            ('\u0131 dotless i', 'i-dotless-i'),
            ('\u0142 l with stroke', 'l-l-with-stroke'),
            ('\u0167 t with stroke', 't-t-with-stroke'),
            # From Latin Extended-B
            ('\u0180 b with stroke', 'b-b-with-stroke'),
            ('\u0183 b with topbar', 'b-b-with-topbar'),
            ('\u0188 c with hook', 'c-c-with-hook'),
            ('\u018c d with topbar', 'd-d-with-topbar'),
            ('\u0192 f with hook', 'f-f-with-hook'),
            ('\u0199 k with hook', 'k-k-with-hook'),
            ('\u019a l with bar', 'l-l-with-bar'),
            ('\u019e n with long right leg', 'n-n-with-long-right-leg'),
            ('\u01a5 p with hook', 'p-p-with-hook'),
            ('\u01ab t with palatal hook', 't-t-with-palatal-hook'),
            ('\u01ad t with hook', 't-t-with-hook'),
            ('\u01b4 y with hook', 'y-y-with-hook'),
            ('\u01b6 z with stroke', 'z-z-with-stroke'),
            ('\u01e5 g with stroke', 'g-g-with-stroke'),
            ('\u0225 z with hook', 'z-z-with-hook'),
            ('\u0234 l with curl', 'l-l-with-curl'),
            ('\u0235 n with curl', 'n-n-with-curl'),
            ('\u0236 t with curl', 't-t-with-curl'),
            ('\u0237 dotless j', 'j-dotless-j'),
            ('\u023c c with stroke', 'c-c-with-stroke'),
            ('\u023f s with swash tail', 's-s-with-swash-tail'),
            ('\u0240 z with swash tail', 'z-z-with-swash-tail'),
            ('\u0247 e with stroke', 'e-e-with-stroke'),
            ('\u0249 j with stroke', 'j-j-with-stroke'),
            ('\u024b q with hook tail', 'q-q-with-hook-tail'),
            ('\u024d r with stroke', 'r-r-with-stroke'),
            ('\u024f y with stroke', 'y-y-with-stroke'),
            # From Latin-1 Supplements
            ('\u00e0: a with grave', 'a-a-with-grave'),
            ('\u00e1 a with acute', 'a-a-with-acute'),
            ('\u00e2 a with circumflex', 'a-a-with-circumflex'),
            ('\u00e3 a with tilde', 'a-a-with-tilde'),
            ('\u00e4 a with diaeresis', 'a-a-with-diaeresis'),
            ('\u00e5 a with ring above', 'a-a-with-ring-above'),
            ('\u00e7 c with cedilla', 'c-c-with-cedilla'),
            ('\u00e8 e with grave', 'e-e-with-grave'),
            ('\u00e9 e with acute', 'e-e-with-acute'),
            ('\u00ea e with circumflex', 'e-e-with-circumflex'),
            ('\u00eb e with diaeresis', 'e-e-with-diaeresis'),
            ('\u00ec i with grave', 'i-i-with-grave'),
            ('\u00ed i with acute', 'i-i-with-acute'),
            ('\u00ee i with circumflex', 'i-i-with-circumflex'),
            ('\u00ef i with diaeresis', 'i-i-with-diaeresis'),
            ('\u00f1 n with tilde', 'n-n-with-tilde'),
            ('\u00f2 o with grave', 'o-o-with-grave'),
            ('\u00f3 o with acute', 'o-o-with-acute'),
            ('\u00f4 o with circumflex', 'o-o-with-circumflex'),
            ('\u00f5 o with tilde', 'o-o-with-tilde'),
            ('\u00f6 o with diaeresis', 'o-o-with-diaeresis'),
            ('\u00f9 u with grave', 'u-u-with-grave'),
            ('\u00fa u with acute', 'u-u-with-acute'),
            ('\u00fb u with circumflex', 'u-u-with-circumflex'),
            ('\u00fc u with diaeresis', 'u-u-with-diaeresis'),
            ('\u00fd y with acute', 'y-y-with-acute'),
            ('\u00ff y with diaeresis', 'y-y-with-diaeresis'),
            # From Latin Extended-A
            ('\u0101 a with macron', 'a-a-with-macron'),
            ('\u0103 a with breve', 'a-a-with-breve'),
            ('\u0105 a with ogonek', 'a-a-with-ogonek'),
            ('\u0107 c with acute', 'c-c-with-acute'),
            ('\u0109 c with circumflex', 'c-c-with-circumflex'),
            ('\u010b c with dot above', 'c-c-with-dot-above'),
            ('\u010d c with caron', 'c-c-with-caron'),
            ('\u010f d with caron', 'd-d-with-caron'),
            ('\u0113 e with macron', 'e-e-with-macron'),
            ('\u0115 e with breve', 'e-e-with-breve'),
            ('\u0117 e with dot above', 'e-e-with-dot-above'),
            ('\u0119 e with ogonek', 'e-e-with-ogonek'),
            ('\u011b e with caron', 'e-e-with-caron'),
            ('\u011d g with circumflex', 'g-g-with-circumflex'),
            ('\u011f g with breve', 'g-g-with-breve'),
            ('\u0121 g with dot above', 'g-g-with-dot-above'),
            ('\u0123 g with cedilla', 'g-g-with-cedilla'),
            ('\u0125 h with circumflex', 'h-h-with-circumflex'),
            ('\u0129 i with tilde', 'i-i-with-tilde'),
            ('\u012b i with macron', 'i-i-with-macron'),
            ('\u012d i with breve', 'i-i-with-breve'),
            ('\u012f i with ogonek', 'i-i-with-ogonek'),
            ('\u0133 ligature ij', 'ij-ligature-ij'),
            ('\u0135 j with circumflex', 'j-j-with-circumflex'),
            ('\u0137 k with cedilla', 'k-k-with-cedilla'),
            ('\u013a l with acute', 'l-l-with-acute'),
            ('\u013c l with cedilla', 'l-l-with-cedilla'),
            ('\u013e l with caron', 'l-l-with-caron'),
            ('\u0140 l with middle dot', 'l-l-with-middle-dot'),
            ('\u0144 n with acute', 'n-n-with-acute'),
            ('\u0146 n with cedilla', 'n-n-with-cedilla'),
            ('\u0148 n with caron', 'n-n-with-caron'),
            ('\u014d o with macron', 'o-o-with-macron'),
            ('\u014f o with breve', 'o-o-with-breve'),
            ('\u0151 o with double acute', 'o-o-with-double-acute'),
            ('\u0155 r with acute', 'r-r-with-acute'),
            ('\u0157 r with cedilla', 'r-r-with-cedilla'),
            ('\u0159 r with caron', 'r-r-with-caron'),
            ('\u015b s with acute', 's-s-with-acute'),
            ('\u015d s with circumflex', 's-s-with-circumflex'),
            ('\u015f s with cedilla', 's-s-with-cedilla'),
            ('\u0161 s with caron', 's-s-with-caron'),
            ('\u0163 t with cedilla', 't-t-with-cedilla'),
            ('\u0165 t with caron', 't-t-with-caron'),
            ('\u0169 u with tilde', 'u-u-with-tilde'),
            ('\u016b u with macron', 'u-u-with-macron'),
            ('\u016d u with breve', 'u-u-with-breve'),
            ('\u016f u with ring above', 'u-u-with-ring-above'),
            ('\u0171 u with double acute', 'u-u-with-double-acute'),
            ('\u0173 u with ogonek', 'u-u-with-ogonek'),
            ('\u0175 w with circumflex', 'w-w-with-circumflex'),
            ('\u0177 y with circumflex', 'y-y-with-circumflex'),
            ('\u017a z with acute', 'z-z-with-acute'),
            ('\u017c z with dot above', 'z-z-with-dot-above'),
            ('\u017e z with caron', 'z-z-with-caron'),
            # From Latin Extended-B
            ('\u01a1 o with horn', 'o-o-with-horn'),
            ('\u01b0 u with horn', 'u-u-with-horn'),
            ('\u01c6 dz with caron', 'dz-dz-with-caron'),
            ('\u01c9 lj', 'lj-lj'),
            ('\u01cc nj', 'nj-nj'),
            ('\u01ce a with caron', 'a-a-with-caron'),
            ('\u01d0 i with caron', 'i-i-with-caron'),
            ('\u01d2 o with caron', 'o-o-with-caron'),
            ('\u01d4 u with caron', 'u-u-with-caron'),
            ('\u01e7 g with caron', 'g-g-with-caron'),
            ('\u01e9 k with caron', 'k-k-with-caron'),
            ('\u01eb o with ogonek', 'o-o-with-ogonek'),
            ('\u01ed o with ogonek and macron', 'o-o-with-ogonek-and-macron'),
            ('\u01f0 j with caron', 'j-j-with-caron'),
            ('\u01f3 dz', 'dz-dz'),
            ('\u01f5 g with acute', 'g-g-with-acute'),
            ('\u01f9 n with grave', 'n-n-with-grave'),
            ('\u0201 a with double grave', 'a-a-with-double-grave'),
            ('\u0203 a with inverted breve', 'a-a-with-inverted-breve'),
            ('\u0205 e with double grave', 'e-e-with-double-grave'),
            ('\u0207 e with inverted breve', 'e-e-with-inverted-breve'),
            ('\u0209 i with double grave', 'i-i-with-double-grave'),
            ('\u020b i with inverted breve', 'i-i-with-inverted-breve'),
            ('\u020d o with double grave', 'o-o-with-double-grave'),
            ('\u020f o with inverted breve', 'o-o-with-inverted-breve'),
            ('\u0211 r with double grave', 'r-r-with-double-grave'),
            ('\u0213 r with inverted breve', 'r-r-with-inverted-breve'),
            ('\u0215 u with double grave', 'u-u-with-double-grave'),
            ('\u0217 u with inverted breve', 'u-u-with-inverted-breve'),
            ('\u0219 s with comma below', 's-s-with-comma-below'),
            ('\u021b t with comma below', 't-t-with-comma-below'),
            ('\u021f h with caron', 'h-h-with-caron'),
            ('\u0227 a with dot above', 'a-a-with-dot-above'),
            ('\u0229 e with cedilla', 'e-e-with-cedilla'),
            ('\u022f o with dot above', 'o-o-with-dot-above'),
            ('\u0233 y with macron', 'y-y-with-macron'),
            # digraphs From Latin-1 Supplements
            ('\u00df: ligature sz', 'sz-ligature-sz'),
            ('\u00e6 ae', 'ae-ae'),
            ('\u0153 ligature oe', 'oe-ligature-oe'),
            ('\u0238 db digraph', 'db-db-digraph'),
            ('\u0239 qp digraph', 'qp-qp-digraph'),
            ]

    def test_make_id(self):
        failures = []
        tests = self.ids + self.ids_unicode_all
        for input, expect in tests:
            output = nodes.make_id(input)
            if expect != output:
                failures.append("'%s' != '%s'" % (expect, output))
        if failures:
            self.fail(f'{len(failures)} failures in {len(self.ids)} ids\n'
                      + "\n".join(failures))

    names = [  # sample, whitespace_normalized, fully_normalized
             ('a', 'a', 'a'),
             ('A', 'A', 'a'),
             ('A a A ', 'A a A', 'a a a'),
             ('A  a  A  a', 'A a A a', 'a a a a'),
             ('  AaA\n\r\naAa\tAaA\t\t', 'AaA aAa AaA', 'aaa aaa aaa')
             ]

    def test_whitespace_normalize_name(self):
        for (sample, ws, full) in self.names:
            result = nodes.whitespace_normalize_name(sample)
            self.assertEqual(result, ws)

    def test_fully_normalize_name(self):
        for (sample, ws, fully) in self.names:
            result = nodes.fully_normalize_name(sample)
            self.assertEqual(result, fully)

    def test_split_name_list(self):
        self.assertEqual(nodes.split_name_list(r'a\ n\ame two\\ n\\ames'),
                         ['a name', 'two\\', r'n\ames'])


class AttributeTypeTests(unittest.TestCase):

    def test_validate_enumerated_type(self):
        # function factory for "choice validators"
        food = nodes.validate_enumerated_type('ham', 'spam')
        self.assertEqual(food('ham'), 'ham')
        with self.assertRaisesRegex(ValueError,
                                    '"bacon" is not one of "ham", "spam".'):
            food('bacon')

    def test_validate_identifier(self):
        # Identifiers must start with an ASCII letter and may contain
        # letters, digits and the hyphen
        # https://docutils.sourceforge.io/docs/ref/doctree.html#idref-type
        self.assertEqual(nodes.validate_identifier('mo-8b'), 'mo-8b')
        with self.assertRaisesRegex(ValueError, '"8b-mo" is no valid id'):
            nodes.validate_identifier('8b-mo')

    def test_validate_identifier_list(self):
        # list of identifiers (cf. above)
        # or a `str` of space-separated identifiers.
        l1 = ['m8-b', 'm8-c']
        s1 = 'm8-b m8-c'
        self.assertEqual(nodes.validate_identifier_list(l1), l1)
        self.assertEqual(nodes.validate_identifier_list(s1), l1)
        l2 = ['m8-b', 'm8_c']
        s2 = 'm8-b #8c'
        with self.assertRaises(ValueError):
            nodes.validate_identifier_list(l2)
        with self.assertRaises(ValueError):
            nodes.validate_identifier_list(s2)

    def test_validate_measure(self):
        # number (may be decimal fraction) + optional CSS2 length unit
        self.assertEqual(nodes.validate_measure('8ex'), '8ex')
        self.assertEqual(nodes.validate_measure('3.5 %'), '3.5%')
        self.assertEqual(nodes.validate_measure('2'), '2')
        with self.assertRaisesRegex(ValueError, '"2km" is no valid measure. '
                                    'Valid units: em ex '):
            nodes.validate_measure('2km')
        # negative numbers are currently not supported
        # TODO: allow? the spec doesnot mention negative numbers.
        # but a negative width or height of an image is odd.
        # nodes.validate_measure('-2')

    def test_validate_NMTOKEN(self):
        # str with ASCII-letters, digits, hyphen, underscore, and full-stop.
        self.assertEqual(nodes.validate_NMTOKEN('-8x_.'), '-8x_.')
        with self.assertRaises(ValueError):
            nodes.validate_NMTOKEN('why me')

    def test_validate_NMTOKENS(self):
        # list of NMTOKENS or string with space-separated NMTOKENS
        l1 = ['8_b', '8.c']
        s1 = '8_b 8.c'
        l2 = ['8_b', '8/c']
        s2 = '8_b #8'
        self.assertEqual(nodes.validate_NMTOKENS(l1), l1)
        self.assertEqual(nodes.validate_NMTOKENS(s1), l1)
        with self.assertRaises(ValueError):
            nodes.validate_NMTOKENS(l2)
        with self.assertRaises(ValueError):
            nodes.validate_NMTOKENS(s2)

    def test_validate_refname_list(self):
        # list or string of "reference names".
        l1 = ['*:@', r'"more"\ & \x!']
        s1 = r'*:@ \"more"\\\ &\ \\x!'  # unescaped backslash is ignored
        self.assertEqual(nodes.validate_refname_list(l1), l1)
        self.assertEqual(nodes.validate_refname_list(s1), l1)
        # whitspace is normalized, case is not normalized
        l2 = ['LARGE', 'a\t \tc']
        s2 = r'LARGE a\ \ \c'
        normalized = ['LARGE', 'a c']

        self.assertEqual(nodes.validate_refname_list(l2), normalized)
        self.assertEqual(nodes.validate_refname_list(s2), normalized)

    def test_validate_yesorno(self):
        # False if '0', else bool
        # TODO: The docs say '0' is false:
        # * Also return `True` for values that evaluate to `False`?
        #   Even for `False` and `None`?
        # * Also return `False` for 'false', 'off', 'no'
        #   like boolean config settings?
        self.assertFalse(nodes.validate_yesorno('0'))
        self.assertFalse(nodes.validate_yesorno(0))
        self.assertTrue(nodes.validate_yesorno('*'))
        self.assertTrue(nodes.validate_yesorno(1))
        # self.assertFalse(nodes.validate_yesorno('no'))


if __name__ == '__main__':
    unittest.main()
