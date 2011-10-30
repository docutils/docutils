import re, sys, copy
from copy import deepcopy

from xml.etree.ElementTree import Element, tostring
import xml.etree.ElementTree as etree

class AsciiMathML:

    symbol_dict = {
    'alpha': u"\u03B1",
        }
    special_dict = {
            '(':'open_paren',
            ')':'close_paren',
            }
    symbol_names = sorted(symbol_dict.keys(), key=lambda key_string: len(key_string), reverse=True)
    special_names = ['(', ')']

    def __init__(self, output_encoding = 'utf8'):
        self.__number_re = re.compile('-?(\d+\.(\d+)?|\.?\d+)')
        self.__tree = Element('math')
        mstyle = etree.SubElement(self.__tree, 'mstyle')
        self.__mathml_ns = 'http://www.w3.org/1998/Math/MathML'
        self.__append_el = mstyle
        self.__output_encoding = output_encoding

    def make_element(self, tag, text=None, *children, **attrib):
        element = Element(tag, **attrib)

        if not text is None:
            if isinstance(text, basestring):
                element.text = text
            else:
                children = (text, ) + children

        for child in children:
            element.append(child)

        return element


    def get_previous_sibling(self,  element, the_tree = 0):
        if the_tree == 0:
            the_tree = self.__tree
        parent = self.get_parent(child = element, the_tree = the_tree)
        if parent == None:
            return
        counter = -1
        for child in parent:
            counter += 1
            if child == element:
                if counter - 1 < 0:
                    return None
                return parent[counter - 1]

    def get_following_sibling(self, element, the_tree = 0):
        if the_tree == 0:
            the_tree = self.__tree
        parent = self.get_parent(the_tree = the_tree, child = element)
        if parent == None:
            return
        counter = -1
        for child in parent:
            counter += 1
            if child == element:
                if len(parent) == counter + 1:
                    return None
                return parent[counter + 1]

    def get_parent(self, child, the_tree = 0):
        """

        the_tree: an xml.etree of the whole tree

        child: an xml.etree of the child element

        There is no direct way to get the parent of an element in etree. This
        method makes a child-parent dictionary of the whold tree, than accesses
        the dictionary

        """
        if the_tree == 0:
            the_tree = self.__tree
        child_parent_map = dict((c, p) for p in the_tree.getiterator() for c in p)
        parent = child_parent_map.get(child)
        return parent


    def __add_namespace(self):
        attributes = self.__tree.attrib
        value = attributes.get('xmlns')
        if not value:
            self.__tree.set('xmlns', self.__mathml_ns)

    def to_xml_string(self, encoding=None):
        if not encoding:
            encoding = self.__output_encoding
        self.__add_namespace()
        xml_string = tostring(self.__tree, encoding=encoding)
        return xml_string

    def get_tree(self):
        self.__add_namespace()
        return self.__tree

    def __add_num_to_tree(self, token, the_type):
        element = self.make_element('mn', text=token)
        self.__append_el.append(element)

    def __add_neg_num_to_tree(self, token, the_type):
        num = token[1:]
        element = self.make_element('mo', text='-')
        self.__append_el.append(element)
        element = self.make_element('mn', text=num)
        self.__append_el.append(element)

    def __add_symbol_alpha_to_tree(self, token, the_type):
        element = self.make_element('mi', text=token)
        self.__append_el.append(element)

    def __add_operator_to_tree(self, token, the_type):
        element = self.make_element('mo', text=token)
        self.__append_el.append(element)

    def __add_special_to_tree(self, token, the_type):
        if token == '(':
            element = self.make_element('mfenced', open='(')
            self.__append_el.append(element)
            self.__append_el = element
        if token == ')':
            if self.__append_el.tag == 'mfenced' and self.__append_el.get('open') == '(':
                self.__append_el.set('close', ')')

    def parse_string(self, the_string):
        
        while the_string != '':
            the_string, token, the_type = self.parse_tokens(the_string)
            if the_type == 'number':
                self.__add_num_to_tree(token, the_type)
            elif the_type == 'neg_number':
                self.__add_neg_num_to_tree(token, the_type)
            elif the_type == 'symbol' or the_type == 'alpha':
                self.__add_symbol_alpha_to_tree(token, the_type)
            elif the_type == 'operator':
                self.__add_operator_to_tree(token, the_type)
            elif the_type == 'special':
                self.__add_special_to_tree(token, the_type)


    def parse_tokens(self, the_string):

        """

        processes the string one token at a time. If a number is found, process
        and return the number with the rest of the stirng.

        Else, see if the string starts with a special symbol, and process and
        return that with the rest of the string.

        Else, get the next character, and process that with the rest of the string.

        """
        the_string = the_string.strip()

        if the_string == '':
            return None, None, None

        match = self.__number_re.match(the_string)

        if match: # found a number
            number = match.group(0)
            if number[0] == '-':
                return the_string[match.end():], number, 'neg_number'
            else:
                return the_string[match.end():], number, 'number'

        for name in self.special_names:
            if the_string.startswith(name):
                special = the_string[:len(name)]
                info = self.special_dict[special] # do nothing with this for now
                return the_string[len(name):], special, 'special'

        for name in self.symbol_names: # found a special symbol
            if the_string.startswith(name):
                symbol = the_string[:len(name)]
                return the_string[len(name):], self.symbol_dict.get(symbol), 'symbol'

        # found either an operator or a letter

        if the_string[0].isalpha():
            return the_string[1:], the_string[0], 'alpha'
        else:
            return the_string[1:], the_string[0], 'operator'


def ascii_to_xml_string(the_string):
    pass

def ascii_to_math_tree(the_string):
    math_obj =  AsciiMathML()
    math_obj.parse_string(the_string)
    math_tree = math_obj.get_tree()
    return math_tree
