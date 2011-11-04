import re, sys, copy
from copy import deepcopy

from xml.etree.ElementTree import Element, tostring
import xml.etree.ElementTree as etree

class AsciiMathML:

    greek_dict = {
    'alpha': u"\u03B1",
        }
    operator_dict = {
        'sum':u"\u2211",
        u"\u2211":u"\u2211", 
        'Sigma':u"\u2211", 
            }
    symbol_dict = {}
    symbol_dict.update(greek_dict)
    special_dict = {
            '(':{'type':'special'},
            ')':{'type':'special'},
            '/':{'type':'special'},
            '^':{'type':'special'},
            '_':{'type':'special'},

            }
    symbol_names = sorted(symbol_dict.keys(), key=lambda key_string: len(key_string), reverse=True)
    special_names = sorted(special_dict.keys(), key=lambda key_string: len(key_string), reverse=True)
    operator_names = sorted(operator_dict.keys(), key=lambda key_string: len(key_string), reverse=True)
    # special_names = [ '(', ')', '/', '^', '_']

    def __init__(self, output_encoding = 'utf8'):
        self.__number_re = re.compile('-?(\d+\.(\d+)?|\.?\d+)')
        self.__tree = Element('math')
        mstyle = etree.SubElement(self.__tree, 'mstyle')
        self.__mathml_ns = 'http://www.w3.org/1998/Math/MathML'
        self.__append_el = mstyle
        self.__output_encoding = output_encoding


    def __make_element(self, tag, text=None, *children, **attrib):
        element = Element(tag, **attrib)

        if not text is None:
            if isinstance(text, basestring):
                element.text = text
            else:
                children = (text, ) + children

        for child in children:
            element.append(child)

        return element

    def __change_el_name(self, element, new_name):
        element.tag = new_name


    def __get_previous_sibling(self,  element , the_tree = 0):
        """
        either the previous sibling passed to the function, or if none is passed, 
        the previous sibling of the last element written

        """
        if the_tree == 0:
            the_tree = self.__tree
        parent = self.__get_parent(child = element, the_tree = the_tree)
        if parent == None:
            return
        counter = -1
        for child in parent:
            counter += 1
            if child == element:
                if counter - 1 < 0:
                    return None
                return parent[counter - 1]

    def __get_last_element(self):
        if len(self.__append_el) > 0:
            return self.__append_el[-1]
        return self.__append_el

    def __get_following_sibling(self, element, the_tree = 0):
        if the_tree == 0:
            the_tree = self.__tree
        parent = self.__get_parent(the_tree = the_tree, child = element)
        if parent == None:
            return
        counter = -1
        for child in parent:
            counter += 1
            if child == element:
                if len(parent) == counter + 1:
                    return None
                return parent[counter + 1]

    def __get_parent(self, child, the_tree = 0):
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

    def __get_grandparent(self, child, the_tree = 0):
        parent = self.__get_parent(child = child, the_tree = the_tree)
        grandparent = self.__get_parent(child = parent, the_tree = the_tree)

    def __is_parenthesis(self, element):
        if element != None and element.tag == 'mfenced' and element.get('open') == '(' and\
                    element.get('close') == ')':
            return True


    def __change_element(self, element, name, **attributes):
        element.tag = name
        the_keys = element.attrib.keys()
        for the_key in the_keys:
            del(element.attrib[the_key])
        for att in attributes:
            element.set(att, attributes[att])


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
        element = self.__make_element('mn', text=token)
        self.__append_el.append(element)

    def __add_neg_num_to_tree(self, token, the_type):
        num = token[1:]
        element = self.__make_element('mo', text='-')
        self.__append_el.append(element)
        element = self.__make_element('mn', text=num)
        self.__append_el.append(element)

    def __add_alpha_to_tree(self, token, the_type):
        element = self.__make_element('mi', text=token)
        self.__append_el.append(element)

    def __add_symbol_to_tree(self, token, token_dict):
        token = token_dict['symbol']
        element = self.__make_element('mi', text=token)
        self.__append_el.append(element)

    def __add_operator_to_tree(self, token, token_info):
        if isinstance(token_info, dict):
            text = token_info.get('symbol')
        else:
            text = token
        element = self.__make_element('mo', text=text)
        self.__append_el.append(element)

    def __handle_binary(self, token, info):
        last_element = self.__get_last_element() 
        if last_element == self.__append_el: # no "previous sibling," and can't process
            self.__add_operator_to_tree(token, info)
            return
        if token == '/':
            num_frac = 0
            if last_element.tag == 'mfrac':
                for child in last_element:
                    if child.tag == 'mfrac':
                        num_frac +=1
            if num_frac % 2 != 0:
                self.__append_el = last_element
                last_element = self.__get_last_element()
            if self.__is_parenthesis(last_element):
                self.__change_element(last_element, 'mrow', **{'class':'nominator'})
            nominator = deepcopy(last_element)
            self.__append_el.remove(last_element)
            mfrac = self.__make_element('mfrac',  nominator)
            self.__append_el.append(mfrac)
            self.__append_el = mfrac
        elif token == '^' or token == '_':
            if last_element.tag == 'msub':
                subsup = self.__make_element('msubsup')
                for child in last_element: # should be just 2--check? 
                    element = deepcopy(child)
                    subsup.append(element)
                self.__append_el.remove(last_element)
                self.__append_el.append(subsup)
                self.__append_el = subsup

            else:
                if token == '^':
                    el_name = 'msup'
                elif token == '_':
                    el_name = 'msub'
                base = deepcopy(last_element)
                self.__append_el.remove(last_element)
                base = self.__make_element(el_name,  base)
                self.__append_el.append(base)
                self.__append_el = base 

    def __handle_open_parenthesis(self):
        element = self.__make_element('mfenced', open='(', separators='', close="")
        self.__append_el.append(element)
        self.__append_el = element

    def __handle_close_parenthesis(self):
        if self.__append_el.tag == 'mfenced' and self.__append_el.get('open') == '(':
            self.__append_el.set('close', ')')
            parent = self.__get_parent(self.__append_el)
            self.__append_el = parent
        else:
            element = self.__make_element('mo', text=')')
            self.__append_el.append(element)

    def __add_special_to_tree(self, token, the_type):
        if token == '(':
            self.__handle_open_parenthesis()
        elif token == ')':
            self.__handle_close_parenthesis()
        elif token == '/' or token == '^' or token == '_':
            self.__handle_binary(token, the_type)



    def parse_string(self, the_string):
        while the_string != '':
            the_string, token, token_info = self.__parse_tokens(the_string)
            if isinstance(token_info, str):
                the_type = token_info
            else:
                the_type = token_info.get('type')
            if the_type == 'number':
                self.__add_num_to_tree(token, the_type)
            elif the_type == 'neg_number':
                self.__add_neg_num_to_tree(token, the_type)
            elif the_type == 'alpha':
                self.__add_alpha_to_tree(token, the_type)
            elif the_type == 'symbol':
                self.__add_symbol_to_tree(token, token_info)
            elif the_type == 'operator':
                self.__add_operator_to_tree(token, token_info)
            elif the_type == 'special':
                self.__add_special_to_tree(token, the_type)

            # for all elements
            if self.__append_el.tag == 'mfrac' or self.__append_el.tag == 'msup' or self.__append_el.tag == 'msub':
                last_element = self.__get_last_element()
                prev_sib = self.__get_previous_sibling(last_element)
                if prev_sib != None:
                    if self.__is_parenthesis(last_element):
                        if self.__append_el.tag == 'mfrac':
                            the_dict = {'class':'denominator'}
                        elif self.__append_el.tag == 'msup':
                            the_dict = {'class':'superscript'}
                        elif self.__append_el.tag == 'msub':
                            the_dict = {'class':'subcript'}
                        self.__change_element(last_element, 'mrow', **the_dict)
                    self.__append_el = self.__get_parent(self.__append_el)
            elif self.__append_el.tag =='msubsup':
                last_element = self.__get_last_element()
                prev_sib = self.__get_previous_sibling(last_element)
                prev_prev_sib =self.__get_previous_sibling(prev_sib) 
                if prev_prev_sib != None:
                    if self.__is_parenthesis(last_element):
                        the_dict = {'class':'subsuper'}
                        self.__change_element(last_element, 'mrow', **the_dict)
                    self.__append_el = self.__get_parent(self.__append_el)


    def __look_at_next_token(self, the_string):
        the_string, token, the_type = self.__parse_tokens(the_string)
        return the_string, token, the_type


    def __parse_tokens(self, the_string):

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
                info = self.special_dict[special] 
                return the_string[len(name):], special, info

        for name in self.operator_names: # found special operator
            if the_string.startswith(name):
                symbol = the_string[:len(name)]
                symbol = self.operator_dict[symbol] 
                return the_string[len(name):], name, {'type': 'operator', 'symbol': symbol} 

        for name in self.symbol_names: # found a special symbol
            if the_string.startswith(name):
                symbol = the_string[:len(name)]
                symbol = self.symbol_dict[symbol] 
                return the_string[len(name):], name, {'type': 'symbol', 'symbol': symbol} 

        # found either an operator or a letter

        if the_string[0].isalpha():
            return the_string[1:], the_string[0], 'alpha'
        else:
            return the_string[1:], the_string[0], 'operator'


def ascii_to_xml_string(the_string):
    math_obj =  AsciiMathML()
    math_obj.parse_string(the_string)
    xml_string = math_obj.to_xml_string()
    return xml_string

def ascii_to_math_tree(the_string):
    math_obj =  AsciiMathML()
    math_obj.parse_string(the_string)
    math_tree = math_obj.get_tree()
    return math_tree
