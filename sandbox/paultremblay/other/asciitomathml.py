 # -*- coding: UTF-8 -*-
import re, sys, copy
from copy import deepcopy

from xml.etree.ElementTree import Element, tostring
import xml.etree.ElementTree as etree

class AsciiMathML:

    greek_dict = {
    'alpha': u"\u03B1",
    'Sigma': u"\u2211",
     'sum':u"\u2211",
        }
    operator_dict = {
        u"\u2211":u"\u2211", 
            }
    symbol_dict = {}
    symbol_dict.update(greek_dict)
    special_dict = {
            '(':{'type':'special'},
            ')':{'type':'special'},
            '/':{'type':'special'},
            '^':{'type':'special'},
            '_':{'type':'special'},
            '||':{'type':'special'},

            }
    symbol_names = sorted(symbol_dict.keys(), key=lambda key_string: len(key_string), reverse=True)
    special_names = sorted(special_dict.keys(), key=lambda key_string: len(key_string), reverse=True)
    operator_names = sorted(operator_dict.keys(), key=lambda key_string: len(key_string), reverse=True)
    # special_names = [ '(', ')', '/', '^', '_']

    def __init__(self, output_encoding = 'utf8'):
        self._number_re = re.compile('-?(\d+\.(\d+)?|\.?\d+)')
        self._tree = Element('math')
        mstyle = etree.SubElement(self._tree, 'mstyle')
        self._mathml_ns = 'http://www.w3.org/1998/Math/MathML'
        self._append_el = mstyle
        self._output_encoding = output_encoding
        self._fenced_for_right_parenthesis = True #  used fence for right parentheiss

    def _make_element(self, tag, text=None, *children, **attrib):
        element = Element(tag, **attrib)

        if not text is None:
            if isinstance(text, basestring):
                element.text = text
            else:
                children = (text, ) + children

        for child in children:
            element.append(child)

        return element

    def _change_el_name(self, element, new_name):
        element.tag = new_name


    def _get_previous_sibling(self,  element , the_tree = 0):
        """
        either the previous sibling passed to the function, or if none is passed, 
        the previous sibling of the last element written

        """
        if the_tree == 0:
            the_tree = self._tree
        parent = self._get_parent(child = element, the_tree = the_tree)
        if parent == None:
            return
        counter = -1
        for child in parent:
            counter += 1
            if child == element:
                if counter - 1 < 0:
                    return None
                return parent[counter - 1]

    def _get_last_element(self):
        if len(self._append_el) > 0:
            return self._append_el[-1]
        return self._append_el

    def _get_following_sibling(self, element, the_tree = 0):
        if the_tree == 0:
            the_tree = self._tree
        parent = self._get_parent(the_tree = the_tree, child = element)
        if parent == None:
            return
        counter = -1
        for child in parent:
            counter += 1
            if child == element:
                if len(parent) == counter + 1:
                    return None
                return parent[counter + 1]

    def _get_parent(self, child, the_tree = 0):
        """

        the_tree: an xml.etree of the whole tree

        child: an xml.etree of the child element

        There is no direct way to get the parent of an element in etree. This
        method makes a child-parent dictionary of the whold tree, than accesses
        the dictionary

        """
        if the_tree == 0:
            the_tree = self._tree
        child_parent_map = dict((c, p) for p in the_tree.getiterator() for c in p)
        parent = child_parent_map.get(child)
        return parent

    def _get_grandparent(self, child, the_tree = 0):
        parent = self._get_parent(child = child, the_tree = the_tree)
        grandparent = self._get_parent(child = parent, the_tree = the_tree)

    def _is_parenthesis(self, element):
        if element != None and element.tag == 'mfenced' and element.get('open') == '(' and\
                    element.get('close') == ')':
            return True


    def _change_element(self, element, name, **attributes):
        element.tag = name
        the_keys = element.attrib.keys()
        for the_key in the_keys:
            del(element.attrib[the_key])
        for att in attributes:
            element.set(att, attributes[att])


    def _add_namespace(self):
        attributes = self._tree.attrib
        value = attributes.get('xmlns')
        if not value:
            self._tree.set('xmlns', self._mathml_ns)

    def to_xml_string(self, encoding=None):
        if not encoding:
            encoding = self._output_encoding
        self._add_namespace()
        xml_string = tostring(self._tree, encoding=encoding)
        return xml_string

    def get_tree(self):
        self._add_namespace()
        return self._tree

    def _add_num_to_tree(self, token, the_type):
        element = self._make_element('mn', text=token)
        self._append_el.append(element)

    def _add_neg_num_to_tree(self, token, the_type):
        num = token[1:]
        element = self._make_element('mo', text='-')
        self._append_el.append(element)
        element = self._make_element('mn', text=num)
        self._append_el.append(element)

    def _add_alpha_to_tree(self, token, the_type):
        element = self._make_element('mi', text=token)
        self._append_el.append(element)

    def _add_symbol_to_tree(self, token, token_dict):
        token = token_dict['symbol']
        element = self._make_element('mi', text=token)
        self._append_el.append(element)

    def _add_operator_to_tree(self, token, token_info):
        if isinstance(token_info, dict):
            text = token_info.get('symbol')
        else:
            text = token
        element = self._make_element('mo', text=text)
        self._append_el.append(element)

    def _handle_binary(self, token, info):
        last_element = self._get_last_element() 
        if last_element == self._append_el: # no "previous sibling," and can't process
            self._add_operator_to_tree(token, info)
            return
        if token == '/':
            num_frac = 0
            if last_element.tag == 'mfrac':
                for child in last_element:
                    if child.tag == 'mfrac':
                        num_frac +=1
            if num_frac % 2 != 0:
                self._append_el = last_element
                last_element = self._get_last_element()
            if self._is_parenthesis(last_element):
                self._change_element(last_element, 'mrow', **{'class':'nominator'})
            nominator = deepcopy(last_element)
            self._append_el.remove(last_element)
            mfrac = self._make_element('mfrac',  nominator)
            self._append_el.append(mfrac)
            self._append_el = mfrac
        elif token == '^' or token == '_':
            if last_element.tag == 'msub' or last_element.tag == 'munder':
                if last_element.tag == 'msub':
                    new_element = self._make_element('msubsup')
                else:
                    new_element = self._make_element('munderover')
                for child in last_element: # should be just 2--check? 
                    element = deepcopy(child)
                    new_element.append(element)
                self._append_el.remove(last_element)
                self._append_el.append(new_element)
                self._append_el = new_element

            else:
                if last_element.text == self.greek_dict.get('Sigma') and token == '^':
                    el_name = 'mover'
                elif last_element.text == self.greek_dict.get('Sigma') and token == '_':
                    el_name = 'munder'
                elif token == '^':
                    el_name = 'msup'
                elif token == '_':
                    el_name = 'msub'
                base = deepcopy(last_element)
                self._append_el.remove(last_element)
                base = self._make_element(el_name,  base)
                self._append_el.append(base)
                self._append_el = base 

    def _handle_open_parenthesis(self):
        element = self._make_element('mfenced', open='(', separators='', close="")
        self._append_el.append(element)
        self._append_el = element

    def _handle_close_parenthesis(self):
        if self._append_el.tag == 'mfenced' and self._append_el.get('open') == '(':
            self._append_el.set('close', ')')
            parent = self._get_parent(self._append_el)
            self._append_el = parent
        else:
            if self._fenced_for_right_parenthesis: 
                element = self._make_element('mfenced', open='', separators='', close=")")
                self._append_el.append(element)
            else:
                element = self._make_element('mo', text=')')
                self._append_el.append(element)

    def _handle_double_bar(self, token, the_type):
        if self._append_el.tag == 'mfenced' and self._append_el.get('open') == u"\u2016":
            self._append_el.set('close', u"\u2016")
            parent = self._get_parent(self._append_el)
            self._append_el = parent
        else:
            element = self._make_element('mfenced', open=u"\u2016", separators='', close="")
            self._append_el.append(element)
            self._append_el = element


    def _add_special_to_tree(self, token, the_type):
        if token == '(':
            self._handle_open_parenthesis()
        elif token == ')':
            self._handle_close_parenthesis()
        elif token == '/' or token == '^' or token == '_':
            self._handle_binary(token, the_type)
        elif token == '||':
            self._handle_double_bar(token, the_type)



    def parse_string(self, the_string):
        """
        Need Documentation!


        """
        while the_string != '':
            the_string, token, token_info = self._parse_tokens(the_string)
            if isinstance(token_info, str):
                the_type = token_info
            else:
                the_type = token_info.get('type')
            if the_type == 'number':
                self._add_num_to_tree(token, the_type)
            elif the_type == 'neg_number':
                self._add_neg_num_to_tree(token, the_type)
            elif the_type == 'alpha':
                self._add_alpha_to_tree(token, the_type)
            elif the_type == 'symbol':
                self._add_symbol_to_tree(token, token_info)
            elif the_type == 'operator':
                self._add_operator_to_tree(token, token_info)
            elif the_type == 'special':
                self._add_special_to_tree(token, the_type)

            # for all elements
            if self._append_el.tag == 'mfrac' or self._append_el.tag == 'msup' or\
                    self._append_el.tag == 'msub' or self._append_el.tag == 'munder':
                last_element = self._get_last_element()
                prev_sib = self._get_previous_sibling(last_element)
                if prev_sib != None:
                    if self._is_parenthesis(last_element):
                        if self._append_el.tag == 'mfrac':
                            the_dict = {'class':'denominator'}
                        elif self._append_el.tag == 'msup':
                            the_dict = {'class':'superscript'}
                        elif self._append_el.tag == 'msub':
                            the_dict = {'class':'subcript'}
                        elif self._append_el.tag == 'munder':
                            the_dict = {'class':'munder'}
                        elif self._append_el.tag == 'mover':
                            the_dict = {'class':'mover'}
                        self._change_element(last_element, 'mrow', **the_dict)
                    self._append_el = self._get_parent(self._append_el)
            elif self._append_el.tag =='msubsup' or self._append_el.tag == 'munderover':
                last_element = self._get_last_element()
                prev_sib = self._get_previous_sibling(last_element)
                prev_prev_sib =self._get_previous_sibling(prev_sib) 
                if prev_prev_sib != None:
                    if self._is_parenthesis(last_element):
                        if self._append_el.tag == 'msubsup':
                            the_dict = {'class':'subsuper'}
                        else:
                            the_dict = {'class':'munderover'}
                        self._change_element(last_element, 'mrow', **the_dict)
                    self._append_el = self._get_parent(self._append_el)


    def _look_at_next_token(self, the_string):
        the_string, token, the_type = self._parse_tokens(the_string)
        return the_string, token, the_type


    def _parse_tokens(self, the_string):

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

        match = self._number_re.match(the_string)

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
