 # -*- coding: UTF-8 -*-
import re, sys, copy
from copy import deepcopy

from xml.etree.ElementTree import Element, tostring
import xml.etree.ElementTree as etree

class InvalidAsciiMath(Exception):
    """
    handle invalid Ascii Math

    """
    pass

class AsciiMathML:

    greek_dict = {
'alpha':       u'\u03B1',
'beta':        u'\u03B2',
'chi' :        u'\u03C7',
'delta':       u'\u03B4',
'epsi':        u'\u03B5',
'varepsilon':  u'\u025B',
'eta':         u'\u03B7',
'gamma':       u'\u03B3',
'iota':        u'\u03B9',
'kappa':       u'\u03BA',
'lambda':      u'\u03BB',
'mu':          u'\u03BC',
'nu':          u'\u03BD',
'omega':       u'\u03C9',
'phi':         u'\u03C6',
'varphi':      u'\u03D5',
'pi' :         u'\u03C0',
'psi':         u'\u03C8',
'Psi':         u'\u03A8',
'rho':         u'\u03C1',
'sigma':       u'\u03C3',
'tau':         u'\u03C4',
'theta':       u'\u03B8',
'vartheta':    u'\u03D1',
'Theta':       u'\u0398',
'upsilon':     u'\u03C5',
'xi':          u'\u03BE',
'zeta':        u'\u03B6'
        }

    symbol_dict = {
            }
    text_dict = {
            'and':'and', # space = true
            'or' : 'or', # space = true
            'if' :'if', # space = true
            }

    symbol_dict.update(greek_dict)
    operator_dict = {
'min':   'min',
'max':   'max',
'lim':   'lim',
'Lim':   'Lim',
'sin':   'sin',
'cos':   'cos',
'tan':   'tan',
'sinh':  'sinh',
'cosh':  'cosh',
'tanh':  'tanh',
'cot':   'cot',
'sec':   'sec',
'csc':   'csc',
'log':   'log',
'ln':    'ln',
'det':   'det',
'gcd':   'gcd',
'lcm':   'lcm',
'Delta': u'\u0394',
'Gamma': u'\u0393',
'Lambda':u'\u039B',
'Omega': u'\u03A9',
'Phi' :  u'\u03A6',
'Pi' :   u'\u03A0',
'Sigma': u'\u2211',
'sum':   u'\u2211',
'Xi':    u'\u039E',
'prod':  u'\u220f',
'^^^':   u'\u22c0',
'vvv':   u'\u22c1',
'nnn':   u'\u22c2',
'uuu':   u'\u22c3',
"*" :    u"\u22C5",
"**":    u"\u22C6",
"//": u"/",
"\\\\":  u"\\",
"setminus": u"\\",
"xx":   u"\u00D7",
"-:": u"\u00F7",
"@":  u"\u2218",
"o+": u"\u2295",
"ox": u"\u2297",
"o.": u"\u2299",
"^^": u"\u2227",
"vv":  u"\u2228",
"nn": u"\u2229",
"uu": u"\u222A",
"!=":u"\u2260",
":=":  u":=",
"lt": u"<",
"<=":  u"\u2264",
"lt=":  u"\u2264",
">=": u"\u2265",
"geq":  u"\u2265",
"ge":  u"\u2265",
"-<":  u"\u227A",
"-lt": u"\u227A",
">-": u"\u227B",
"-<=": u"\u2AAF",
">-=": u"\u2AB0",
"in": u"\u2208",
"!in":u"\u2209",
"sub": u"\u2282",
"sup":u"\u2283",
"sube": u"\u2286",
"supe":  u"\u2287",
"-=": u"\u2261",
"~=": u"\u2245",
"~~":  u"\u2248",
"prop":  u"\u221D",
"not": u"\u00AC",
"=>": u"\u21D2",
"<=>": u"\u21D4",
"AA": u"\u2200",
"EE": u"\u2203",
"_|_": u"\u22A5",
"TT":  u"\u22A4",
"|--": u"\u22A2",
"|==": u"\u22A8",
"int": u"\u222B",
"oint":u"\u222E",
"del":  u"\u2202",
"grad":u"\u2207",
"+-":u"\u00B1",
"O/":u"\u2205",
"oo":u"\u221E",
'aleph': u"\u2135",
"...":u"...",
":.":u"\u2234",
"/_": u"\u2220",
"\\ ":u"\u00A0",
"quad": u"\u00A0\u00A0",
"qquad": u"\u00A0\u00A0\u00A0\u00A0",
"cdots": u"\u22EF",
"vdots": u"\u22EE",
"ddots": u"\u22F1",
"diamond": u"\u22C4",
"square": u"\u25A1",
"|__":u"\u230A",
"__|": u"\u230B",
"|~":u"\u2308",
"~|": u"\u2309",
"CC": u"\u2102",
"NN":u"\u2115",
"QQ": u"\u211A",
"RR": u"\u211D",
"ZZ": u"\u2124",
"dim":  u"dim",
"mod":  u"mod",
"lub":  u"lub",
"glb":  u"glb",
            }
# left off f and g because don't know what to do with them

    special_dict = {
'(':{'type':'special'},
'{':{'type':'special'},
'}':{'type':'special'},
')':{'type':'special'},
'[':{'type':'special'},
']':{'type':'special'},
'/':{'type':'special'},
'^':{'type':'special'},
'_':{'type':'special'},
'|':{'type':'special'},
'||':{'type':'special'},
'(:':{'type':'special'},  
':)':{'type':'special'},  
'<<':{'type':'special'}, 
'>>':{'type':'special'},  
'{:':{'type':'special'},
':}':{'type':'special'},
'hat':{'type':'special'},
'bar':{'type':'special'},
'vec':{'type':'special'},
'dot':{'type':'special'},
'ddot':{'type':'special'},
'ul':{'type':'special'},
'root':{'type':'special'},
'stackrel':{'type':'special'},
'frac':{'type':'special'},
'sqrt':{'type':'special'},
'text':{'type':'special'},

            }

    under_over_list = [u"\u2211", u"\u220f", u"\u22c0", u"\u22c1",u"\u22c2",u"\u22c3", "min", "max", "Lim", "lim"]
    under_over_base_last = ['hat', 'bar', 'vec', 'dot', 'ddot', 'ul']
    over_list = ['hat', 'bar', 'vec', 'dot', 'ddot']
    under_list = ['ul']
    fence_list = ['(', ')', '{', '}', '[', ']', u'\u2239', u'\u232a', '(:', ':)', '<<', '>>', '{:', ':}']
    open_fence_list = ['(', '{', '[', u'\u2329', '<<', '{:']
    close_fence_list = [')', '}', ']', u'\u232A', '>>', ':}']
    function_list = ['root', 'stackrel', 'frac', 'sqrt']
    group_func_list = ['min', 'max', 'sin', 'cos', 'tan', 'sinh', 'cosh', 'tanh', 'cot', 'sec', 'csc', 'log', 'ln', 'det', 'gcd', 'lcm']
    fence_pair = {')':'(', '}':'{', ']':'[', u'\u232A':u'\u2329', ':}': '{:'} # last pair goes first in this dic
    over_dict = {'hat':'^', 'bar':u"\u00AF", 'vec':u"\u2192", 'dot':u".", 'ddot':u".."}
    under_dict = {'ul': u"\u0332"}
    sym_list = symbol_dict.keys()
    spec_name_list = special_dict.keys()
    op_name_list = operator_dict.keys()
    text_list = text_dict.keys()
    names = sorted(sym_list + op_name_list + spec_name_list + text_list, key=lambda key_string: len(key_string), reverse=True)

    def __init__(self, output_encoding = 'utf8'):
        self._number_re = re.compile('-?(\d+\.(\d+)?|\.?\d+)')
        self._tree = Element('math')
        mstyle = etree.SubElement(self._tree, 'mstyle')
        self._mathml_ns = 'http://www.w3.org/1998/Math/MathML'
        self._append_el = mstyle
        self._output_encoding = output_encoding
        self._fenced_for_right = False #  used fence for right fence with no match
        self._fenced_for_left = False #  used fence for left fence with no match
        self._use_fence = True #  use <mfence> for fences

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

    def _change_element(self, element, name, **attributes):
        """
        Changes just the top element to the name "element' with the **attributes passed to it.

        """
        element.tag = name
        the_keys = element.attrib.keys()
        for the_key in the_keys:
            del(element.attrib[the_key])
        for att in attributes:
            element.set(att, attributes[att])

                
    def _fix_open_fence(self, element):
        """
        changes <mfence open="(" close="" 
         ...
         </mfenced>

         <mo>(</mo>
         ...


        """
        parent = self._get_parent(element)
        the_open = element.get('open')
        position = 0
        found = False
        for e in parent:
            if e == element:
                found = 1
                break
            position += 1
        paren = self._make_element('mo', text = the_open)
        parent.insert(position, paren)
        c = 1 + position
        for e in element:
            parent.insert(c, e)
            c += 1
        parent.remove(element)

    def _insert_mrow(self, element, class_name):
        """
        Inserts an mrow element around element

        """
        if len(element) == 1:
            return
        self._change_element(element, 'mrow', **{'class':class_name})
        new_element = deepcopy(element)
        self._append_el.remove(element)
        parenthesis = self._make_element('mfenced', open='(', separators='', close=")")
        parenthesis.append(new_element)
        self._append_el.append(parenthesis)

    def _count_commas(self, element):
        """
        counts commas for matrix
        """
        if  element == None:
            return 0
        count = 0
        for child in element:
            if child.tag == 'mo' and child.text == ',':
                count += 1
        return count

    def _is_matrix(self, element):
        """
        Tests if element is in fact a matrix

        """
        if len(element) < 3:
            return
        if not self._is_full_fenced(element):
            return
        counter = 0
        row_len = None
        for child in element:
            if counter % 2 == 0: # even
                if not self._is_full_fenced(child):
                    return
                num_commas = self._count_commas(child)
                if row_len == None:
                    row_len = num_commas
                else:
                    if num_commas != row_len:
                        return
                inner_counter = 0
            else:
                if child.tag != 'mo' or child.text != ',':
                    return
            counter += 1
        return True


    def _is_full_fenced(self, element):
        """
        Returns True if element is a fence with matching open and close; or if open is {: or closse is :}

        """
        if element == None:
            return
        close_fence = element.get('close')
        the_class = element.get('class')
        open_fence = element.get('open')
        if close_fence == u'\u232A': # don't remove these parenthesis
            return
        pair = self.fence_pair.get(close_fence)
        if the_class == 'invisible':
            pass
        elif not pair:
            return
        if element.tag == 'mfenced': 
            return True


    def _add_num_to_tree(self, token, the_type):
        element = self._make_element('mn', text=token)
        self._append_el.append(element)


    def _add_text_to_tree(self, text):
        present_text = self._append_el.text
        if present_text == None:
            present_text = ''
        self._append_el.text = present_text + text

    def _add_text_el_to_tree(self):
        element = self._make_element('mtext')
        self._append_el.append(element)
        self._append_el = element

    def _end_text_el_to_tree(self):
        self._append_el.attrib.pop('open')
        self._append_el = self._get_parent(self._append_el)

    def _add_special_text_to_tree(self, text):
        """
        adds if , and , or

        """
        element = self._make_element('mspace', **{'width':'1ex'})
        self._append_el.append(element)
        element = self._make_element('mo', text=text)
        self._append_el.append(element)
        element = self._make_element('mspace', **{'width':'1ex'})
        self._append_el.append(element)

    def _add_neg_num_to_tree(self, token, the_type):
        groups = ['msup', 'msub', 'munderover', 'munder', 'mover', 'mroot', 'msqrt', 'mfrac']
        if self._append_el.tag in groups:
            element = self._make_element('mrow', **{'class':'neg-num'})
            self._append_el.append(element)
            append_el = element
        else:
            append_el = self._append_el
        num = token[1:]
        element = self._make_element('mo', text='-')
        append_el.append(element)
        element = self._make_element('mn', text=num)
        append_el.append(element)

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

    def _do_matrix(self):
        last_element = self._get_last_element()
        is_matrix = self._is_matrix(last_element)
        if not is_matrix:
            return
        the_open = last_element.get('open')
        close = last_element.get('close')
        the_class = last_element.get('class')
        the_dict = {'open': the_open, 'close': close, 'separators':''}
        if the_class:
            the_dict['class'] = the_class
        fenced = self._make_element('mfenced', **the_dict)
        table = self._make_element('mtable')
        fenced.append(table)
        for child in last_element:
            if self._is_full_fenced(child):
                row = self._make_element('mtr')
                table.append(row)
                cell = self._make_element('mtd')
                row.append(cell)
                for gc in child:
                    if gc.tag != 'mo' or gc.text != ',':
                        cell.append(gc)
                    else:
                        cell = self._make_element('mtd')
                        row.append(cell)
        self._append_el.remove(last_element)
        self._append_el.append(fenced)


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
            if self._is_full_fenced(last_element):
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
                if last_element.text in self.under_over_list and token == '^':
                    el_name = 'mover'
                elif last_element.text in self.under_over_list and token == '_':
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


    def _handle_open_fence(self, token):
        if self._use_fence:
            element = self._make_element('mfenced', open=token, separators='', close="")
        else:
            element = self._make_element('mo', text=token)
        self._append_el.append(element)
        self._append_el = element

    def _handle_close_fence(self, token):
        first_match = self.fence_pair.get(token)
        element = self._append_el
        match_found = False
        while element != None:
            if element.tag == 'mfenced' and element.get('open') == first_match :
                element.set('close', token)
                parent = self._get_parent(element)
                self._append_el = parent
                match_found = True
                break
            elif element.tag == 'mfenced' and ( element.get('open') == '{:' or token == ':}'):
                element.set('class','invisible')
                parent = self._get_parent(element)
                self._append_el = parent
                match_found = True
                break
            element = self._get_parent(element)

        if match_found:
            return
        if self._fenced_for_right:
            element = self._make_element('mfenced', open='', separators='', close=token)
            self._append_el.append(element)
        else:
            element = self._make_element('mo', text=token)
            self._append_el.append(element)

    def _handle_close_fence_old(self, token):
        the_open = self._append_el.get('open')
        first_match = self.fence_pair.get(token)
        if self._append_el.tag == 'mfenced' and the_open == first_match :
            self._append_el.set('close', token)
            parent = self._get_parent(self._append_el)
            self._append_el = parent
        elif self._append_el.tag == 'mfenced' and ( the_open == '{:' or token == ':}'):
            self._append_el.set('class','invisible')
            parent = self._get_parent(self._append_el)
            self._append_el = parent
        else:
            self._find_match_fence(self._append_el, token)
            if self._fenced_for_right:
                element = self._make_element('mfenced', open='', separators='', close=token)
                self._append_el.append(element)
            else:
                element = self._make_element('mo', text=token)
                self._append_el.append(element)

    def _find_match_fence(self, element, close):
        first_match = self.fence_pair.get(close)
        while element != None:
            print(element.tag)
            if element.tag == 'mfenced' and element.get('open') == first_match :
                self._append_el = element
                element.set('close', close)
                parent = self._get_parent(element)
                self._append_el = parent
                break
            element = self._get_parent(element)

    def _handle_double_single_bar(self, token, the_type):
        if token == '||':
            the_chr = u'\u2016'
        elif token == '|':
            the_chr = '|'

        if self._append_el.tag == 'mfenced' and self._append_el.get('open') == the_chr:
            self._append_el.set('close', the_chr)
            parent = self._get_parent(self._append_el)
            self._append_el = parent
        else:
            element = self._make_element('mfenced', open=the_chr, separators='', close="")
            self._append_el.append(element)
            self._append_el = element


    def _handle_over(self, token):
        element = self._make_element('mover', **{'class':token} )
        self._append_el.append(element)
        self._append_el = element

    def _handle_under(self, token):
        element = self._make_element('munder', **{'class':token} )
        self._append_el.append(element)
        self._append_el = element

    def _handle_function(self, token):
        if token == 'root':
            element = self._make_element('mroot')
            self._append_el.append(element)
            self._append_el = element
        elif token == 'stackrel':
            element = self._make_element('mover', **{'class':'stackrel'})
            self._append_el.append(element)
            self._append_el = element
        elif token == 'frac':
            element = self._make_element('mfrac')
            self._append_el.append(element)
            self._append_el = element
        elif token == 'sqrt':
            element = self._make_element('msqrt')
            self._append_el.append(element)
            self._append_el = element

    def _add_special_to_tree(self, token, the_type):
        if token in self.open_fence_list:
            self._handle_open_fence(token)
        elif token in self.close_fence_list:
            self._handle_close_fence(token)
        elif token == '/' or token == '^' or token == '_':
            self._handle_binary(token, the_type)
        elif token == '||' or token == '|':
            self._handle_double_single_bar(token, the_type)
        elif token == '|':
            self._handle_single_bar(token, the_type)
        elif token in self.over_list:
            self._handle_over(token)
        elif token in self.under_list:
            self._handle_under(token)
        elif token in self.function_list:
            self._handle_function(token)

    def _add_fence_to_tree(self, token, the_type):
        if token == '(:' or  token == '<<':
            token = u"\u2329"
        if token == ':)' or token == '>>':
            token = u"\u232a"
        if token in self.open_fence_list:
            self._handle_open_fence(token)
        elif token in self.close_fence_list:
            self._handle_close_fence(token)

    def _fix_tree(self):
        for e in self._tree.getiterator('mfenced'):
            if e.get('close') == '' and e.get('class') != 'invisible' and not self._fenced_for_left:
                self._fix_open_fence(e)
        for e in self._tree.getiterator():
            if e.tag == 'mfrac' and len(e) != 2:
                element = self._make_element('mo')
                while len(e) != 2:
                    element = self._make_element('mo')
                    e.insert(len(e), element)
            elif e.tag == 'mroot' and len(e) != 2:
                if len(e) == 1:
                    element = self._make_element('mo')
                    e.insert(0,element)
                else:
                    while len(e) != 2:
                        element = self._make_element('mo')
                        e.insert(len(e), element)
            elif (e.tag == 'mover' or e.tag == 'munder') and len(e) != 2:
                char = self.over_dict.get(e.get('class'))
                if not char:
                    char = self.under_dict.get(e.get('class'))
                if len(e) == 0: 
                    element = self._make_element('mo')
                    e.insert(0, element)
                    element = self._make_element('mo', text=char)
                    e.insert(1, element)
                elif len(e) == 1: 
                    element = self._make_element('mo')
                    e.insert(1, element)
            elif (e.tag == 'munderover' or e.tag == 'msubsup') and len(e) != 3:
                element = self._make_element('mo')
                while len(e) != 3:
                    element = self._make_element('mo')
                    e.insert(len(e), element)
            elif (e.tag == 'msup' or e.tag == 'msub') and len(e) != 2:
                element = self._make_element('mo')
                while len(e) != 2:
                    element = self._make_element('mo')
                    e.insert(len(e), element)


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
            if the_type == 'text':
                text = token
                self._add_text_to_tree(text)
            elif the_type == 'start_text':
                self._add_text_el_to_tree()
            elif the_type == 'end_text':
                self._end_text_el_to_tree()
            elif the_type == 'special_text':
                self._add_special_text_to_tree(token)
            elif the_type == 'number':
                self._add_num_to_tree(token, the_type)
            elif the_type == 'neg_number':
                self._add_neg_num_to_tree(token, the_type)
            elif the_type == 'alpha':
                self._add_alpha_to_tree(token, the_type)
            elif the_type == 'symbol':
                self._add_symbol_to_tree(token, token_info)
            elif the_type == 'operator':
                self._add_operator_to_tree(token, token_info)
            elif token in self.fence_list:
                self._add_fence_to_tree(token, the_type)
            elif the_type == 'special':
                self._add_special_to_tree(token, the_type)

            if self._append_el.tag == 'mover' and self._append_el.get('class') == 'stackrel' and len(self._append_el) == 2:
                if self._is_full_fenced(self._append_el[0]):
                    self._change_element(self._append_el[0], 'mrow', **{'class':'top'})
                if self._is_full_fenced(self._append_el[1]):
                    self._change_element(self._append_el[1], 'mrow', **{'class':'bottom'})
                top = deepcopy(self._append_el[0])
                self._append_el[0] = self._append_el[1]
                self._append_el[1] = top
                self._append_el = self._get_parent(self._append_el)
            elif (self._append_el.tag == 'mover' or self._append_el.tag == 'munder')\
                    and self._append_el.get('class') in self.under_over_base_last and len(self._append_el) > 0:
                last_element = self._get_last_element()
                if self._is_full_fenced(last_element): # remove parenthesis
                    if self._append_el.tag == 'mover':
                        the_dict = {'class':'mover'}
                    if self._append_el.tag == 'munder':
                        the_dict = {'class':'munder'}
                    self._change_element(last_element, 'mrow', **the_dict)
                text = self._append_el.get('class') # add top
                if self._append_el.tag == 'mover':
                    text = self.over_dict.get(text) 
                elif self._append_el.tag == 'munder':
                    text = self.under_dict.get(text) 
                element = self._make_element('mo', text=text)
                self._append_el.append(element)
                self._append_el = self._get_parent(self._append_el)
            elif self._append_el.tag == 'msqrt' and len(self._append_el) == 1:
                if self._is_full_fenced(self._append_el[0]):
                    self._change_element(self._append_el[0], 'mrow', **{'class':'radical'})
                self._append_el = self._get_parent(self._append_el)
            elif self._append_el.tag == 'mroot' and len(self._append_el) == 2:
                if self._is_full_fenced(self._append_el[0]):
                    self._change_element(self._append_el[0], 'mrow', **{'class':'index'})
                if self._is_full_fenced(self._append_el[1]):
                    self._change_element(self._append_el[1], 'mrow', **{'class':'base'})
                the_index = deepcopy(self._append_el[0])
                self._append_el[0] = self._append_el[1]
                self._append_el[1] = the_index
                self._append_el = self._get_parent(self._append_el)

            elif (self._append_el.tag == 'mfrac' or self._append_el.tag == 'msup' or\
                    self._append_el.tag == 'msub' or self._append_el.tag == 'munder'\
                    or self._append_el.tag == 'mover') and len(self._append_el) == 2:
                if self._is_full_fenced(self._append_el[1]):
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
                    self._change_element(self._append_el[1], 'mrow', **the_dict)
                self._append_el = self._get_parent(self._append_el)
            elif self._append_el.tag =='msubsup' or self._append_el.tag == 'munderover':
                last_element = self._get_last_element()
                prev_sib = self._get_previous_sibling(last_element)
                prev_prev_sib =self._get_previous_sibling(prev_sib) 
                if prev_prev_sib != None:
                    if self._is_full_fenced(last_element):
                        if self._append_el.tag == 'msubsup':
                            the_dict = {'class':'subsuper'}
                        else:
                            the_dict = {'class':'munderover'}
                        self._change_element(last_element, 'mrow', **the_dict)
                    self._append_el = self._get_parent(self._append_el)

            last_element = self._get_last_element()
            if last_element.tag == 'mfenced' and last_element.get('close') == ':}':
                self._change_element(last_element, 'mrow', **{'class':'invisible'})

            if self._is_full_fenced(last_element) and len(self._append_el)> 1 :
                prev_sib = self._get_previous_sibling(last_element)
                is_function = False
                if prev_sib.text in self.group_func_list:
                    is_function = True
                if prev_sib.tag == 'munderover':
                    if prev_sib[0].tag == 'mo' and prev_sib[0].text in self.group_func_list:
                        is_function = True
                if is_function:
                    self._insert_mrow(last_element, 'function')

            self._do_matrix()
        self._fix_tree()


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
        if self._append_el.tag == 'mtext':
            next_char = the_string[0]
            if not self._append_el.get('open'):
                if next_char == ' ':
                    return the_string[1:], ' ', {'type':'empty_text'}
                elif next_char == '(' or next_char == '{' or next_char == '[':
                    self._append_el.set('open', next_char)
                    return the_string[1:], ' ', {'type': 'empty_text'}
                else: # false text; continue
                    self._append_el = self._get_parent(self._append_el)
            else:
                first_match = self.fence_pair.get(next_char)
                if first_match:
                    return the_string[1:], next_char, {'type':'end_text'}
                else:
                    return the_string[1:], next_char, {'type':'text'}
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

        for name in self.names:
            if the_string.startswith(name):
                the_found = the_string[:len(name)]
                symbol = self.symbol_dict.get(the_found)
                operator = self.operator_dict.get(the_found)
                special = self.special_dict.get(the_found)
                text = self.text_dict.get(the_found)
                if the_found == 'text':
                    return the_string[len(name):], name, {'type': 'start_text'} 
                elif symbol != None:
                    return the_string[len(name):], name, {'type': 'symbol', 'symbol': symbol} 
                elif special != None:
                    return the_string[len(name):], name, special
                elif operator != None:
                    return the_string[len(name):], name, {'type': 'operator', 'symbol': operator} 
                elif text != None:
                    return the_string[len(name):], name, {'type': 'special_text'} 

        # found either an operator or a letter

        if the_string[0].isalpha():
            return the_string[1:], the_string[0], 'alpha'
        else:
            return the_string[1:], the_string[0], 'operator'



def ascii_to_xml_string(the_string):
    if isinstance(the_string, str) and  sys.version_info < (3,):
        the_string = the_string.decode('utf8')
    math_obj =  AsciiMathML()
    math_obj.parse_string(the_string)
    xml_string = math_obj.to_xml_string()
    return xml_string

def ascii_to_math_tree(the_string):
    if isinstance(the_string, str) and  sys.version_info < (3,):
        the_string = the_string.decode('utf8')
    math_obj =  AsciiMathML()
    math_obj.parse_string(the_string)
    math_tree = math_obj.get_tree()
    return math_tree
