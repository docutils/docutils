import unittest

from xml.etree.ElementTree import tostring
import asciitomathml


class TestAsciiToMathML(unittest.TestCase):


    def test_simple(self):
        self.assertEquals('x', 'x')

    def test_make_element(self):
        mathml_obj = asciitomathml.AsciiMathML()
        root = mathml_obj.make_element('root')
        child1 = mathml_obj.make_element('child1')
        child2 = mathml_obj.make_element('child2')
        new_root = mathml_obj.make_element('new_root', child1, child2)
        self.assertEquals(tostring(new_root),'<new_root><child1 /><child2 /></new_root>')

    def test_get_parent(self):
        mathml_obj = asciitomathml.AsciiMathML()
        root = mathml_obj.make_element('root')
        parent = mathml_obj.get_parent(root)
        child_element = mathml_obj.make_element('child1')
        root.append(child_element)
        parent = mathml_obj.get_parent(child = child_element, the_tree = root)
        self.assertEquals(root, parent)
        
    def test_get_previous_sibling(self):
        mathml_obj = asciitomathml.AsciiMathML()
        root = mathml_obj.make_element('root')
        child_element1 = mathml_obj.make_element('child1')
        child_element2 = mathml_obj.make_element('child2')
        root.append(child_element1)
        root.append(child_element2)
        prev_sib = mathml_obj.get_previous_sibling(child_element2)
        prev_sib = mathml_obj.get_previous_sibling(child_element2, the_tree = root)
        self.assertEquals(prev_sib, child_element1)

    def test_get_following_sibling(self):
        mathml_obj = asciitomathml.AsciiMathML()
        root = mathml_obj.make_element('root')
        child_element1 = mathml_obj.make_element('child1')
        child_element2 = mathml_obj.make_element('child2')
        root.append(child_element1)
        root.append(child_element2)
        foll_sib = mathml_obj.get_following_sibling(child_element1 )
        foll_sib = mathml_obj.get_following_sibling(child_element1, the_tree = root)
        self.assertEquals(foll_sib, child_element2)

    def test_symbol_dict(self):
        mathml_obj = asciitomathml.AsciiMathML()
        the_dict = mathml_obj.symbol_dict

    def test_parse_tokens(self):
        mathml_obj = asciitomathml.AsciiMathML()
        rest_of_string, token, the_type = mathml_obj.parse_tokens('1')
        self.assertEquals(token, '1')
        rest_of_string, token, the_type = mathml_obj.parse_tokens('-55.6')
        self.assertEquals(token, '-55.6')
        rest_of_string, token, the_type = mathml_obj.parse_tokens('alpha')
        self.assertEquals(token, u"\u03B1")
        rest_of_string, token, the_type = mathml_obj.parse_tokens('')
        self.assertEquals(token, None)

    def test_parse_string(self):
        mathml_obj = asciitomathml.AsciiMathML()
        the_string = '-1 -55.6 text alpha'
        mathml_obj.parse_string(the_string)

    def test_to_xml_string(self):
        mathml_obj = asciitomathml.AsciiMathML()
        the_string = '1'
        mathml_obj.parse_string(the_string)
        xml_string = mathml_obj.to_xml_string()
        result = '<math xmlsn="http://www.w3.org/1998/Math/MathML"><mstyle><mn>1</mn></mstyle></math>'
        self.assertEquals(xml_string, result)


if __name__ == '__main__':
    unittest.main()

