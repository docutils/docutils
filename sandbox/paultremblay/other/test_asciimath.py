import unittest

from xml.etree.ElementTree import tostring
import asciitomathml
from asciitomathml import AsciiMathML



class TestAsciiToMathML(unittest.TestCase):


    def test_simple(self):
        self.assertEquals('x', 'x')

    def test_make_element(self):
        mathml_obj = asciitomathml.AsciiMathML()
        root = mathml_obj._AsciiMathML__make_element('root')
        child1 = mathml_obj._AsciiMathML__make_element('child1')
        child2 = mathml_obj._AsciiMathML__make_element('child2')
        new_root = mathml_obj._AsciiMathML__make_element('new_root', child1, child2)
        self.assertEquals(tostring(new_root),'<new_root><child1 /><child2 /></new_root>')

    def test_get_parent(self):
        mathml_obj = asciitomathml.AsciiMathML()
        root = mathml_obj._AsciiMathML__make_element('root')
        parent = mathml_obj._AsciiMathML__get_parent(root)
        child_element = mathml_obj._AsciiMathML__make_element('child1')
        root.append(child_element)
        parent = mathml_obj._AsciiMathML__get_parent(child = child_element, the_tree = root)
        self.assertEquals(root, parent)
        
    def test_get_previous_sibling(self):
        mathml_obj = asciitomathml.AsciiMathML()
        root = mathml_obj._AsciiMathML__make_element('root')
        child_element1 = mathml_obj._AsciiMathML__make_element('child1')
        child_element2 = mathml_obj._AsciiMathML__make_element('child2')
        root.append(child_element1)
        root.append(child_element2)
        prev_sib = mathml_obj._AsciiMathML__get_previous_sibling(child_element2)
        prev_sib = mathml_obj._AsciiMathML__get_previous_sibling(child_element2, the_tree = root)
        self.assertEquals(prev_sib, child_element1)

    def test_get_following_sibling(self):
        mathml_obj = asciitomathml.AsciiMathML()
        root = mathml_obj._AsciiMathML__make_element('root')
        child_element1 = mathml_obj._AsciiMathML__make_element('child1')
        child_element2 = mathml_obj._AsciiMathML__make_element('child2')
        root.append(child_element1)
        root.append(child_element2)
        foll_sib = mathml_obj._AsciiMathML__get_following_sibling(child_element1 )
        foll_sib = mathml_obj._AsciiMathML__get_following_sibling(child_element1, the_tree = root)
        self.assertEquals(foll_sib, child_element2)

    def test_symbol_dict(self):
        mathml_obj = asciitomathml.AsciiMathML()
        the_dict = mathml_obj.symbol_dict

    def test_parse_tokens(self):
        # mathml_obj =  AsciiToMathMlLocal()
        mathml_obj = asciitomathml.AsciiMathML()
        rest_of_string, token, the_type = mathml_obj._AsciiMathML__parse_tokens('1')
        self.assertEquals(token, '1')
        rest_of_string, token, the_type = mathml_obj._AsciiMathML__parse_tokens('-55.6')
        self.assertEquals(token, '-55.6')
        rest_of_string, token, token_dict = mathml_obj._AsciiMathML__parse_tokens('alpha')
        self.assertEquals(token_dict['symbol'], u"\u03B1")
        self.assertEquals(token, 'alpha')
        rest_of_string, token, the_type = mathml_obj._AsciiMathML__parse_tokens('')
        self.assertEquals(token, None)

    def test_parse_string(self):
        mathml_obj = asciitomathml.AsciiMathML()
        the_string = '-1 -55.6 text alpha'
        mathml_obj.parse_string(the_string)

    def test_to_xml_string_basic(self):
        the_strings = [
        ('1', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mn>1</mn></mstyle></math>'),
        ('-1', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mo>-</mo><mn>1</mn></mstyle></math>'),
        ('alpha', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mi>&#945;</mi></mstyle></math>'),
        ('a', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mi>a</mi></mstyle></math>'),
        ('+', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mo>+</mo></mstyle></math>'),
                ]

        for the_pair in the_strings:
            the_string = the_pair[0]
            result = the_pair[1]
            mathml_obj = asciitomathml.AsciiMathML(output_encoding='us-ascii')
            mathml_obj.parse_string(the_string)
            xml_string = mathml_obj.to_xml_string()
            self.assertEquals(xml_string, result)

    def __test_to_xml_string_parenthesis(self):
        the_strings = [
                ('(x)', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mfenced close=")" open="("><mi>x</mi></mfenced></mstyle></math>'),
                ('(x)x', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mfenced close=")" open="("><mi>x</mi></mfenced><mi>x</mi></mstyle></math>'),
                ('((x))x', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mfenced close=")" open="("><mfenced close=")" open="("><mi>x</mi></mfenced></mfenced><mi>x</mi></mstyle></math>'),

                ]
        for the_pair in the_strings:
            the_string = the_pair[0]
            result = the_pair[1]
            mathml_obj = asciitomathml.AsciiMathML(output_encoding='us-ascii')
            mathml_obj.parse_string(the_string)
            xml_string = mathml_obj.to_xml_string()
            self.assertEquals(xml_string, result)

    def test_anything(self):
        the_string = 'x^2'
        mathml_obj = asciitomathml.AsciiMathML(output_encoding='us-ascii')
        mathml_obj.parse_string(the_string)
        xml_string = mathml_obj.to_xml_string()

    def test_fractions(self):
        the_strings = [
        ('1/2', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mfrac><mn>1</mn><mn>2</mn></mfrac></mstyle></math>'),
        ('1/2 3', '<math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle><mfrac><mn>1</mn><mn>2</mn></mfrac><mn>3</mn></mstyle></math>'),
        ]
        for the_pair in the_strings:
            the_string = the_pair[0]
            result = the_pair[1]
            mathml_obj = asciitomathml.AsciiMathML(output_encoding='us-ascii')
            mathml_obj.parse_string(the_string)
            xml_string = mathml_obj.to_xml_string()
            self.assertEquals(xml_string, result)

if __name__ == '__main__':
    unittest.main()

