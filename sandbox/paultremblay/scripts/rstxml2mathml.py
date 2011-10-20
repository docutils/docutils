import io, argparse, sys
import xml.etree.cElementTree as etree
import asciimathml


def parse_args():
    desc = """Inserts Mathmx elements into an rst document. 
In order to use the script, first run rs2txml.py on the RST file.
Then run this script on that resulting file
Or, in one pass: rst2xml.py <infile> | python3 rstxml2mathml.py
    """
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('in_file', default = sys.stdin, nargs='?',  
            help = 'the file to input; default is standard in')
    args =  parser.parse_args()
    return args

def convert_to_mathml():
    args = parse_args()
    standard_in = False
    in_file = args.in_file
    if isinstance(in_file, io.TextIOWrapper):
        the_string = sys.stdin.read()
        xml_tree = etree.fromstring(the_string)
    else:
        xml_tree = etree.ElementTree().parse(in_file)
    for element in xml_tree.iter():
        if element.tag == 'math' or element.tag == 'math_block':
            mathml_tree  = asciimathml.parse(element.text)
            mathml_tree.set("title", element.text)
            mathml_tree.set("xmlns", "http://www.w3.org/1998/Math/MathML")
            element.append(etree.XML(etree.tostring(mathml_tree)))
            element.text = ''
    string_tree = etree.tostring(xml_tree, encoding="utf-8") 
    if sys.version_info < (3,):
        sys.stdout.write(string_tree)
    else:
        sys.stdout.write(string_tree.decode())

if __name__ == '__main__':
    convert_to_mathml()
