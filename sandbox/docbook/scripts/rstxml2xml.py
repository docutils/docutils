#!/usr/bin/env python
import sys
import argparse
if sys.version_info < (3,0):
    sys.stderr.write('Need python 3.0 or greater\n')
    sys.stderr.wrtie('Script quiting\n')
    sys.exit(1)
import xml.etree.cElementTree as etree
import asciitomathml.asciitomathml 
from xml.etree.ElementTree import Element, tostring

class InvaidXml(Exception):
    # not used at this point
    pass

class ConverttoMathml():
  
    def __init__(self):
        pass

    def _parse_args(self):
        desc = """Inserts Mathml elements into an rst document. 
In order to use the script, first run rs2txml.py on the RST file.
Then run this script on that resulting file
Or, in one pass: rst2xml.py <infile> | python3 rstxml2mathml.py
        """
        parser = argparse.ArgumentParser(description=desc)
        parser.add_argument('in_file', default = sys.stdin, nargs='?',  
                help = 'the file to input; default is standard in')
        parser.add_argument('-o,--out', nargs=1, dest="out_file" ) 
        parser.add_argument('--out-encoding', nargs=1, dest="out_encoding" ) 
        parser.add_argument('--in-encoding', nargs=1, dest="in_encoding" ) 
        args =  parser.parse_args()
        return args

    def convert_to_mathml(self):
        args = self._parse_args()
        out_encoding = args.out_encoding
        if not out_encoding:
            out_encoding = 'utf8'
        else:
            out_encoding = out_encoding[0]
        in_encoding = args.in_encoding
        if not in_encoding:
            in_encoding = 'utf8'
        else:
            in_encoding = in_encoding[0]
        standard_in = False
        in_file = args.in_file
        out_file = args.out_file
        if out_file:
            out_file = out_file[0]
        if not isinstance(in_file, str):
            orig_string = sys.stdin.read()
        else:
            read_obj = open(in_file, mode='r', encoding=in_encoding)
            lines = read_obj.readlines()
            orig_string = ''.join(lines)
        old_tree = etree.XML(orig_string)
        for ma in ['math_block', 'math']:
            for e in old_tree.iter(ma):
                math_text = e.text
                e.text = ''
                if ma == 'math_block':
                    math_obj =  asciitomathml.asciitomathml.AsciiMathML(mstyle={'displaystyle':'true'})
                else:
                    math_obj =  asciitomathml.asciitomathml.AsciiMathML()
                math_obj.parse_string(math_text)
                math_tree = math_obj.get_tree()
                math_string = tostring(math_tree, encoding='ascii') # <encoding=utf8> cannot appear here
                math_tree = etree.XML(math_string)
                e.insert(0, math_tree)
        xml_string = tostring(old_tree, encoding=out_encoding)
        if out_file: 
            write_obj = open(out_file, mode='w', encoding=out_encoding)
            write_obj.write(xml_string.decode(out_encoding))
            write_obj.close()
        else:
            sys.stdout.write(xml_string.decode(out_encoding))



if __name__ == '__main__':
    main_obj = ConverttoMathml()
    main_obj.convert_to_mathml()
