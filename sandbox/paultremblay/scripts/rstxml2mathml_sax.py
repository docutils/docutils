#!/usr/bin/python
#   $Id: sax_complete_copy.py 54 2011-04-17 15:44:41Z cynthia $

import os, sys, argparse, io
import xml.sax.handler
from xml.sax.handler import feature_namespaces
from StringIO import StringIO

"""
if sys.version_info < (3,):
else:
    from io import StringIO
"""

import asciimathml
from xml.etree.ElementTree import Element, tostring
import xml.etree.cElementTree as etree
import tempfile, subprocess, os
# import docutils.math.latex2mathml
from xml.sax import InputSource

"""
if sys.version_info < (3,):
    sys.stderr.write('Only run with pyton 3\n')
    sys.stderr.write('Script now quiting\n')
    sys.exit(1)
"""


class CopyTree(xml.sax.ContentHandler):


  
  def __init__(self, mathml):
        self.__characters = ''
        self.__mathml = mathml
        self.__ns_dict = {'http://www.w3.org/XML/1998/namespace': "xml"}
        self.__raw = False
        self.__fix_soft_hyphens = False


  def startDocument(self):
      pass

  def startElement(self, name, qname):
      print('okay')

  def characters (self, characters): 
    self.__characters += characters


  def startElementNS(self, name, qname, attrs):
        self.__write_text()
        ns = name[0]
        el_name = name[1]
        sys.stdout.write('<')
        if el_name == 'raw':
            if attrs.get((None, 'format')) == 'xml':
                self.__raw = True
        if ns:
            sys.stdout.write('ns1:%s' % el_name)
        else:
            sys.stdout.write(el_name)
        if ns:
            sys.stdout.write(' xmlns:ns1="%s"' % ns)

        the_keys = list(attrs.keys())
        counter = 1
        for the_key in the_keys:
            counter +=1
            ns_att = the_key[0]
            att_name = the_key[1]
            value = attrs[the_key]
            ns_prefix = self.__ns_dict.get(ns_att)
            if ns_att and not ns_prefix:
                sys.stderr.write('No name space for "%s"\n' % (ns_att))
                sys.exit(1)
            if ns_att and ns_prefix == 'xml':
                sys.stdout.write(' xml:%s="%s"' % (att_name, value))
            elif ns_att:
                sys.stderr.write('Sorry, but don\'t know what to do with ns "%s"\n' % (ns_prefix))
                sys.exit(1)
#            if ns_att and ns_att != ns:
#                sys.stdout.write(' xmlns:ns%s="%s"' % (counter,ns_att))
#            if ns_att and ns_att == ns:
#                sys.stdout.write(' ns1:%s="%s"' % (att_name, value))
            else:
                sys.stdout.write(' %s="%s"' % (att_name, value))
        sys.stdout.write('>')

    

  def __write_text(self, raw = False):
        soft_hyphen = chr(173)
        if raw:
            text = self.__characters
        else:
            text =  xml.sax.saxutils.escape(self.__characters)
            if self.__fix_soft_hyphens:
                text = text.replace(soft_hyphen, '-')
        sys.stdout.write(text)
        self.__characters = ''

  def endElementNS(self, name, qname):
        ns = name[0]
        el_name = name[1]
        if (el_name == 'math_block' and  self.__mathml == 'ascii') or (el_name == 'math' and self.__mathml == 'ascii'):
            raw_tree  = asciimathml.parse(self.__characters)[0]
            math_tree = Element('math', title="%s" % self.__characters, xmlns="http://www.w3.org/1998/Math/MathML")
            math_tree.append(raw_tree)
            string_tree = tostring(math_tree, encoding="utf-8") 
            sys.stdout.write(string_tree.decode('utf8'))
            """
            if sys.version_info < (3,):
                print(type(string_tree))
                print()
                sys.stdout.write(string_tree.decode('utf8'))
                # sys.stdout.write(line.encode('utf8'))
            else:
                sys.stdout.write(string_tree.decode())
            """
            self.__characters = ''
        elif (el_name == 'math_block' and  self.__mathml == 'latex') or (el_name == 'math' and self.__mathml == 'latex'):
            raw_tree = self.__tralics()
            if raw_tree == None:
                self.__write_text()
            else:
                raw_tree = raw_tree[0]
                math_tree = Element('math', title="%s" % self.__characters, xmlns="http://www.w3.org/1998/Math/MathML")
                math_tree.append(raw_tree)
                string_tree = tostring(math_tree, encoding="utf-8").decode() 
                sys.stdout.write(string_tree)
                self.__characters = ''
        elif el_name == 'raw' and self.__raw:
            self.__write_text(raw = True)
        else:
            self.__write_text()
        if ns:
            sys.stderr.write('Should not be namespace "%s" here\n' % (ns))
            sys.exit(1)
            sys.stdout.write('</ns1:%s>' % el_name)
        else:
            sys.stdout.write('</%s>' % el_name)

  def __python_latex_math(self):
        """
        Python code seriously broken

        """
        try:
            mathml_tree = docutils.math.latex2mathml.parse_latex_math(self.__characters)
        except SyntaxError:
            return self.__characters
        math_code = ''.join(mathml_tree.xml())
        return math_code

  def __tralics(self):
        num, tex_file = tempfile.mkstemp(suffix='.tex')
        write_obj = open(tex_file, 'w')
        write_obj.write('$')
        write_obj.write(self.__characters)
        write_obj.write('$')
        write_obj.close()
        num, bogus_out = tempfile.mkstemp()
        bogus_out = open(bogus_out, 'w')
        p = subprocess.call(['tralics', '-silent', '-utf8output', '-noentnames', tex_file], stdout=bogus_out)
        bogus_out.close()
        dir_name = os.path.dirname(tex_file)
        filename, ext = os.path.splitext(tex_file)
        xml_file = filename + '.xml'
        log_file = filename + '.log'
        xml_file = os.path.join(dir_name, xml_file)
        if not os.path.isfile(xml_file):
            sys.stderr.write('Cannot find file %s\n"' % xml_file)
            sys.stderr.write('Bug, program now quiting\n')
            sys.exit(1)
        tree = etree.ElementTree()
        read_obj = open(xml_file, 'r')
        xml_tree = tree.parse(xml_file)
        found = None
        while not found:
            for child in xml_tree:
                if child.tag == '{http://www.w3.org/1998/Math/MathML}math':
                    found = 1
                    break
            try:
                xml_tree = xml_tree[0]
            except IndexError:
                sys.stderr.write('Could not find any latex math\n')
                break
        if not found:
            return None
        return xml_tree

class ConverttoMathml:


    def __init__(self):
        pass

    def __parse_args(self):
        desc = """Inserts Mathmx elements into an rst document. 
In order to use the script, first run rs2txml.py on the RST file.
Then run this script on that resulting file
Or, in one pass: rst2xml.py <infile> | python3 rstxml2mathml.py
        """
        parser = argparse.ArgumentParser(description=desc)
        parser.add_argument('--mathml', choices = ['latex', 'ascii'], nargs=1 ) # much better--demand an arg; the option is still optional
        parser.add_argument('in_file', default = sys.stdin, nargs='?',  
                help = 'the file to input; default is standard in')
        args =  parser.parse_args()
        return args


    def convert_to_mathml(self):
        args = self.__parse_args()
        standard_in = False
        in_file = args.in_file
        mathml = args.mathml
        if mathml:
            mathml = mathml[0]
        if not isinstance(in_file, str):
            standard_in = True
            the_string = sys.stdin.read()
        if standard_in:
            read_obj = StringIO(the_string)
        else:
            read_obj = open(in_file, 'r')
        the_handle=CopyTree(mathml)
        parser = xml.sax.make_parser()
        parser.setFeature(feature_namespaces, 1)
        parser.setContentHandler(the_handle)
        parser.setFeature("http://xml.org/sax/features/external-general-entities", True)
        try:
            parser.parse(read_obj)             
        except xml.sax._exceptions.SAXParseException as msg:
            print(str(msg))
            sys.exit(1)
        read_obj.close()

if __name__ == '__main__':
    main_obj = ConverttoMathml()
    main_obj.convert_to_mathml()
