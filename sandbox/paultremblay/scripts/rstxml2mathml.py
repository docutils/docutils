#!/usr/bin/python
#   $Id: sax_complete_copy.py 54 2011-04-17 15:44:41Z cynthia $

import xml.sax.handler
from xml.sax.handler import feature_namespaces
import os, sys, argparse
from io import StringIO
import asciimathml
from xml.etree.ElementTree import Element, tostring
import tempfile, subprocess, os

try:
  from lxml import etree
except ImportError:
  try:
    import xml.etree.cElementTree as etree
  except ImportError:
    try:
      import xml.etree.ElementTree as etree
    except ImportError:
      try:
        import cElementTree as etree
      except ImportError:
        try:
          import elementtree.ElementTree as etree
        except ImportError:
          print("Failed to import ElementTree from any known place")
          sys.exit(1)


class CopyTree(xml.sax.ContentHandler):


  
  def __init__(self):
        self.__characters = ''
        self.__ascii_math = False
        self.__latex_math = False
        self.__ns_dict = {'http://www.w3.org/XML/1998/namespace': "xml"}


  def characters (self, characters): 
    self.__characters += characters


  def startElementNS(self, name, qname, attrs):
        ns = name[0]
        el_name = name[1]
        if el_name == 'math_block' and attrs.get((None, 'classes')) == 'asciimath':
            self.__ascii_math= True
        if el_name == 'math_block' and attrs.get((None, 'classes')) == 'latex':
            self.__latex_math= True
        sys.stdout.write('<')
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

    

  def __write_text(self):
        text =  xml.sax.saxutils.escape(self.__characters)
        sys.stdout.write(text)
        self.__characters = ''

  def endElementNS(self, name, qname):
        ns = name[0]
        el_name = name[1]
        if el_name == 'math_block' and  self.__ascii_math == True:
            self.__ascii_math= False
            raw_tree  = asciimathml.parse(self.__characters)[0]
            math_tree = Element('math', title="%s" % self.__characters, xmlns="http://www.w3.org/1998/Math/MathML")
            math_tree.append(raw_tree)
            string_tree = tostring(math_tree, encoding="utf-8").decode() 
            sys.stdout.write(string_tree)
            self.__characters = ''
        elif el_name == 'math_block' and  self.__latex_math == True:
            raw_tree = self.__tralics()[0]
            math_tree = Element('math', title="%s" % self.__characters, xmlns="http://www.w3.org/1998/Math/MathML")
            math_tree.append(raw_tree)
            string_tree = tostring(math_tree, encoding="utf-8").decode() 
            sys.stdout.write(string_tree)
            self.__characters = ''
            self.__latex_math = False
        else:
            self.__write_text()
        if ns:
            sys.stderr.write('Should not be namespace "%s" here\n' % (ns))
            sys.exit(1)
            sys.stdout.write('</ns1:%s>' % el_name)
        else:
            sys.stdout.write('</%s>' % el_name)

  def __tralics(self):
        num, tex_file = tempfile.mkstemp(suffix='.tex')
        write_obj = open(tex_file, 'w')
        write_obj.write(self.__characters)
        write_obj.close()
        num, bogus_out = tempfile.mkstemp()
        bogus_out = open(bogus_out, 'w')
        p = subprocess.call(['tralics', '-silent', '-utf8output', tex_file], stdout=bogus_out)
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
        xml_tree = tree.parse(xml_file)
        found = None
        while not found:
            for child in xml_tree:
                if child.tag == '{http://www.w3.org/1998/Math/MathML}math':
                    found = 1
                    break
            xml_tree = xml_tree[0]
        return xml_tree
        """
        line_to_read = 1
        xml_string = None
        while line_to_read:
            line_to_read = read_obj.readline()
            line = line_to_read
            search_obj = regexp.search(line)
            if search_obj:
                xml_string = (search_obj.group(0))
        read_obj.close()
        if xml_string == None:
            sys.stderr.write("found no math string\n")
            sys.stderr.write('Bug, not quiting\n')
        return xml_string
        """

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
        # parser.add_argument('--type', nargs=1 ) # much better--demand an arg; the option is still optional
        parser.add_argument('in_file', default = sys.stdin, nargs='?',  
                help = 'the file to input; default is standard in')
        args =  parser.parse_args()
        return args

    def convert_to_mathml(self):
        import io
        args = self.__parse_args()
        standard_in = False
        in_file = args.in_file
        if isinstance(in_file, io.TextIOWrapper):
            standard_in = True
            the_string = sys.stdin.read()
        if standard_in:
            read_obj = StringIO(the_string)
        else:
            read_obj = open(in_file, 'r')
        the_handle=CopyTree()
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
