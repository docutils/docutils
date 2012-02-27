#!/usr/bin/python
#   $Id: sax_complete_copy.py 54 2011-04-17 15:44:41Z cynthia $

import os, sys, argparse, io
import xml.sax.handler
from xml.sax.handler import feature_namespaces
from StringIO import StringIO


import asciitomathml.asciitomathml 
from xml.etree.ElementTree import Element, tostring
import xml.etree.cElementTree as etree
import tempfile, subprocess, os
from xml.sax import InputSource

class InvaidXml(Exception):
    pass


class FixTree(xml.sax.ContentHandler):
  
  def __init__(self, mathml=True, raw_xml=True):
        self.__characters = ''
        self.__mathml = mathml
        self.__raw_xml = raw_xml
        self.__write_raw = False
        self.__ns_dict = {'http://www.w3.org/XML/1998/namespace': "xml"}


  def startDocument(self):
      pass


  def characters (self, characters): 
    self.__characters += characters


  def startElementNS(self, name, qname, attrs):
        self.__write_text()
        ns = name[0]
        el_name = name[1]
        sys.stdout.write('<')
        if el_name == 'raw':
            if attrs.get((None, 'format')) == 'xml' and self.__raw_xml:
                self.__write_raw = True
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
                raise InvaidXml('No namespace for "%s"\n' % (ns_att))
            if ns_att and ns_prefix == 'xml':
                sys.stdout.write(' xml:%s="%s"' % (att_name, value))
            elif ns_att:
                raise InvaidXml('Sorry, but don\'t know what to do with ns "%s"\n' % (ns_prefix))
            else:
                sys.stdout.write(' %s="%s"' % (att_name.encode('utf8'), value.encode('utf8')))
        sys.stdout.write('>')

    

  def __write_text(self, raw = False):
        if raw:
            ff
            text = self.__characters
        else:
            text =  xml.sax.saxutils.escape(self.__characters)
        sys.stdout.write(text.encode('utf8'))
        self.__characters = ''

  def endElementNS(self, name, qname):
        ns = name[0]
        el_name = name[1]
        if (el_name == 'math_block' and  self.__mathml) or (el_name == 'math' and self.__mathml) :
            # math_obj =  asciitomathml.asciitomathml.AsciiMathML()
            if el_name == 'math_block':
                math_obj =  asciitomathml.asciitomathml.AsciiMathML(mstyle={'displaystyle':'true'})
            else:
                math_obj =  asciitomathml.asciitomathml.AsciiMathML()
            math_obj.parse_string(self.__characters)
            math_tree = math_obj.get_tree()
            math_string = tostring(math_tree, encoding="us-ascii")
            sys.stdout.write(math_string)
            self.__characters = ''
        elif el_name == 'raw' and self.__write_raw:
            self.__write_text(raw = True)
            self.__write_raw = False
        else:
            self.__write_text()
        if ns:
            raise InvaidXml('Should not be namespace "%s" here\n' % (ns))
        else:
            sys.stdout.write('</%s>' % el_name)



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
        parser.add_argument('--no-mathml', action="store_const", const=True, dest='no_mathml')
        parser.add_argument('--no-rawxml', action="store_const", const=True, dest='no_rawxml')
        parser.add_argument('--fix-sh', action="store_const", const=True, dest='fix_sh', default=False)
        parser.add_argument('in_file', default = sys.stdin, nargs='?',  
                help = 'the file to input; default is standard in')
        args =  parser.parse_args()
        return args


    def convert_to_mathml(self):
        args = self.__parse_args()
        standard_in = False
        in_file = args.in_file
        no_mathml = args.no_mathml
        if no_mathml:
            mathml = False
        else:
            mathml = True
        no_rawxml = args.no_rawxml
        if no_rawxml:
            raw_xml = False
        else:
            raw_xml = True
        fix_sh = args.fix_sh
        if not isinstance(in_file, str):
            standard_in = True
            the_string = sys.stdin.read()
        if standard_in:
            read_obj = StringIO(the_string)
        else:
            read_obj = open(in_file, 'r')
        the_handle=FixTree(mathml= mathml, raw_xml = raw_xml)
        parser = xml.sax.make_parser()
        parser.setFeature(feature_namespaces, 1)
        parser.setContentHandler(the_handle)
        parser.setFeature("http://xml.org/sax/features/external-general-entities", True)
        try:
            parser.parse(read_obj)             
        except xml.sax._exceptions.SAXParseException as error:
            msg = error.args[0]
            raise InvaidXml(msg)
        except InvaidXml as error:
            msg = error.args[0]
            raise InvaidXml(msg)
        read_obj.close()

if __name__ == '__main__':
    main_obj = ConverttoMathml()
    main_obj.convert_to_mathml()
