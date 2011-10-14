#!/usr/bin/python
#   $Id: sax_complete_copy.py 54 2011-04-17 15:44:41Z cynthia $

import xml.sax.handler
from xml.sax.handler import feature_namespaces
import os, sys, argparse,  codecs
from io import StringIO
import asciimathml
import markdown
# from xml.etree.ElementTree import tostring

# don't know if I need these two lines
# sys.stdout = codecs.getwriter("utf-8")(sys.stdout.detach())
# sys.stdin = codecs.getwriter("utf-8")(sys.stdin.detach())

class CopyTree(xml.sax.ContentHandler):


  
  def __init__(self):
        self.__characters = ''
        self.__math_element= True
        self.__ns_dict = {'http://www.w3.org/XML/1998/namespace': "xml"}


  def characters (self, characters): 
    self.__characters += characters


  def startElementNS(self, name, qname, attrs):
        ns = name[0]
        el_name = name[1]
        if el_name == 'math_block':
            self.__math_element= True
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
        if el_name == 'math_block':
            self.__math_element= False
            math_string = '$$ %s $$' % (self.__characters)
            new_tree = markdown.markdown(math_string, ['asciimathml'])
            new_tree = new_tree[3:-4]
            sys.stdout.write(new_tree)
            self.__characters = ''
        else:
            self.__write_text()
        if ns:
            sys.stderr.write('Should not be namespace "%s" here\n' % (ns))
            sys.exit(1)
            sys.stdout.write('</ns1:%s>' % el_name)
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
        # parser.add_argument('--type', nargs=1 ) # much better--demand an arg; the option is still optional
        parser.add_argument('in_file', default = sys.stdin, nargs='?',  
                help = 'the file to input; default is standard in')
        args =  parser.parse_args()
        return args

    def main(self):
        args = self.__parse_args()
        standard_in = False
        in_file = args.in_file
        if str(type(args.in_file)) == "<class '_io.TextIOWrapper'>":
            standard_in = True
            the_string = sys.stdin.read()
        if standard_in:# if a string, make a read obj
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
    main_obj.main()
