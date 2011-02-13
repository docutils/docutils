#! /Library/Frameworks/Python.framework/Versions/2.7/bin/python
import sys, os, argparse
import docutilsToFo.rst2xml_lib

parser = argparse.ArgumentParser(description='validate docutils document') 
parser.add_argument('xml_file', nargs=1, help = 'file to validate')
args = parser.parse_args()
xml_file = args.xml_file[0]

error = docutilsToFo.rst2xml_lib.validate_fo_xsl(xml_file)
if error:
    sys.exit(1)
