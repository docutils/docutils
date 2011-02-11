#! /Library/Frameworks/Python.framework/Versions/2.7/bin/python

#  $Id$

import sys, commands, shlex, subprocess, argparse, ConfigParser, os,  tempfile
import docutilsToFo.rst2xml_lib 
import docutilsToFo.make_stylesheet

def get_args():
    parser = argparse.ArgumentParser(description='Converts a file to XSLFO') 
    parser.add_argument('--xslt', nargs=1, choices = ['xsltproc', 'lxml', 'saxon'], default=['lxml'], 
            dest='xsl_transform', help = 'choose which processer to use when transforming' ) 
    parser.add_argument('--strict', nargs=1, choices = ['True', 'False'],  
            default = 'True', help = 'whether to quit on errors', dest='strict')
    parser.add_argument('--clean', action="store_const", const=True, help = 'Whether to remove files after test', dest='clean')
    parser.add_argument('in_path', nargs=1, help = 'The file to convert')
    parser.add_argument('-s, -stylesheet', nargs=1, help = 'The root stylesheet to use',
            dest='root_stylesheet')
    parser.add_argument('-o, --out', nargs=1, help = 'Path to output result', dest='out_path')
    return  parser.parse_args()

 


home= os.environ.get('HOME')
home_config_file = os.path.join(home, '.docutils')
cwd = os.getcwd()
project_config_file = os.path.join(cwd, '.docutils.conf')
config = ConfigParser.SafeConfigParser()
config.read([home_config_file, project_config_file])


arg = get_args()

if arg.root_stylesheet:
    root_stylesheet = arg.root_stylesheet[0]
else:
    try:
        root_stylesheet = config.get('FO', 'xsl-stylesheet')
    except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
        sys.stderr.write('No stylesheet found.\n')
        sys.stderr.write('Script quiting.\n')
        sys.exit(1)



out_path = None
in_path = arg.in_path[0]
if arg.out_path:
    out_path = arg.out_path[0]
    # filename, ext = os.path.splitext(out_path)
    # out_xml = '%s.xml' % (filename)
else:
    out_xml = tempfile.mkstemp(suffix = '.fo')[1]

# convert to XML
# docutilsToFo.rst2xml_lib.publish_xml_cmdline(in_path = in_path, out_path = out_xml)

# get path to put stylesheet
if out_path:
    filename, ext = os.path.splitext(out_path)
    out_xsl = '%s.xsl' % (filename)
else:
    out_xsl = tempfile.mkstemp(suffix = '.xsl')[1]

# make a stylesheet
ss_obj = docutilsToFo.make_stylesheet.ReadConfig(import_ss = root_stylesheet )
ss_string = ss_obj.make_stylesheet()
write_obj = file(out_xsl, 'w')
write_obj.write(ss_string)
write_obj.close()

# convert to FO 
error = docutilsToFo.rst2xml_lib.transform_lxml(xslt_file = out_xsl, xml_file = in_path, 
        param_dict = {}, out_file = out_path )
if error:
    sys.stderr.write(error)
    sys.exit(1)


