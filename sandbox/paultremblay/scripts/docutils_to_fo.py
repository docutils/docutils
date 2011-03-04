#! /Library/Frameworks/Python.framework/Versions/2.7/bin/python

#  $Id$

import sys, commands, shlex, subprocess, argparse, ConfigParser, os,  tempfile
import docutilsToFo.rst2xml_lib 
import docutilsToFo.make_stylesheet

def get_args():
    use = 'Converts a file to XSL-FO\n'
    parser = argparse.ArgumentParser(description=use) 
    # parser.add_argument('--xslt', nargs=1, choices = ['xsltproc', 'lxml', 'saxon'], default=['lxml'], 
            # dest='xsl_transform', help = 'choose which processer to use when transforming' ) 
    parser.add_argument('--strict', nargs=1, choices = ['True', 'False'],  
            default = 'True', help = 'whether to quit on errors', dest='strict')
    parser.add_argument('--clean', action="store_const", const=True, 
            help = 'Whether to remove files after test', dest='clean')
    parser.add_argument('--debug', action="store_const", 
            const=True, help = 'print out verbose messages', dest='debug')
    in_file_msg = 'The file to convert. Will either be a path to a file, or standard in.'
    parser.add_argument('in_file', nargs='?', help = in_file_msg, default=sys.stdin)
    parser.add_argument('-s, -stylesheet', nargs=1, help = 'The root stylesheet to use',
            dest='root_stylesheet')
    parser.add_argument('-o, --out', nargs=1, help = 'Path to output result', dest='out_path')
    parser.add_argument('--config', nargs=1, help = 'Path to configuration file', dest='config_file')
    parser.add_argument('--verbose', nargs=1, type = int, help = 'How verbose messaging should be',
            dest='verbose')
    parser.add_argument('--no-fo-valid', action="store_const", 
            const=True, help = 'don\'t validate FO', dest='no_valid_fo')
    return  parser.parse_args()

def parse_config_files(the_paths):
    config = ConfigParser.SafeConfigParser()
    for the_path in the_paths:
        config.read(the_path)
    return config

def read_config_files():
    config_files = []
    if os.environ.get('HOME'):
        config_files.append(os.path.join(os.environ.get('HOME'), '.docutils'))
    config_files.append(os.path.join(os.getcwd(), 'docutils.conf'))
    if arg.config_file:
        config_files.append(arg.config_file[0])
    config = parse_config_files(config_files)
    return config

def get_config_option(the_option, section='FO'):
    try:
        value = config_obj.get(section, the_option)
    except ConfigParser.NoSectionError, error:
        return
    except ConfigParser.NoOptionError, error:
        return
 

arg = get_args()
config_obj = read_config_files()
# get_config_option('strict')
verbose = arg.verbose
if verbose:
    verbose = arg.verbose[0]
else:
    verbose = 2

debug = arg.debug
if debug: verbose = 5
if debug: sys.stderr.write('In debug mode\n')
if debug: sys.stderr.write('script is "%s"\n' % __file__)
in_file = arg.in_file
if debug:
    sys.stderr.write('in_file is "%s"\n' % str(in_file))
config_file = arg.config_file

valid_fo = True
if  arg.no_valid_fo:
    valid_fo = False
root_stylesheet = get_config_option('xsl-stylesheet')
if arg.root_stylesheet: root_stylesheet = arg.root_stylesheet[0]

if debug: sys.stderr.write('root_stylesheet is "%s"\n' % str(root_stylesheet))
out_path = None
if arg.out_path:
    out_path = arg.out_path[0]
else:
    out_xml = tempfile.mkstemp(suffix = '.fo')[1]

# get path to put stylesheet
if out_path:
    filename, ext = os.path.splitext(out_path)
    out_xsl = '%s.xsl' % (filename)
else:
    out_xsl = tempfile.mkstemp(suffix = '.xsl')[1]
if debug:
    sys.stderr.write('out_xsl (file to output XSL stylesheet) is "%s"\n' % (out_xsl))

# make a stylesheet
ss_obj = docutilsToFo.make_stylesheet.ReadConfig(import_ss = root_stylesheet, 
        verbose = verbose, config_file = config_file)
try:
    ss_string = ss_obj.make_stylesheet()
except docutilsToFo.make_stylesheet.FOConfigFileException, msg:
    sys.stderr.write(str(msg))
    sys.stderr.write('\nscript now quitting\n')
    sys.exit(1)
write_obj = file(out_xsl, 'w')
write_obj.write(ss_string)
write_obj.close()

# convert to FO 
error = docutilsToFo.rst2xml_lib.transform_lxml(xslt_file = out_xsl, xml_file = in_file, 
        param_dict = {}, out_file = out_path, verbose = verbose, valid_fo = valid_fo )
if error:
    if type(error) == type(' '):
        sys.stderr.write(error)
    sys.exit(1)


