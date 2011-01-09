#! /Library/Frameworks/Python.framework/Versions/2.7/bin/python
#  $Id$
import sys, copy, getopt, os, StringIO, ConfigParser, commands, shlex, subprocess
from lxml import etree
import lxml

def transform_xslt_as_string(inxsltfile, inxmlfile):
    xslt_doc = etree.parse(inxsltfile)
    try:
        transform = etree.XSLT(xslt_doc)
    except lxml.etree.XSLTParseError, error:
        raise Rst2foException, str(error)
    indoc = etree.parse(inxmlfile)
    paramdict = {'param1': 'value1', 'param2': 'value2'}
    try:
        outdoc = transform(indoc, **paramdict)
    except lxml.etree.XSLTApplyError, error:
        raise Rst2foException, str(error)
    return outdoc



def transform(xslt_file, xml_file, param_dict):
    xslt_doc = etree.parse(xslt_file)
    try:
        transform = etree.XSLT(xslt_doc)
    except lxml.etree.XSLTParseError, error:
        raise Rst2foException, str(error)
    indoc = etree.parse(xml_file)
    try:
        outdoc = transform(indoc, **param_dict)
    except lxml.etree.XSLTApplyError, error:
        raise Rst2foException, str(error)
    return outdoc


def print_result(input_obj, out_path = None):
    if out_path:
        write_obj = open(out_path, 'w')
        input_obj.write(write_obj)
        write_obj.close()
    else:
        sys.stdout.write(str(input_obj))


def get_xslt():
    """
    just an example of how to create a stylesheet as a string

    """
    string_xslt="""<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    version="1.1"
>

    <xsl:output method="xml" encoding="UTF-8"/>


    <xsl:template match="@*|node()">
        <xsl:copy> 
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="comment()">
        <xsl:comment>
            <xsl:value-of select="."/>
        </xsl:comment>
    </xsl:template>

</xsl:stylesheet>
"""
    return  StringIO.StringIO(string_xslt)

def help():
    print "rst2fo.py --stylesheet --<file>"


def parse_config_file(the_path):
    opts_dict = {}
    config = ConfigParser.SafeConfigParser()
    config.read(the_path)
    try:
        xsltproc = config.getboolean('FO', 'xsltproc')
        opts_dict['use_xsltproc'] = xsltproc
    except ConfigParser.NoSectionError:
        pass
    try:
        xsl_stylesheet = config.get('FO', 'xsl-stylesheet')
        opts_dict['xsl_stylesheet'] = xsl_stylesheet
    except ConfigParser.NoSectionError:
        pass
    except ConfigParser.NoOptionError:
        pass
    return opts_dict


def read_config_file():
    opts_dict = {}
    home= os.environ.get('HOME')
    home_config_file = os.path.join(home, '.docutils')
    if os.path.isfile(home_config_file):
        opts_dict_home = parse_config_file(home_config_file)
        opts_dict.update(opts_dict_home)
    cwd = os.getcwd()
    project_config_file = os.path.join(cwd, '.docutils.conf')
    if os.path.isfile(project_config_file):
        opts_dict_project = parse_config_file(project_config_file)
        opts_dict.update(opts_dict_project)
    return opts_dict


def get_options():
    opts_dict = read_config_file()
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hc', ['help', 'fo-template=', 'xsltproc' ])
    except getopt.GetoptError, error:
        sys.stderr.write('rst2fo.py: Can\'t parse command line: %s' % str(error))
        help()
        sys.exit(1)
    for option, arg_in_opt in opts:
        if option in ('-h', '--help'):
            help()
            sys.exit(0)
        if option in ('--fo-template'):
            fo_template = arg_in_opt
            opts_dict['fo_template']= arg_in_opt
        if option in ('--xsltproc','-c'):
            opts_dict['use_xsltproc'] = True
    return opts_dict, args


def main():
    the_opts_dict, the_args = get_options()
    if the_args:
        xml_file = the_args[0]
    else:
        # line = unicode(sys.stdin.read(), "UTF-8")
        line = sys.stdin.read()
        xml_file = StringIO.StringIO(line)
    convert_obj = paultremblay.rst2fo_lib.Convert()
    xslt_stylesheet = the_opts_dict.get('xsl_stylesheet')
    if not xslt_stylesheet:
        xslt_stylesheet_dir = convert_obj.get_path_of_xsl_dir()
        xslt_stylesheet = os.path.join(xslt_stylesheet_dir, 'docutils_to_fop.xsl')
    param_dict = {}
    fo_template = the_opts_dict.get('fo_template')
    if fo_template:
        param_dict['fo-template'] = "'%s'" % fo_template
    use_command_line = the_opts_dict.get('use_xsltproc')
    if use_command_line: # a pure hack to be done away with!
        command = 'xsltproc %s' % (xslt_stylesheet)
        if not the_args:
            temp_file = 'temp_as_command.xml'
            write_obj = file(temp_file, 'w')
            line_to_read = 1
            while line_to_read:
                line_to_read = xml_file.readline()
                line = line_to_read
                write_obj.write(line)
            write_obj.close()
            command = '%s %s' % (command, temp_file)
        else:
            command = '%s %s' % (command, xml_file)

        # status, output = commands.getstatusoutput(command)
        args = shlex.split(command)
        subprocess.Popen(args)
        
    else:
        out_doc = convert_obj.transform(xslt_stylesheet, xml_file, param_dict)
        convert_obj.print_result(out_doc)

main()
