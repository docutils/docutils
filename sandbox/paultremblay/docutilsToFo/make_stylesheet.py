#! /Library/Frameworks/Python.framework/Versions/2.7/bin/python
#  $Id$
import sys, copy, getopt, os, ConfigParser
from docutils_fo_dicts import *

class WriteStylesheet:

    def __init__(self):
        self.__string = ''
        pass

    def __write_start_element(self, name, atts):
        pass

    def __write_end_element(self, name):
        pass

    def __write_root_start(self):
        self.__string +=  """<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >\n\n"""
        pass

    def __write_root_end(self):
        self.__string +='</xsl:stylesheet>'

    def __write_att_sets(self):
        att_sets = self.__att_sets.keys()
        for att_set in att_sets:
            self.__string += '     <xsl:attribute-set name="%s">\n' % (att_set)
            att_dict = self.__att_sets[att_set]
            atts = att_dict.keys()
            for att in atts:
                self.__string += '          <xsl:attribute name="%s">' % (att)
                self.__string += att_dict[att]
                self.__string += '</xsl:attribute>\n' 

            self.__string += '     </xsl:attribute-set>\n\n'

    def __write_import(self):
        self.__string += """     <xsl:import href= "%s"/>\n\n""" % (self.__import_ss)

    def __write_params(self):
        pass

    def write_stylesheet(self, import_ss, params, att_sets, out=None):
        self.__import_ss = import_ss
        self.__params = params
        self.__att_sets = att_sets
        self.__out = out
        self.__write_root_start()
        self.__write_import()
        self.__write_params()
        self.__write_att_sets()
        self.__write_root_end()
        return self.__string

class ReadConfig:

    def __init__(self, import_ss):
        self.__attribute_sets = {}
        self.__params = {}
        self.__import_ss = import_ss

    def write_config_file(self, dest=None):
        w = WriteStylesheet()
        ss_string = w.write_stylesheet(import_ss = self.__import_ss, params = self.__params, 
                att_sets = self.__attribute_sets)
        return ss_string

    def read_config_file(self):
        opts_dict = {}
        home= os.environ.get('HOME')
        home_config_file = os.path.join(home, '.docutils')
        if os.path.isfile(home_config_file):
            opts_dict_home = self.parse_config_file(home_config_file)
            opts_dict.update(opts_dict_home)
        cwd = os.getcwd()
        project_config_file = os.path.join(cwd, '.docutils.conf')
        if os.path.isfile(project_config_file):
            opts_dict_project = self.parse_config_file(project_config_file)
            opts_dict.update(opts_dict_project)
        return opts_dict

    def parse_config_file(self, the_path):
        config = ConfigParser.SafeConfigParser()
        config.read(the_path)
        if not 'FO' in config.sections():
            return
        opts =  config.items('FO')
        opts_dict = {}
        for pair_tupple in opts:
            first = pair_tupple[0]
            second = pair_tupple[1]
            fields = first.split('.', 1)
            if len(fields) == 2:
                self.__handle_attributes(fields[0], fields[1], second)
        return opts_dict

    def __handle_attributes(self, set, att, value):
        name_type_pair = att_set_dict.get(set)
        if name_type_pair: # found a valid att-set
            true_name = name_type_pair[0] # the true name as found in the stylesheet
            the_type = name_type_pair[1]
            att_true_value = which_dict.get(the_type).get(att)
            if not att_true_value:
                self.__error('%s not a valid value for att-set %s' % (att, set))
            elif type(att_true_value) == type([]):
                att_list = self.__handle_multiple_att(att, att_true_value, value)
                if att_list:
                    for item in att_list:
                        self.__add_attribute(true_name, item[0], item[1] )
            else:
                self.__add_attribute(true_name, att_true_value, value )
        else:
            self.__error('%s not a valid attribute-set' % (set))

    def __add_attribute(self, att_set, att, value):
        att_exists =  self.__attribute_sets.get(att_set)
        if not att_exists:
            self.__attribute_sets[att_set] = {}
        self.__attribute_sets[att_set][att] = value

    def __error(self, msg):
        sys.stderr.write(msg)
        sys.stderr.write('\n')

    def __handle_multiple_att(self, att, the_list, value):
        if att == 'font-style':
            att_list = font_style_dict.get(value)
            if not att_list:
                self.__error('%s not a valid value for att-set %s' % (value, att))
            else:
                return att_list

    def print_att_list(self):
        print self.__attribute_sets

    def main(self):
        self.read_config_file()
        ss_string = self.write_config_file()
        return ss_string
        # self.print_att_list()


if __name__ == '__main__':
    read_config_obj =  ReadConfig(import_ss = '/Users/cynthia/tmp/paultremblay/xsl_fo/docutils_to_fo.xsl' )
    ss_string = read_config_obj.main()
    print ss_string
