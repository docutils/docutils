#!/usr/bin/env python 

# configure rst_2_dbk_dir by hand
configure_dir = ''

import sys, os, tempfile, codecs
import docutils_nest.nest_utils
import docutils_nest.rst_options
import rst_to_docbook.xsl_convert, rst_to_docbook.location
import rst_to_docbook.read_config

"""
Module for convert rst documents to docbook
"""

# read input file
     




class ConvertRstToDoc:

    def __init__(self, doc_debug = 0):
        outer_dir =  rst_to_docbook.location.get_location()
        self.__rst_2_dbk_dir = os.path.join(outer_dir, '.rst_to_docbook')
        config_file = os.path.join(self.__rst_2_dbk_dir, 'configure.xml')
        self.__xslt_processor = self.__get_configs(config_file)
        if doc_debug:
            self.__setup_debug()
        else:
            self.__debug = 0
            

        
    def __setup_debug(self):
        self.__debug_dir = os.path.join(self.__rst_2_dbk_dir, 'debug')
        if not (os.path.isdir(self.__debug_dir)):
            os.mkdir(self.__debug_dir)
        list_of_files = os.listdir(self.__debug_dir)
        sys.stdout.write('Removing files from %s...\n' % self.__debug_dir)
        for file in list_of_files:
            file = os.path.join(self.__debug_dir, file)
            sys.stdout.write('%s\n' % file)
            os.remove(file)
        self.__debug = 1


    def __get_configs(self, config_file):
        config_obj = rst_to_docbook.read_config.Configure(config_file)
        config_dict = config_obj.read_configs()
        processor = config_dict.get('xslt-processor')
        processor = processor[0]
        return processor

        
    def convert_to_docbook(self):
        # get file, output, and the docutils_options
        file, output, docutils_options =  self.__handle_options()
        docutils_nest_file = tempfile.mktemp()
        main_temp_file = tempfile.mktemp()
        self.__convert_to_nest_utils(   file, 
                                        output = docutils_nest_file, 
                                        docutils_options = docutils_options)
        self.__copy_file(docutils_nest_file, main_temp_file)
        if self.__debug:
            new_file = os.path.join(self.__debug_dir, 'converted_to_nest_utils_info')
            self.__copy_file(docutils_nest_file, new_file)
            
        os.remove(docutils_nest_file)

        # convert with first xslt
        converted_string_file = tempfile.mktemp()
        self.__string_to_attributes(main_temp_file, converted_string_file)
        self.__copy_file(converted_string_file, main_temp_file)
        if self.__debug:
            new_file = os.path.join(self.__debug_dir, 'converted_string_info')
            self.__copy_file(converted_string_file, new_file)
        os.remove(converted_string_file)

        # convert with second xslt
        converted_arg_file = tempfile.mktemp()
        self.__convert_args(main_temp_file, converted_arg_file)
        self.__copy_file(converted_arg_file, main_temp_file)
        if self.__debug:
            new_file = os.path.join(self.__debug_dir, 'converted_to_args_info')
            self.__copy_file(converted_arg_file, new_file)
        os.remove(converted_arg_file)

        # final convert with xslt
        docbook_file = tempfile.mktemp()
        self.__xsl_convert_to_docbook(main_temp_file, docbook_file)
        self.__copy_file(docbook_file, main_temp_file)
        if self.__debug:
            new_file = os.path.join(self.__debug_dir, 'converted_to_docbook_info')
            self.__copy_file(docbook_file, new_file)
        os.remove(docbook_file)

        # write to output
        self.__copy_file(main_temp_file, output)
        os.remove(main_temp_file)

    
    def __handle_options(self):
        options_dict = {
            'output':       [1],
            'doc_debug':        [],
        }
        opt_obj = docutils_nest.rst_options.OptionsRst(sys.argv, options_dict)
        inline_opts, args, doc_opts = opt_obj.parse_options()
        if inline_opts == 0:
            self.__print_help_message()
            sys.stderr.write('Script will now quite because of illegal options\n')
            sys.exit(1)
        try:
            file = args[0]
        except IndexError:
            sys.stderr.write('You must provide a file for the script to convert.\n'
                    'Script will now quit'
                    )
            sys.exit(1)
        output = inline_opts.get('output')
        if not output:
            sys.stderr.write('You must provide an output option for the script to work\n'
                    'Script will now quit\n'
                    )
            sys.exit(1)
        return file, output, doc_opts
    
    def __convert_to_nest_utils(self, file, output, docutils_options):

        sys.stdout.write('converting to nest-utils.xml ...\n')
        convert_obj = docutils_nest.nest_utils.RstWithInline(file, output, docutils_opts = docutils_options)
        convert_obj.convert()

    def __string_to_attributes(self, file, output):
        xsl_file = os.path.join(self.__rst_2_dbk_dir, 
            'xslt_stylesheets', 'reStruct_field_names_tokenize.xsl'
                )
        sys.stdout.write('converting string in nest-utils '
            'with xslt...\n')
        trans_obj =  rst_to_docbook.xsl_convert.XslConvert(processor = self.__xslt_processor)
        trans_obj.transform(file = file, 
                            xsl_file = xsl_file, 
                            output = output)
    
    def __convert_args(self, file, output):
        xsl_file = os.path.join(self.__rst_2_dbk_dir, 
            'xslt_stylesheets', 'reStruct_field_names.xsl'
                )
        sys.stdout.write('converting args in nest-utils '
            'with xslt...\n')
        trans_obj =  rst_to_docbook.xsl_convert.XslConvert(processor = self.__xslt_processor)
        trans_obj.transform(file = file, 
                            xsl_file = xsl_file, 
                            output = output)
    def __xsl_convert_to_docbook(self, file, output):
        sys.stdout.write('doing final converstion with xslt...\n')
        xsl_file = os.path.join(self.__rst_2_dbk_dir,
            'xslt_stylesheets', 'reStructure_to_docbook.xsl'
                )
         # reStructure_to_docbook.xsl
        trans_obj =  rst_to_docbook.xsl_convert.XslConvert(processor = self.__xslt_processor)
        trans_obj.transform(file = file, 
                            xsl_file = xsl_file, 
                            output = output)
        
    def __copy_file(self, file, output):
        (utf8_encode, utf8_decode, utf8_reader, utf8_writer) = codecs.lookup("utf-8")
        write_obj = utf8_writer(open(output, 'w'))
        read_obj = open(file, 'r')
        line = 1
        while line:
            line = read_obj.readline()
            write_obj.write(line)
        read_obj.close()
        write_obj.close()

    def __print_help_message(self):
        sys.stderr.write('script will now quit')
        sys.exit(1)
    
if __name__ == '__main__':
    if len(sys.argv) == 1:
        file = '/home/paul/Documents/in_progress/cvs/sandbox/paultremblay/rst_to_docbook/test_files/test_simple.rst'
        file = '/home/paul/Documents/in_progress/cvs/sandbox/paultremblay/rst_to_docbook/test_files/reStructure_docbook_example.rst'
        sys.argv.append('--indents')
        sys.argv.append('--doc_debug')
        sys.argv.append('--output')
        sys.argv.append('output.xml')
        sys.argv.append('%s' % file)
    test_obj = ConvertRstToDoc(doc_debug = 1)
    test_obj.convert_to_docbook()
    
