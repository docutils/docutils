#!/usr/bin/env python

# If you choose to configure the below by hand, the directory must exist
# and it must contain a valid configuration file named config_rst_2_doc.xml
configure_dir = 'bogus'

import sys, os, tempfile, codecs
import docutils_nest.nest_utils
import docutils_nest.rst_options
import rst_to_docbook.xsl_convert

"""
Module for convert rst documents to docbook
"""

# read input file
     




class ConvertRstToDoc:

    def __init__(self, doc_debug = 0):
        if doc_debug:
            self.__setup_debug()
        else:
            self.__debug = 0
            
        self.__setup_config_dir()

    def __setup_config_dir(self):
        library_dir = rst_to_docbook.xsl_convert.__file__
        library_dir = os.path.dirname(library_dir)
        module_name = os.path.join(library_dir, 'convert_rst_to_docbook.py')
        no_config = ('sorry, but configuration file does not exist\n'
                            'script will now quit\n')
                    

        if not configure_dir:
            platform = sys.platform
            index = platform.find('linux')
            if index > -1:
                home_dir =  os.path.expanduser("~")
                rst_docbook_dir = os.path.join(home_dir, '.rst_to_docbook')
                dir_exists = os.path.isdir(rst_docbook_dir)
                if not dir_exists:
                    sys.stderr.write('sorry, but I could not find a '
                            'a directory in your home directory entitled '
                            '".rst_to_docbook. Create this directory and '
                            'make a copy of the configuration file.\n'
                            'Script will now quit\n'
                            )
                    os.system.exit(1)
                config_file = os.path.join(home_dir, 'config_rst_2_doc.xml')
                if not os.path.exists(config_file):
                    sys.stderr.write(no_config)
                    
            else:
                sys.stderr.write('sorry, don\'t know how to find configure file '
                'for %s.\n' 
                'Try configuring this file (%s) by hand.\n'
                
                
                % (platform, module_name))
                sys.exit(1)
        else:
            
            config_file = os.path.join(configure_dir, 'config_rst_2_doc.xml')
            if not os.path.exists(config_file):
                sys.stderr.write(no_config)
                sys.exit(1)
        
    def __setup_debug(self):
        platform = sys.platform
        index = platform.find('linux')
        if index > -1:
            home_dir =  os.path.expanduser("~")
            rst_docbook_dir = os.path.join(home_dir, '.rst_to_docbook')
            dir_exists = os.path.isdir(rst_docbook_dir)
            if not dir_exists:
                os.mkdir(rst_docbook_dir)
            self.__debug = 1
            self.__debug_dir = rst_docbook_dir
            return
        sys.stderr.write('sorry, don\'t know how to make debug directory '
                'for %s' % platform)


    def convert_to_docbook(self):
        # get file, output, and the docutils_options
        file, output, docutils_options =  self.__handle_options()
        docutils_nest_file = tempfile.mktemp()
        self.__convert_to_nest_utils(   file, 
                                        output = docutils_nest_file, 
                                        docutils_options = docutils_options)
        self.__copy_file(docutils_nest_file, output)
        if self.__debug:
            new_file = os.path.join(self.__debug_dir, 'converted_to_nest_utils_info')
            self.__copy_file(docutils_nest_file, new_file)
            
        os.remove(docutils_nest_file)

    
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
        convert_obj = docutils_nest.nest_utils.RstWithInline(file, output, docutils_opts = docutils_options)
        convert_obj.convert()

    def __convert_args(self, file, output):
        pass
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
        sys.argv.append('--doc_debug')
        sys.argv.append('--output')
        sys.argv.append('output.xml')
        sys.argv.append('%s' % file)
    test_obj = ConvertRstToDoc(doc_debug = 1)
    test_obj.convert_to_docbook()
