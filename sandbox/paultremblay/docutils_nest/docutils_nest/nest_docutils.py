#!/usr/bin/env python
import sys, os
import docutils_nest.nested_inline
import docutils_nest.read_config
import docutils_nest.options_trem
import docutils_nest.location

"""
converst an XML file to nested tags


"""
outer_dir =  docutils_nest.location.get_location()
doc_nest_dir = os.path.join(outer_dir, '.docutils_nest')


class GetOptions:
    def __init__(self, sys_string):
        self.__sys_string = sys_string

    def get_options(self):
        options_dict = {
            'output':   [1,'o']
        }
        options_obj = docutils_nest.options_trem.ParseOptions(
                                        system_string = self.__sys_string,
                                        options_dict = options_dict,
                                        )

        options, arguments = options_obj.parse_options()
        output = ''
        file = ''
        if options == 0:
            sys.stderr.write('Script will quit because of invalid options')
            sys.exit(1)
        output = options.get('output')
                
        if not output:
            sys.stderr.write('must provide output for script to work\n')
            sys.exit(1)
        if arguments:
            file = arguments[0]

        if not file:
            sys.stderr.write('must provide a file for script to work\n')
            sys.exit(1)
        
        return  file, output

class GetConfig:

    def __init__(self, file):
        self.__file = file

    def get_config(self):
        config_obj = docutils_nest.read_config.Configure(self.__file)
        config_values = config_obj.read_configs()
        return config_values

class MakeTags:
    def __init__(   self, 
                    file, 
                    output,
                    config_values
                    
                    ):
        self.__file = file
        self.__output = output
        self.__start_role = config_values.get('start_role')
        self.__end_role = config_values.get('end_role')
        self.__start_group = config_values.get('start_group')
        self.__end_group = config_values.get('end_group')
        self.__place_of_role = config_values.get('place_of_role')
        self.__tag_name = config_values.get('tag_name')
        self.__warning = config_values.get('warning')

    def make_tags(self):
        convert_obj = docutils_nest.nested_inline.InlineBrackets(
                            file = self.__file, 
                            output = self.__output, 
                            start_role = self.__start_role,
                            end_role = self.__end_role,
                            start_group = self.__start_group,
                            end_group = self.__end_group,
                            place = self.__place_of_role,
                            tag_name = self.__tag_name,
                            warning = self.__warning
                            )
        convert_obj.make_tags()




class NestDocutils:

    def __init__(self, file, output):
        self.__file = file
        self.__output = output

    def nest_tags(self):
        config_file = os.path.join(doc_nest_dir, 'configure.xml')
        config_obj = GetConfig(config_file)
        config_values = config_obj.get_config()
        tags_obj = MakeTags(self.__file, self.__output, config_values)
        tags_obj.make_tags()

if __name__ == '__main__':
    
    # file = '/home/paul/lib/python/paul/restructure_tools/test_inline.xml'
    # output = '/home/paul/paultemp/brackets_to_tags.temp.xml'
    # nest_obj = NestDocutils(file, output)
    # nest_obj.nest_tags()
    # sys.exit(0)
    options_obj = GetOptions(sys.argv)
    file, output = options_obj.get_options()
    config_file = os.path.join(doc_nest_dir, 'configure.xml')
    config_obj = GetConfig(config_file)
    config_values = config_obj.get_config()
    tags_obj = MakeTags(file, output, config_values)
    tags_obj.make_tags()
    command = 'xmlvalid -c -v %s' % output
    os.system(command)

