"""

Parse options from the command line in order to convert an rst text to XML.

This script separates the options a script needs from the options that docuitls needs.

Takes a system string. Returns a dictionary of options for docutils inline, a list of argumetns for docutils-inline, and a string for docutils.

"""

import sys, os
import options_trem

class OptionsRst:

    def __init__(self, sys_argv, script_options):


        self.__sys_argv = sys_argv
        self.__rst_options_dict = {
            'pep-references':      [],
            'rfc-references':         [],
            'tab-width':              [1],
            'newlines':               [],
            'indents':                [],
            'no-xml-declaration':     [],
            'no-doctype':             [],
            'generator':              [0,'g'],
            'no-generator':           [],
            'date':                   [0,'d'],
            'time':                   [0,'t'],
            'no-datestamp':           [],
            'source-link':            [0, 's'],
            'source-url':             [1],
            'no-source-link':         [],
            'toc-entry-backlinks':    [],
            'toc-top-backlinks':      [],
            'no-toc-backlinks':       [],
            'footnote-backlinks':     [],
            'no-footnote-backlinks':  [],
            'report':                 [1, 'r'],
            'verbose':                [0,'v'],
            'quiet':                  [0, 'q'],
            'halt':                    [1],
            'strict':                 [],
            'debug':                  [],
            'no-debug':               [],
            'warnings':               [],
            'input-encoding':         [1, 'i'], 
            'output-encoding':        [1, 'o'],
            'language':               [1, 'l'],
            'config':                 [1],
            'version':                [0, 'V'],
            'help':                   [0, 'h'],
            }


        self.__inline_options_dict = script_options
        self.__rst_options_list = self.__rst_options_dict.keys()
        self.__inline_options_list = self.__inline_options_dict.keys()

        self.__both_options = {}
        self.__both_options.update(self.__rst_options_dict)
        self.__both_options.update(self.__inline_options_dict)

    def parse_options(self):
        opts, args = self.__get_raw_options()
        if opts == 0:
            return 0, 0, 0
        doc_opts, inline_opts = self.__split_options(opts)
        return inline_opts, args, doc_opts
    
    def __get_raw_options(self):
        raw_obj = options_trem.ParseOptions(
                options_dict = self.__both_options,
                system_string = self.__sys_argv
                )

        options, arguments = raw_obj.parse_options()
        return options, arguments

    def __split_options(self, options):
        inline_options = {}
        docutils_options = []
        keys = options.keys()
        for key in keys:
            if key in self.__rst_options_list:
                if options[key] == None:
                    docutils_options.append('--%s' % key)
                else:
                    docutils_options.append('--%s=%s' % (key, options[key]))

            else:
                inline_options[key] = options[key]

        return docutils_options, inline_options
            
        
        

if __name__ == '__main__':
    this_options_dict = {
            'output':               [1],
        }
    test_obj = OptionsRst(sys.argv, this_options_dict)
    test_obj.parse_options()
