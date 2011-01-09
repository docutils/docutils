#!/usr/bin/env python
import sys, os
import rst_to_docbook.convert_rst_to_docbook
import rst_to_docbook.options_trem

class Convert:

    def __init__(self):
        pass

    def convert(self):
        debug_doc = self.__parse_options()
        
        convert_obj = rst_to_docbook.convert_rst_to_docbook.ConvertRstToDoc(doc_debug = debug_doc)
        convert_obj.convert_to_docbook()

    def __parse_options(self):
        if '--debug-doc' in sys.argv:
            sys.argv.remove('--debug-doc')
            return 1
        return 0

if __name__ == '__main__':
    execute_obj = Convert()
    execute_obj.convert()
