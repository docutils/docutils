#!/usr/bin/env python

import sys, os
import docutils_nest.nest_utils
import docutils_nest.rst_options


def convert_file():
    options_dict = {
        'output':       [1]
    }
    opt_obj = docutils_nest.rst_options.OptionsRst(sys.argv, options_dict)
    inline_opts, args, doc_opts = opt_obj.parse_options()
    if inline_opts == 0:
        print_help_message()
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
    convert_obj = docutils_nest.nest_utils.RstWithInline(file, output, docutils_opts = doc_opts)
    convert_obj.convert()
    
if __name__ == '__main__':
    convert_file()


