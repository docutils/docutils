# /usr/bin/env python 

import sys, os
import options_trem

"""

The configuration script gets the target from the command line. It creates a file with the configuration variable, and a short script for the rest of script to be able to read and locate he configuration files.

"""

def configure():
    target = get_target()
    make_var_file(target)
    make_location(target)

def get_target():
    """
    This functions uses a module I wrote to parse options. If no options are
    determined on the command line, the function returnst the default
    /etc/nest_docutis

    """
    options_dict = {
        'target':     [1, 't'],
    }
    options_obj = options_trem.ParseOptions(sys.argv, 
            options_dict)
    opt_dict, args = options_obj.parse_options()
    if opt_dict == 0:
        sys.stderr.write('you must provide a target!\n')
        sys.exit(1)
    target = opt_dict.get('target')
    if not target:
        sys.stderr.write('you must provide a target!\n')
        sys.exit(1)
    return target

def make_var_file(target):
    write_obj = open('var_file', 'w')
    # write_obj.write('[global]\n')
    write_obj.write(target)
    write_obj.close()

def make_location(target):
    write_obj = open('rst_to_docbook/location.py', 'w')
    write_obj.write(
    """
    def get_location():
        return %s


    """
    % target)


if __name__ == '__main__':
    configure()

