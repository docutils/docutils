
# /usr/bin/env python 

import sys, os
import docutils_nest.options_trem

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
    options_obj = docutils_nest.options_trem.ParseOptions(sys.argv, 
            options_dict)
    opt_dict, args = options_obj.parse_options()
    if opt_dict == 0:
        sys.stderr.write('invalid way to run configure:\n'
                'python configure.py --target <directory of choice>'
                )
        sys.exit(1)
    target = opt_dict.get('target')
    if not target:
        target = default_target()
    return target

def default_target():
    sys.stdout.write('using default /etc for configuration directory\n')
    return '/etc'
    
def make_var_file(target):
    write_obj = open('var_file', 'w')
    # write_obj.write('[global]\n')
    write_obj.write(target)
    write_obj.close()

def make_location(target):
    write_obj = open('docutils_nest/location.py', 'w')
    write_obj.write(
    """
def get_location():
    return '%s'


    """
    % target)


if __name__ == '__main__':
    configure()





"""
# /usr/bin/env python 

import sys, os
import options_trem


def configure():
    target = get_target()
    change_setup(target)
    change_script(target)

def get_target():
    options_dict = {
        'target':     [1, 't'],
    }
    options_obj = options_trem.ParseOptions(sys.argv, 
            options_dict)
    opt_dict, args = options_obj.parse_options()
    if opt_dict == 0:
        sys.stdout.write('Will use the default configuration of /etc/nest_docutils\n')
        return '/etc/docutils_nest'
    target = opt_dict.get('target')
    if not target:
        return '/etc/docutils_nest'
    return target

def change_setup(target):
    read_obj = open('setup.py', 'r')
    write_obj = open('temp', 'w')
    line = 1
    while line:
        line = read_obj.readline()
        index = line.find('data_files=')
        if index > -1:
            write_obj.write('data_files = [("%s", ["data/configure.xml"])],\n' % target)
        else:
            write_obj.write(line)
    read_obj.close()
    write_obj.close()
    read_obj = open('temp', 'r')
    write_obj = open('setup.py', 'w')
    line = 1
    while line:
        line = read_obj.readline()
        write_obj.write(line)
    read_obj.close()
    write_obj.close()


def change_script(target):

    read_obj = open('docutils_nest/nest_docutils.py', 'r')
    write_obj = open('temp', 'w')
    line = 1
    while line:
        line = read_obj.readline()
        index = line.find('$configure$')
        if index > -1:
            write_obj.write("ext_location = '%s' #  $configure$" % \
                    target)
        else:
            write_obj.write(line)
    read_obj.close()
    write_obj.close()
    read_obj = open('temp', 'r')
    write_obj = open('docutils_nest/nest_docutils.py', 'w')
    line = 1
    while line:
        line = read_obj.readline()
        write_obj.write(line)
    read_obj.close()
    write_obj.close()

if __name__ == '__main__':
    configure()
"""
