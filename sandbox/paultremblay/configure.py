# /usr/bin/env python 

import sys, os
import options_trem

"""

The configuration script gets the target from the command line. It changest the setup.py and the actual scrilpt so that they have the right target.

"""

def configure():
    target = get_target()
    change_setup(target)
    change_script(target)

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
        sys.stdout.write('Will use the default configuration of /etc/nest_docutils\n')
        return '/etc/docutils_nest'
    target = opt_dict.get('target')
    if not target:
        return '/etc/docutils_nest'
    return target

def change_setup(target):
    """

    Chage the setup.py file to reflect the target

    """
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

    """

    Changet the script to reflect the right target

    """
    read_obj = open('nest_inline/nest_docutils.py', 'r')
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
