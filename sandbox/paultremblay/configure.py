# /usr/bin/python 

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
        return '/etc/nest_docutils'
    target = opt_dict.get('target')
    if not target:
        return '/etc/nest_docutils'
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
    write_obj = open('nest_inline/nest_docutils.py', 'w')
    line = 1
    while line:
        line = read_obj.readline()
        write_obj.write(line)
    read_obj.close()
    write_obj.close()

if __name__ == '__main__':
    configure()
