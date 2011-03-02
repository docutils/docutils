import os, sys, glob, pprint, commands
sys.path.insert(0, '../docutilsToFo')
from docutils_fo_dicts import att_set_dict

def dump(w_obj, object):
    pp = pprint.PrettyPrinter(indent=0)
    w_obj.write(pp.pformat(object))
    w_obj.write('\n')

ss = '/Users/cynthia/tmp/paultremblay/tools/make_my_att_list.xsl'
current_dir = os.getcwd()
os.chdir('../docutilsToFo/xsl_fo')
paths = glob.glob('*xsl')
the_dict = {}
for the_path in paths:
    command = 'xsltproc %s %s' % (ss, the_path) 
    status, output = commands.getstatusoutput(command)
    entries = output.split('\n')
    for entry in entries:
        if not entry:
            continue
        if 'No match' in entry:
            sys.stderr.write('%s\n' % (the_path))
            sys.stderr.write('%s\n' % (entry))
            sys.exit(1)
            continue
        try:
            first, second = entry.split()
            the_dict[first] = second
        except:
            pass

os.chdir(current_dir)
if the_dict != att_set_dict:
    write_file = os.path.join('..', 'docutilsToFo', 'att_set_dict.py')
    if not os.path.isfile(write_file):
        sys.stderr.write('Can\'t find %s\n' % (write_file))
        sys.exit(1)
    write_obj = file(write_file, 'w')
    write_obj.write('att_set_dict = ')
    sys.stderr.write('Updating docutilsToFo/att_set_dict.py\n')
    dump(write_obj, the_dict)
    write_obj.close()

