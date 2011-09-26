import os, sys 
sys.path.insert(0, '../docutilsToFo')
from docutils_fo_dicts import att_set_dict, short_cut_att_sets
import pprint

def dump(object):
    pp = pprint.PrettyPrinter(indent=4)
    sys.stdout.write(pp.pformat(object))
    sys.stdout.write('\n')

set_shortcut = set(short_cut_att_sets.values())
set_att_set = set(att_set_dict.keys())

if set_shortcut.issubset(set_att_set):
    print 'No problems'
else:
    print 'missing att sets'
    diff = set_shortcut.difference(set_att_set)
    dump(diff)
