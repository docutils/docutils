from docutilsToFo.docutils_fo_dicts import *
import pprint
def find_diff(dict1, dict2):
    set1 = set(dict1)
    set2 = set(dict2)
    diff = set1.symmetric_difference(set2)
    return len(diff), diff

def find_common(dict1, dict2):
    set1 = set(dict1)
    set2 = set(dict2)
    common = set1.intersection(set2)
    return common

def dump(object):
    pp = pprint.PrettyPrinter(indent=4)
    pp.pprint(object)

list_of_dicts = []
all_dicts = which_dict.keys()
for the_str in all_dicts:
    the_dict = which_dict[the_str]
    pair = (the_str, the_dict)
    list_of_dicts.append(pair)

list_of_dicts.sort()

counter = 0
for pair in list_of_dicts:
    first = pair[0]
    if counter == len(list_of_dicts) - 1:
        break
    for j in range(counter + 1, len(list_of_dicts)):
        # print list_of_dicts[j][0]
        second = list_of_dicts[j][0]
        if first == second:
            print 'woops'
        first_dict = list_of_dicts[counter][1]
        second_dict = list_of_dicts[j][1]
        the_diff, diff_dict = find_diff(first_dict, second_dict )
        if the_diff < 20:
            if len(first_dict) < 20 or len(second_dict) < 20:
                pass
            else:
                print '%s and %s have %s diff' % (first, second, the_diff)
                common_dict = find_common(first_dict, second_dict)
                # dump(common_dict)
                dump(diff_dict)
    counter += 1



block_set = set(block_dict.keys())
block_container_set = set(block_container_dict.keys())
cell_set= set(cell_dict.keys())
intersect_block_block_container = block_set.intersection(block_container_set)
intersect_block_container_cell = block_container_set.intersection(cell_set)
diff = block_container_set.symmetric_difference(block_set)

rbe_set = set(region_before_dict.keys())
rbo_set = set(region_body_dict.keys())
diff = rbe_set.symmetric_difference(rbo_set)

