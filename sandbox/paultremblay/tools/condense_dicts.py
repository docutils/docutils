from docutilsToFo.docutils_fo_dicts import *
import pprint, copy
def dump(object):
    pp = pprint.PrettyPrinter(indent=0)
    pp.pprint(object)

list_of_dicts2_ = [
block_dict,
block_container_dict,
cell_dict,
flow_dict,
footnote_body_dict,
header_dict,
inline_dict,
item_body_dict,
item_label_dict,
list_block_dict,
list_item_dict,
page_sequence_dict,
region_after_dict,
region_before_dict,
region_body_dict,
simple_page_master_dict,
table_dict,
table_body_dict,
table_row_dict,
]



s_block = set(block_dict)
s_block_container = set(block_container_dict)
s_cell = set(cell_dict)
s_flow = set(flow_dict)
s_footnote_body = set(footnote_body_dict)
s_header = set(header_dict)
s_inline = set(inline_dict)
s_item_body = set(item_body_dict)
s_item_label = set(item_label_dict)
s_list_block = set(list_block_dict)
s_list_item = set(list_item_dict)
s_page_sequence = set(page_sequence_dict)
s_region_after= set(region_after_dict)
s_region_before = set(region_before_dict)
s_region_body = set(region_body_dict)
s_simple_page_master= set(simple_page_master_dict)
s_table = set(table_dict)
s_table_body = set(table_body_dict)
s_table_row = set(table_row_dict)



"""
new_block_container_dict = {}
diff = set(block_container_dict).difference(set(azimuth_dict))
new_dict = {}
for item in diff:
    new_dict[item] = item
dump(new_dict)
"""
"""
new_region_after_dict = {}
new_region_after_dict.update(common_dict1)
new_region_after_dict.update(padding_dict)
diff = set(new_region_after_dict).symmetric_difference(set(region_after_dict))
new_dict = {}
for item in diff:
    new_dict[item] = item
dump(new_dict)
"""
diff = set(new_region_after_dict).symmetric_difference(set(region_after_dict))
print diff



# items in the first (list_block_dict) not in second. the second is the base.
# diff = set(list_block_dict).difference(set(list_item_dict))








