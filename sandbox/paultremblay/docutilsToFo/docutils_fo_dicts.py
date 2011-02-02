
block_dict = {
'space-before':'space-before',
'font-style':['font-weight', 'font-style']
}

att_set_dict = {
'paragraph':['paragrah', 'block'],
'bullet-list':['bullet-list', 'list-block'],
'bullet-list-test':['bullet-list', 'list-block']
}

list_block_dict = {
'space-after':'space-after',
'space-between-items': 'space-after',
'font-style':['font-weight', 'font-style']
}


att_dict = {
'block':['space-before', ('space-between', 'space-before')]
}

font_style_dict={'bold': [('font-weight','bold')],
'italic': [('font-style','italic')],
'bold-italic':[('font-weight','bold'), ('font-style','italic')],
'italic-bold':[('font-weight','bold'), ('font-style','italic')]
}

which_dict = {
'block': block_dict,
'list-block':list_block_dict
}


