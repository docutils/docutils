 # -*- coding: UTF8 -*-
from att_set_dict import att_set_dict
 

#===========================================================
# SHORT CUTS

short_cut_att_sets = {
'abstract': 'abstract-block',
'abstract-title': 'abstract-title-block',
'abstract-paragraph': 'abstract-paragraph-block',
'address':'address-block',
'author':'author-block',
'authors':'authors-block',
'bibliographic-field':'bibliographic-fields-item-label-block',
'bibliographic-fields': 'bibliographic-fields-list-block',
'bibliographic-fields-text': 'bibliographic-fields-item-body',
'bibliographic-fields-paragraph': 'bibliographic-fields-block',
'body': 'default-flow',
'body-header' : 'body-header-block',
'body-footer' : 'body-footer-block',
'bullet-list': 'bullet-list-block',
'bullet-list-level2' :'bullet-level2-list-block',
'bullet-list-paragraph': 'bullet-list-item-body-block',
'contact':'contact-block',
'copyright':'copyright-block',
'bibliographic-field-custom1': 'custom-bib-info1',
'bibliographic-field-custom2': 'custom-bib-info2',
'bibliographic-field-custom3': 'custom-bib-info3',
'bibliographic-field-custom4': 'custom-bib-info4',
'bibliographic-field-custom5': 'custom-bib-info5',
'bibliographic-field-custom6': 'custom-bib-info6',
'bibliographic-field-custom7': 'custom-bib-info7',
'bibliographic-field-custom8': 'custom-bib-info8',
'bibliographic-field-custom9': 'custom-bib-info9',
'bibliographic-field-custom10': 'custom-bib-info10',
'block-quote' : 'block-quote-outer-block',
'block-quote-paragraph' : 'block-quote-paragraph-block', 
'block-quote-attribution' : 'block-quote-attribution-block',
'date':'date-block',
'dedication': 'dedication-block',
'dedication-title': 'dedication-title-block',
'dedication-paragraph': 'dedication-paragraph-block',
'definition-list-classifier': 'classifier-inline',
'definition-list': 'definition-list-block',
'definition-list-paragraph': 'definition-paragraph-block',
'definition-list-definition': 'definition-block',
'definition-term':'definition-term-block',
'document': 'default-page-sequence',
'even-footer': 'even-footer-block',
'even-header': 'even-header-block',
'even-page': 'even-simple-page-master',
'enumerated-list': 'enumerated-list-block',
'enumerated-list-level2': 'enumerated-level2-list-block',
'enumerated-list-paragraph': 'enumerated-list-item-body-block',
'field-name':'field-list-item-label-block',
'field-list': 'field-list-block',
'field-list-paragraph': 'field-body-block',
'field-name':'field-list-item-label-block',
'first-header' : 'first-header-block',
'first-footer' : 'first-footer-block',
'first-page': 'first-simple-page-master',
'first-paragraph': 'first-paragraph-block',
'footer' : 'footer-block',
'footer-paragraph': 'paragraph-footer-block',
'header': 'header-block',
'even-header': 'even-header-block',
'header-paragraph': 'paragraph-header-block',
'heading1':'title-level1-block',
'heading2':'title-level2-block',
'heading3':'title-level3-block',
'heading4':'title-level4-block',
'heading5':'title-level5-block',
'heading6':'title-level6-block',
'heading7':'title-level7-block',
'line-block': 'outer-line-block',
'line-level1': 'level1-line-block',
'line-level2': 'level2-line-block',
'line-level3': 'level3-line-block',
'line-level4': 'level4-line-block',
'line-level5': 'level5-line-block',
'odd-footer': 'odd-footer-block',
'odd-header': 'odd-header-block',
'odd-page': 'odd-simple-page-master',
'option': 'option-inline',
'option-argument': 'option-argument-inline',
'organization':'organization-block',
'page': 'default-simple-page-master',
'paper-size': 'paper-size-simple-page-master',
'paragraph': 'paragraph-block',
'revision':'revision-block',
'stanza-title': 'stanza-title-block',
'status':'status-block',
'subtitle': 'document-subtitle-block',
'table-and-caption' : 'table-block-container',
'title': 'document-title-block',
'title-subtitle': 'document-title-page-block',
'toc': 'toc-block',
'toc-body-footer':'toc-body-footer-block',
'toc-body-header':'toc-body-header-block',
'toc-first-header':'toc-first-header-block',
'toc-first-footer':'toc-first-footer-block',
'toc-odd-header':'toc-odd-header-block',
'toc-even-header':'toc-even-header-block',
'toc-odd-footer':'toc-odd-footer-block',
'toc-even-footer':'toc-even-footer-block',
'toc-title': 'toc-title-block',
'toc-entry1': 'toc-level1-block',
'toc-entry2': 'toc-level2-block',
'toc-entry3': 'toc-level3-block',
'toc-entry4': 'toc-level4-block',
'toc-entry5': 'toc-level5-block',
'toc-default': 'toc-entry-defaults-block',
'transition':'transition-block',
'version':'version-block',
}

short_cut_att_sets2 = {
('option-list-body','list') :'option-list-item-body',
('option-list-body','definition') :'option-list-description-block',
('option-list','list') :'option-list-block',
('option-list','definition') :'option-list-definition-block',
('options', 'list'): 'option-list-item-label-block',
('options', 'definition'): 'option-group-block',
('option-list-paragraph', 'list'): 'option-list-item-body-block',
('option-list-paragraph', 'definition'): 'option-list-paragraph-block',
}
# ===========================================================




# ATT LISTS
# ==========================================================================

accessibility_properties = [
'source-document',
'role']
area_alignment_properties = [
'alignment-adjust',
'alignment-baseline',
'baseline-shift',
'dominant-baseline',
'vertical-align']
area_properties = [
]
area_properties_inheritable = [
'display-align',
'reference-orientation',
'writing-mode']
area_properties_unheritable = [
'clip',
'overflow']
aural_properties_inheritable = [
'azimuth',
'elevation',
'pitch',
'pitch-range',
'play-during',
'richness',
'speak',
'speak-header',
'speak-numeral',
'speak-punctuation',
'speech-rate',
'stress',
'voice-family',
'volume']
aural_properties_unheritable = [
'cue',
'cue-after',
'cue-before',
'pause',
'pause-after',
'pause-before']
background_properties = [
'background',
'background-attachment',
'background-color',
'background-image',
'background-position',
'background-position-vertical',
'background-position-horizontal',
'background-repeat',
'rx:background-content-type',
'rx:background-content-height',
'rx:background-content-width',
'rx:background-scaling']
block_attlist = [
'text-altitude',
'text-depth']
block_container_attlist = [
'absolute-position',
'z-index']
block_properties = [
'clear',
'span']
border_padding_background_properties = [
]
border_precedence_properties = [
'border-after-precedence',
'border-before-precedence',
'border-end-precedence',
'border-start-precedence']
border_properties = [
'border',
'border-after-color',
'border-after-style',
'border-after-width',
'border-after-width.length',
'border-after-width.conditionality',
'border-before-color',
'border-before-style',
'border-before-width',
'border-before-width.length',
'border-before-width.conditionality',
'border-bottom',
'border-bottom-color',
'border-bottom-style',
'border-bottom-width',
'border-bottom-width.length',
'border-bottom-width.conditionality',
'border-color',
'border-end-color',
'border-end-style',
'border-end-width',
'border-end-width.length',
'border-end-width.conditionality',
'border-left',
'border-left-color',
'border-left-style',
'border-left-width',
'border-left-width.length',
'border-left-width.conditionality',
'border-right',
'border-right-color',
'border-right-style',
'border-right-width',
'border-right-width.length',
'border-right-width.conditionality',
'border-start-color',
'border-start-style',
'border-start-width',
'border-start-width.length',
'border-start-width.conditionality',
'border-style',
'border-top',
'border-top-color',
'border-top-style',
'border-top-width',
'border-top-width.length',
'border-top-width.conditionality',
'border-width']
box_size_properties = [
]
character_properties_inheritable = [
'letter-spacing',
'letter-spacing.minimum',
'letter-spacing.optimum',
'letter-spacing.maximum',
'letter-spacing.precedence',
'letter-spacing.conditionality',
'word-spacing',
'word-spacing.minimum',
'word-spacing.optimum',
'word-spacing.maximum',
'word-spacing.precedence',
'word-spacing.conditionality',
'glyph-orientation-horizontal',
'glyph-orientation-vertical',
'score-spaces',
'text-transform']
character_properties_unheritable = [
'text-decoration',
'text-shadow']
common_block_properties = [
'id',
'rx:key']
common_inline_properties = [
'id',
'rx:key',
'text-altitude',
'text-depth']
flow_attlist = [
]
flow_properties = [
'id',
'rx:key',
'flow-name']
font_properties = [
'font',
'font-selection-strategy',
'font-family',
'font-size',
'font-size-adjust',
'font-stretch',
'font-style',
'font-variant',
'font-weight']
footnote_body_attlist = [
]
height_properties = [
'height',
'min-height',
'max-height',
'block-progression-dimension',
'block-progression-dimension.minimum',
'block-progression-dimension.optimum',
'block-progression-dimension.maximum']
hyphenation_properties_block = [
'hyphenation-keep',
'hyphenation-ladder-count']
hyphenation_properties_inline = [
'country',
'language',
'script',
'xml:lang',
'hyphenate',
'hyphenation-character',
'hyphenation-push-character-count',
'hyphenation-remain-character-count']
inheritable_properties = [
]
inheritable_properties_block = [
'intrusion-displace',
'relative-align']
inheritable_properties_inline = [
'color',
'visibility']
inline_attlist = [
]
inline_properties = [
]
keeps_and_breaks_properties_atomic = [
'break-after',
'break-before',
'page-break-after',
'page-break-before']
keeps_and_breaks_properties_block_inheritable = [
'orphans',
'widows']
keeps_and_breaks_properties_inline_inheritable = [
'keep-together',
'keep-together.within-line',
'keep-together.within-column',
'keep-together.within-page',
'page-break-inside']
keeps_properties_atomic = [
'keep-with-next',
'keep-with-next.within-line',
'keep-with-next.within-column',
'keep-with-next.within-page',
'keep-with-previous',
'keep-with-previous.within-line',
'keep-with-previous.within-column',
'keep-with-previous.within-page']
leader_properties = [
'leader-alignment',
'leader-pattern',
'leader-pattern-width',
'leader-length',
'leader-length.minimum',
'leader-length.optimum',
'leader-length.maximum',
'rule-style',
'rule-thickness']
line_height_properties = [
'line-height',
'line-height.minimum',
'line-height.optimum',
'line-height.maximum',
'line-height.precedence',
'line-height.conditionality',
'line-height-shift-adjustment']
line_related_properties = [
'text-align',
'text-align-last',
'text-indent',
'last-line-end-indent',
'line-stacking-strategy',
'linefeed-treatment',
'white-space',
'white-space-treatment',
'white-space-collapse',
'wrap-option',
'direction']
list_block_attlist = [
'clear']
list_item_attlist = [
]
list_item_body_attlist = [
'id',
'rx:key']
list_item_label_attlist = [
'id',
'rx:key']
list_properties = [
'provisional-distance-between-starts',
'provisional-label-separation']
margin_properties_CSS = [
'margin',
'margin-bottom',
'margin-left',
'margin-right',
'margin-top']
margin_properties_block = [
'space-after',
'space-after.minimum',
'space-after.optimum',
'space-after.maximum',
'space-after.precedence',
'space-after.conditionality',
'space-before',
'space-before.minimum',
'space-before.optimum',
'space-before.maximum',
'space-before.precedence',
'space-before.conditionality']
margin_properties_inheritable = [
'start-indent',
'end-indent']
margin_properties_inline = [
'space-start',
'space-start.minimum',
'space-start.optimum',
'space-start.maximum',
'space-start.precedence',
'space-start.conditionality',
'space-end',
'space-end.minimum',
'space-end.optimum',
'space-end.maximum',
'space-end.precedence',
'space-end.conditionality']
padding_properties = [
'padding',
'padding-after',
'padding-after.length',
'padding-after.conditionality',
'padding-before',
'padding-before.length',
'padding-before.conditionality',
'padding-bottom',
'padding-bottom.length',
'padding-bottom.conditionality',
'padding-end',
'padding-end.length',
'padding-end.conditionality',
'padding-left',
'padding-left.length',
'padding-left.conditionality',
'padding-right',
'padding-right.length',
'padding-right.conditionality',
'padding-start',
'padding-start.length',
'padding-start.conditionality',
'padding-top',
'padding-top.length',
'padding-top.conditionality']
page_sequence_attlist = [
'format',
'letter-value',
'grouping-separator',
'grouping-size',
'id',
'rx:key',
'initial-page-number',
'force-page-count',
'master-reference']
region_after_attlist = [
'extent',
'precedence']
region_before_attlist = [
'extent',
'precedence']
region_body_attlist = [
'column-count',
'column-gap']
region_properties = [
'region-name']
relative_position_properties = [
'relative-position',
'position']
row_group_attlist = [
'id',
'rx:key']
simple_page_master_attlist = [
'master-name',
'page-height',
'page-width',
'reference-orientation',
'size',
'writing-mode']
table_attlist = [
'clear']
table_body_attlist = [
]
table_cell_attlist = [
'id',
'rx:key',
'column-number',
'ends-row',
'number-columns-spanned',
'number-rows-spanned',
'starts-row']
table_header_attlist = [
]
table_properties_inheritable = [
'border-collapse',
'border-spacing',
'border-separation',
'border-separation.inline-progression-direction',
'border-separation.block-progression-direction',
'caption-side',
'empty-cells']
table_properties_unheritable = [
'table-layout',
'table-omit-header-at-break',
'table-omit-footer-at-break',
'rx:table-omit-initial-header']
table_row_attlist = [
]
width_properties = [
'width',
'min-width',
'max-width',
'inline-progression-dimension',
'inline-progression-dimension.minimum',
'inline-progression-dimension.optimum',
'inline-progression-dimension.maximum']

area_properties.extend(area_properties_inheritable)
area_properties.extend(area_properties_unheritable)

inheritable_properties_inline.extend(aural_properties_inheritable)
inheritable_properties_inline.extend(character_properties_inheritable)
inheritable_properties_inline.extend(font_properties)
inheritable_properties_inline.extend(hyphenation_properties_inline)
inheritable_properties_inline.extend(line_height_properties)

inheritable_properties_block.extend(table_properties_inheritable)
inheritable_properties_block.extend(area_properties_inheritable)
inheritable_properties_block.extend(hyphenation_properties_block)
inheritable_properties_block.extend(margin_properties_inheritable)
inheritable_properties_block.extend(keeps_and_breaks_properties_inline_inheritable)
inheritable_properties_block.extend(keeps_and_breaks_properties_block_inheritable)
inheritable_properties_block.extend(leader_properties)
inheritable_properties_block.extend(line_related_properties)
inheritable_properties_block.extend(list_properties)

inheritable_properties.extend(inheritable_properties_inline)
inheritable_properties.extend(inheritable_properties_block)

border_padding_background_properties.extend(border_properties)
border_padding_background_properties.extend(padding_properties)
border_padding_background_properties.extend(background_properties)

margin_properties_block.extend(margin_properties_CSS)
margin_properties_inline.extend(margin_properties_CSS)

keeps_and_breaks_properties_atomic.extend(keeps_properties_atomic)

common_block_properties.extend(accessibility_properties)
common_block_properties.extend(aural_properties_unheritable)
common_block_properties.extend(border_padding_background_properties)
common_block_properties.extend(margin_properties_block)
common_block_properties.extend(inheritable_properties)

common_inline_properties.extend(accessibility_properties)
common_inline_properties.extend(aural_properties_unheritable)
common_inline_properties.extend(area_alignment_properties)
common_inline_properties.extend(border_padding_background_properties)
common_inline_properties.extend(character_properties_unheritable)
common_inline_properties.extend(keeps_properties_atomic)
common_inline_properties.extend(margin_properties_inline)
common_inline_properties.extend(relative_position_properties)
common_inline_properties.extend(inheritable_properties_inline)

region_properties.extend(border_padding_background_properties)

region_properties.extend(area_properties)
flow_properties.extend(inheritable_properties)

block_properties.extend(common_block_properties)
block_properties.extend(keeps_and_breaks_properties_atomic)
block_properties.extend(relative_position_properties)

box_size_properties.extend(height_properties)
box_size_properties.extend(width_properties)

inline_properties.extend(common_inline_properties)
inline_properties.extend(inheritable_properties_block)

block_attlist.extend(block_properties)
block_attlist.extend(character_properties_unheritable)
block_container_attlist.extend(area_properties_unheritable)
block_container_attlist.extend(block_properties)
block_container_attlist.extend(box_size_properties)
flow_attlist.extend(flow_properties)
footnote_body_attlist.extend(accessibility_properties)
footnote_body_attlist.extend(inheritable_properties)
inline_attlist.extend(height_properties)
inline_attlist.extend(inline_properties)
list_block_attlist.extend(common_block_properties)
list_block_attlist.extend(keeps_and_breaks_properties_atomic)
list_block_attlist.extend(relative_position_properties)
list_item_attlist.extend(common_block_properties)
list_item_attlist.extend(keeps_and_breaks_properties_atomic)
list_item_attlist.extend(relative_position_properties)
list_item_body_attlist.extend(accessibility_properties)
list_item_body_attlist.extend(inheritable_properties)
list_item_label_attlist.extend(accessibility_properties)
list_item_label_attlist.extend(inheritable_properties)
page_sequence_attlist.extend(inheritable_properties)
region_after_attlist.extend(region_properties)
region_before_attlist.extend(region_properties)
region_body_attlist.extend(margin_properties_CSS)
region_body_attlist.extend(region_properties)
row_group_attlist.extend(accessibility_properties)
row_group_attlist.extend(aural_properties_unheritable)
row_group_attlist.extend(background_properties)
row_group_attlist.extend(border_precedence_properties)
row_group_attlist.extend(border_properties)
row_group_attlist.extend(inheritable_properties)
row_group_attlist.extend(relative_position_properties)
simple_page_master_attlist.extend(margin_properties_CSS)
table_attlist.extend(box_size_properties)
table_attlist.extend(common_block_properties)
table_attlist.extend(keeps_and_breaks_properties_atomic)
table_attlist.extend(table_properties_unheritable)
table_body_attlist.extend(keeps_and_breaks_properties_atomic)
table_body_attlist.extend(row_group_attlist)
table_cell_attlist.extend(accessibility_properties)
table_cell_attlist.extend(aural_properties_unheritable)
table_cell_attlist.extend(border_padding_background_properties)
table_cell_attlist.extend(border_precedence_properties)
table_cell_attlist.extend(box_size_properties)
table_cell_attlist.extend(inheritable_properties)
table_cell_attlist.extend(keeps_and_breaks_properties_atomic)
table_header_attlist.extend(row_group_attlist)
table_row_attlist.extend(height_properties)
table_row_attlist.extend(keeps_and_breaks_properties_atomic)
table_row_attlist.extend(row_group_attlist)

which_list = {
'block': block_attlist,
'block-container': block_container_attlist,
'cell': table_cell_attlist,
'flow': flow_attlist,
'footnote-body': footnote_body_attlist,
'header': table_header_attlist,
'inline': inline_attlist,
# 'item-body': list_item_body_attlist,
'list-item-body': list_item_body_attlist,
'item-label': list_item_label_attlist,
'list-block':list_block_attlist,
'list-item': list_item_attlist,
'page-sequence': page_sequence_attlist,
'region-after': region_after_attlist,
'region-before': region_before_attlist,
'region-body': region_body_attlist,
'simple-page-master': simple_page_master_attlist,
'table': table_attlist,
'table-body': table_body_attlist,
'table-cell': table_cell_attlist,
'table-header' : table_header_attlist,
'table-row': table_row_attlist
}

custom_atts = {
'bottom-margin' : 'margin-bottom',
'left-margin' : 'margin-left',
'right-margin' : 'margin-right', 
'top-margin' : 'margin-top',
'line-spacing':'line-height',
'first-line-indent': 'text-indent',
'left-indent': 'start-indent',
'right-indent': 'end-indent',
'alignment':'text-align',
'keep-on-same-page': 'keep-together.within-page',
'font': 'font-family', # could be problamatic
'space-from-label': 'provisional-distance-between-starts',
'height': 'page-height',
'width': 'page-width',
# 'space-from-option': 'provisional-distance-between-starts',
# 'space-between-items': 'space-before',
}


special_att_sets_dict = {
        }

# att att-sets
special_att_att_set_dict = {
('bold', 'font-style'): [('font-weight','bold')],
('italic', 'font-style'): [('font-style','italic')],
('bold-italic', 'font-style'):[('font-weight','bold'), ('font-style','italic')],
('italic-bold', 'font-style'):[('font-weight','bold'), ('font-style','italic')],
('normal', 'font-style'):[('font-weight', 'normal'), ('font-style', 'normal')]
}

# att-sets atts
special_att_set_att_dict = {
('bibliographic-fields', 'space-from-field'): [('bibliographic-fields-list-block', 'provisional-distance-between-starts')], 
('bibliographic-fields', 'space-from-name'): [('bibliographic-fields-list-block', 'provisional-distance-between-starts')], 
('bibliographic-fields', 'space-between-items'): [('bibliographic-fields-list-item', 'space-before')], 
('body-section' , 'start-page'):[('body-page-sequence', 'initial-page-number')],
('body-section' , 'page-format'):[('body-page-sequence', 'format')],
('bullet-list', 'space-between-items'): [('bullet-list-item', 'space-before')], 
('bullet-list', 'space-from-bullet'): [('bullet-list-block', 'provisional-distance-between-starts')], 
('bullet-list-level2', 'space-from-bullet'): [('bullet-level2-list-block', 'provisional-distance-between-starts')], 
('bullet-list-level2', 'space-between-items'): [('bullet-level2-list-item', 'space-before')], 
('definition-list', 'space-between-items'): [('definition-list-item-block', 'space-before')], 
('definition-list', 'space-below-term'): [('definition-term-block', 'space-after')], 

('enumerated-list', 'space-between-items'): [('enumerated-list-item', 'space-before')], 
('enumerated-list', 'space-from-number'): [('enumerated-list-block', 'provisional-distance-between-starts')], 
('enumerated-list-level2', 'space-from-number'): [('bullet-level2-list-block', 'provisional-distance-between-starts')], 
('enumerated-list-level2', 'space-between-items'): [('bullet-level2-list-item', 'space-before')], 
('header' , 'height'):[('header-region-before', 'extent'),('region-body', 'margin-top')],
('field-list', 'space-from-name'): [('field-list-block', 'provisional-distance-between-starts')], 
('field-list', 'space-between-items'): [('field-list-item', 'space-before')], 
('footer' , 'height'):[('footer-region-after', 'extent'),('region-body', 'margin-bottom')],
('toc-section' , 'start-page'):[('toc-page-sequence', 'initial-page-number')],
('toc-section' , 'page-format'):[('toc-page-sequence', 'format')],
('table' , 'width'):[('table', 'inline-progression-dimension')],

}

# special: only in certain contexts
special_att_set_att_dict2 = {
('option-list', 'space-from-option', 'list'): [('option-list-block', 'provisional-distance-between-starts')], 
('option-list', 'space-between-items', 'list'): [('option-list-item', 'space-before')], 
('option-list', 'space-between-items', 'definition'): [('option-list-item-block', 'space-before')], 
('option-list', 'space-below-option', 'definition'): [('option-group-block', 'space-after')], 
        }
special_att_value_dict = {
('font-style', 'bold'): [('font-weight','bold')],
('font-style', 'italic'): [('font-style','italic')],
('font-style','bold-italic'):[('font-weight','bold'), ('font-style','italic')],
('font-style','italic-bold' ):[('font-weight','bold'), ('font-style','italic')],
('font-style', 'normal'):[('font-weight', 'normal'), ('font-style', 'normal')],
('page-break-before' , 'True'):[('break-before', 'page')],
('page-break-before' , 'true'):[('break-before', 'page')],
('page-break-before' , 'Yes'):[('break-before', 'page')],
('page-break-before' , 'yes'):[('break-before', 'page')],
('page-break-before' , 'False'):[('break-before', 'auto')],
('page-break-before' , 'ralse'):[('break-before', 'auto')],
('page-break-before' , 'No'):[('break-before', 'auto')],
('page-break-before' , 'no'):[('break-before', 'auto')],
('page-break-after' , 'True'):[('break-after', 'page')],
('page-break-after' , 'true'):[('break-after', 'page')],
('page-break-after' , 'Yes'):[('break-after', 'page')],
('page-break-after' , 'yes'):[('break-after', 'page')],
('page-break-after' , 'False'):[('break-after', 'auto')],
('page-break-after' , 'ralse'):[('break-after', 'auto')],
('page-break-after' , 'No'):[('break-after', 'auto')],
('page-break-after' , 'no'):[('break-after', 'auto')],
('keep-with-next' , 'True'):[('keep-with-next', 'always')],
('keep-with-next' , 'true'):[('keep-with-next', 'always')],
('keep-with-next' , 'Yes'):[('keep-with-next', 'always')],
('keep-with-next' , 'yes'):[('keep-with-next', 'always')],
('keep-with-previous' , 'True'):[('keep-with-previous', 'always')],
('keep-with-previous' , 'true'):[('keep-with-previous', 'always')],
('keep-with-previous' , 'Yes'):[('keep-with-previous', 'always')],
('keep-with-previous' , 'yes'):[('keep-with-previous', 'always')],
('keep-on-same-page' , 'True'):[('keep-together.within-page', 'always')],
('keep-on-same-page' , 'true'):[('keep-together.within-page', 'always')],
('keep-on-same-page' , 'Yes'):[('keep-together.within-page', 'always')],
('keep-on-same-page' , 'yes'):[('keep-together.within-page', 'always')],
('vertical-alignment' , 'bottom'):[('display-align', 'after')],
('vertical-alignment' , 'top'):[('display-align', 'before')],
('vertical-alignment' , 'center'):[('display-align', 'center')],
}

special_values_dict = {
        }


# ==================================================================
# PROPERTIES THAT ARE REALLY PARAMS

prop_as_param_dict = {
'page.page-layout':'page-layout',
'title-subtitle.placement': 'title-pagination', 
'bibliographic-fields.placement' : 'bibliographic-pagination',
'dedication.placement': 'dedication-pagination',
'abstract.placement': 'abstract-pagination',
'toc.placement': 'toc-pagination',
'front.order': 'font-order',
'header.suppress-first-page': 'suppress-first-page-header',
'footer.suppress-first-page': 'suppress-first-page-footer',
'bibliographic-fields.author-text': 'author-text',
'bibliographic-fields.authors-text': 'authors-text',
'bibliographic-fields.organization-text': 'organization-text',
'bibliographic-fields.contact-text': 'contact-text',
'bibliographic-fields.status-text': 'status-text',
'bibliographic-fields.copyright-text': 'copyright-text',
'bibliographic-fields.address-text': 'address-text',
'bibliographic-fields.revision-text': 'revision-text',
'bibliographic-fields.date-text': 'date-text',
'bibliographic-fields.format': 'bibliographic-format',
'bibliographic-field-custom1.text': 'custom-bib-info1-name',
'bibliographic-field-custom2.text': 'custom-bib-info2-name',
'bibliographic-field-custom3.text': 'custom-bib-info3-name',
'bibliographic-field-custom4.text': 'custom-bib-info4-name',
'bibliographic-field-custom5.text': 'custom-bib-info5-name',
'bibliographic-field-custom6.text': 'custom-bib-info6-name',
'bibliographic-field-custom7.text': 'custom-bib-info7-name',
'bibliographic-field-custom8.text': 'custom-bib-info8-name',
'bibliographic-field-custom9.text': 'custom-bib-info9-name',
'bibliographic-field-custom10.text': 'custom-bib-info10-name',
'heading1.number-format':'number-section1',
'heading2.number-format':'number-section2',
'heading3.number-format':'number-section3',
'heading4.number-format':'number-section4',
'heading5.number-format':'number-section5',
'heading6.number-format':'number-section6',
'heading7.number-format':'number-section7',
'headings.inherit-sections-number':'inherit-section-num',
'line-block.number': 'number-verse',
'transition.text':'transition-text',
'bullet-list.text': 'bullet-text',
'bullet-list-level2.text': 'bullet-text-level2',
'option-list.format':'option-list-format',
'options-list.separator': 'options-separator',
'document.page-layout': 'page-layout',
'block-quote.attriubution-text' : 'text-before-block-quote-attribution',
'table.column-widths': 'table-cols',
}


# ========================================================================
# DEFAULT PARAMS
param_dict = {
'abstract-pagination' : 'with-front',
'address-text' : 'Address: ',
'attention-title' : 'Attention!',
'author-text' : 'Author: ',
'authors-text' : 'Authors: ',
'bibliographic-pagination' : 'with-toc',
'bullet-text' : '•',
'caution-title' : 'Caution!',
'contact-text' : 'Contact: ',
'copyright-text' : 'Copyright: ',
'danger-title' : '!Danger!',
'date-text' : 'Date: ',
'dedication-pagination' : 'with-front',
'error-title' : 'Error',
'footnote-placement' : 'footnote',
'footnote-style' : 'list',
'front-order' : 'title,bibliographic,dedication,abstract,toc',
'hint-title' : 'Hint',
'important-title' : 'Important',
'inherit-section-num' : 'True',
'internal-link-type' : 'link',
'note-title' : 'Note',
'number-section1' : '1',
'number-section2' : '.1',
'number-section3' : '.1',
'number-section4' : '.1',
'number-section5' : '.1',
'number-section6' : '.1',
'number-section7' : '.1',
'number-section8' : '.1',
'number-section9' : '.1',
'number-verse' : '',
'option-list-format' : 'list',
'options-separator' : ', ',
'organization-text' : 'Organization: ',
'page-layout' : 'simple',
'revision-text' : 'Revision: ',
'space-between-footnotes' : '5pt',
'spacing-footer' : '',
'spacing-header' : '',
'status-text' : 'Status: ',
'strict' : '',
'suppress-first-page-footer' : '',
'suppress-first-page-header' : '',
'table-title-placement' : 'bottom',
'test' : '',
'text-before-block-quote-attribution' : '—',
'text-before-epigraph-attribution' : '—',
'text-before-pull-quote-attribution' : '—',
'tip-title' : 'Tip',
'title-pagination' : 'with-front',
'toc-pagination' : 'with-toc',
'transition-text' : '***',
'version-text' : 'Version: ',
'warning-title' : 'Warning!',
}
param_list = param_dict.keys()

true_or_false_dict = {'True':'True', 'true':'True', 'yes':'True', 'Yes': 'True', 'False': 'False', 
        'false':'False', 'no': 'False', 'No':'False'}
true_dict = {'true':True, 'True':True, 'yes':True, 'Yes':True}
false_dict = {'false':False, 'False':False, 'none': False, 'None':False, 'no': False, 'No':False}
param_dict_test = {'strict':true_or_false_dict,
        'suppress-first-page-header': true_or_false_dict,
        'suppress-first-page-footer': true_or_false_dict,
        }

# Commands List
# =========================================================================
commands_list = ['xsl-stylesheet']
