# -*-makefile-*-
DIR_FLAGS = -D xformoff='Decorations' -D generator=0

PERL_FLAGS = -Ihttpdefaultcss

PERL_FLAG_embed-stylesheet1 = -Inodefaultcss
PERL_FLAG_embed-stylesheet3 = -Ifiledefaultcss
PERL_FLAG_stylesheet2_2 = -Ifiledefaultcss

RST_FLAG_attribution1 = -W attribution=parens -W html-prolog=1 -W embed-stylesheet -W stylesheet=none
RST_FLAG_attribution2 = -W attribution=none -W bodyattr='bgcolor=red'
RST_FLAG_bad-link-target = -W link-target='1/0'
RST_FLAG_cloak-email = -W cloak-email-addresses
RST_FLAG_colspecs = -W colspecs=0
RST_FLAG_entryattr = -D entryattr='class="latin" bgcolor=blue' -D align
RST_FLAG_embed-stylesheet2 = -W embed-stylesheet
RST_FLAG_embed-stylesheet3 = -W embed-stylesheet
RST_FLAG_embed-stylesheet4 = -W embed-stylesheet -W stylesheet=local.css
RST_FLAG_embed-stylesheet5 = -W embed-stylesheet -W stylesheet=file://local.css
RST_FLAG_embed-stylesheet6 = -W embed-stylesheet -W stylesheet=http://default/local.css
RST_FLAG_embed-stylesheet7 = -W embed-stylesheet -W stylesheet=nosuchfile.css
RST_FLAG_footnotes_01 = -W footnote-backlinks=0 -W footnote-references=brackets
RST_FLAG_footnotes_02 = -W footnote-backlinks=0 -W footnote-references=superscript
RST_FLAG_limits = -W field-limit=9 -W option-limit=11
RST_FLAG_link-target1 = -W link-target='"top"'
RST_FLAG_references_01 = -D xformoff='Decorations|References'
RST_FLAG_stylesheet1 = -W stylesheet=local.css
RST_FLAG_stylesheet2 = -W stylesheet=file:nodefaultcss/Text/Restructured/default.css
RST_FLAG_stylesheet2_1 = -W stylesheet2=local.css
RST_FLAG_stylesheet2_2 = -W embed-stylesheet -W stylesheet2=local.css
RST_FLAG_subtitle1 = -D section_subtitles
RST_FLAG_subtitle2 = -D section_subtitles
