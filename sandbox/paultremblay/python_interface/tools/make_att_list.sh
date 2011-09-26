xsltproc make_att_list1.xsl temp_prop_basic.rng |xmlformat.pl > scratch1.xml
xsltproc make_att_list2.xsl scratch1.xml |xmlformat.pl > scratch2.xml
xsltproc make_att_list3.xsl scratch2.xml |xmlformat.pl > scratch3.xml
xsltproc make_att_list5.xsl scratch3.xml |xmlformat.pl > scratch4.xml
python hack.py scratch4.xml > scratch5.xml
# python scratch2.py
