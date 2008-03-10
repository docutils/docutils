#!/bin/sh

# ===========================
# generate-sample-variants.sh
# ===========================
#   
# Convert the sample text to HTML in some variants to enable comparison.
# 
# ::

INFILE="../data/sample.txt"
RST2HTML="../tools/rst2html_strict.py"

# Standard html4css1 writer
# =========================
#
# Field lists are rendered via tables with some magic for long field names.
# This can lead to problems in demonstrated cases and is not configurable by
# CSS.
#
# The ``--field-name-limit=14`` is the docutils default, which I have
# overwritten in my personal ``~/.docutils`` file::

rst2html --stylesheet=../data/html4css1.css --link-stylesheet --field-name-limit=14 \
         --title=html4css1 $INFILE sample-html4css1.html
         
# html4strict writer with htm4css1 stylesheet     
# ===========================================
#
# Tables have no border.
#
# Docinfo and field-lists are rendered as default definition lists (ugly).
# ::

$RST2HTML --stylesheet=../data/html4css1.css --link-stylesheet \
          --title=html4-strict-css1 $INFILE sample-html4-strict-css1.html


# html4strict writer with  htm4css2 stylesheet     
# ============================================
# ::

$RST2HTML --stylesheet=../data/html4css2.css --link-stylesheet \
          --title=html4css2 $INFILE sample-html4css2.html

# Layout variant with hanging indent  
# ==================================
# 
# prolems with multi-paragraph body::

$RST2HTML --stylesheet=../data/field-list-hanging.css --link-stylesheet \
          --title=field-list-hanging $INFILE sample-field-list-hanging.html

# Layout variant with 2 colums and truncating
# ===========================================
# 
# Long field names are truncated.
# ::

$RST2HTML --stylesheet=../data/field-list-table1.css --link-stylesheet \
          --title=field-list-table1 $INFILE sample-field-list-table1.html

# Layout variant with 2 colums, wrapping and truncating
# =====================================================
# 
# Long field names wrap. If wrapping is not possible, the field names are
# truncated.
# 
# Problems with wrapping: the next field-body floats up to the right of the
# wrapped field-name.
#
# ::

$RST2HTML --stylesheet=../data/field-list-table-wrap.css --link-stylesheet \
          --title=field-list-table-wrap $INFILE sample-field-list-table-wrap.html


