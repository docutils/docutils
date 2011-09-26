# $Id$
set -e
set -u #  warns if a variable is not assigned.

STYLESHEET=../xsl_fo/docutils_to_fo.xsl 
rm -Rf test_output
mkdir test_output
cp test_files/* test_output
cd test_output

echo converting rst to xml
# convert the files to XML
FILES=`ls *rst`

for THE_FILE in $FILES 
do
    FILENAME=`basename $THE_FILE .rst`
    NEW_FILE=${FILENAME}.xml
    rst2xml.py $THE_FILE > $NEW_FILE
done

echo converting XML to FO
# simple page, no numbers, inline; just paragraphs
xsltproc --stringparam page-layout "simple" \
$STYLESHEET long_plain.xml  > simple_no_page_nos.fo 
xmlformat.pl -i simple_no_page_nos.fo

# first page, no numbers, etc; just paragraphs
xsltproc --stringparam page-layout "first" \
$STYLESHEET long_plain.xml   > first_page_diff_no_page_nos.fo 
xmlformat.pl -i first_page_diff_no_page_nos.fo

# odd-even page, no numbers, etc; just paragraphs
xsltproc --stringparam page-layout "odd-even" \
$STYLESHEET long_plain.xml    > odd_even_no_page_nos.fo 
xmlformat.pl -i odd_even_no_page_nos.fo

# first-odd-even page, no numbers, etc; just paragraphs
xsltproc --stringparam page-layout "first-odd-even" \
$STYLESHEET long_plain.xml    > first_odd_even_no_page_nos.fo 
xmlformat.pl -i first_odd_even_no_page_nos.fo

# simple page, with headers and footers, no inline; just paragraphs
xsltproc --stringparam page-layout "simple" \
$STYLESHEET header_footer.xml   >  header_footer.fo 
xmlformat.pl -i header_footer.fo

# simple page, with headers and footers, no inline; just paragraphs
# the --suppress-page-num has no effect
xsltproc --stringparam page-layout "simple" \
--stringparam suppress-first-page-header "True" \
$STYLESHEET header_footer.xml    > header_footer2.fo 
xmlformat.pl -i header_footer2.fo

# first page, with headers and footers, no inline; just paragraphs
xsltproc --stringparam page-layout "simple" \
$STYLESHEET header_footer.xml   >  first_page_header_footer.fo 
xmlformat.pl -i first_page_header_footer.fo

# first page, with headers and footers, no inline; just paragraphs
# suppress first page header and footer
xsltproc --stringparam page-layout "first" \
--stringparam suppress-first-page-header "True" \
--stringparam suppress-first-page-footer "True" \
$STYLESHEET header_footer.xml   >  first_suppress_header__footer.fo 
xmlformat.pl -i first_suppress_header__footer.fo

# odd-even page, with headers and footers, no inline; just paragraphs
xsltproc --stringparam page-layout "odd-even" \
$STYLESHEET header_footer.xml   >  odd_even_page_header_footer.fo 
xmlformat.pl -i odd_even_page_header_footer.fo

# first-odd-even page, with headers and footers, no inline; just paragraphs
xsltproc --stringparam page-layout "first-odd-even" \
$STYLESHEET header_footer.xml   >  first_odd_even_page_header_footer.fo 
xmlformat.pl -i first_odd_even_page_header_footer.fo

# first-odd-even page, with headers and footers, no inline; just paragraphs
# suppress first header and footer
xsltproc --stringparam page-layout "first-odd-even" \
--stringparam suppress-first-page-header "True" \
--stringparam suppress-first-page-footer "True" \
$STYLESHEET header_footer.xml   >  first_odd_even_page_header_footer_suppress_first.fo 
xmlformat.pl -i first_odd_even_page_header_footer_suppress_first.fo

# TOC test
xsltproc $STYLESHEET toc.xml > toc.fo
xmlformat.pl -i toc.xml

# literal block test
xsltproc $STYLESHEET literal_block.xml > literal_block.fo

# inline test
xsltproc $STYLESHEET inline.xml > inline.fo
xmlformat.pl -i inline.xml

# transistion test
xsltproc $STYLESHEET transition.xml > transition.fo
xmlformat.pl -i transition.xml

# bullet list test
xsltproc $STYLESHEET bullet_list.xml > bullet_list.fo
xmlformat.pl -i bullet_list.xml

# enumerated list test
xsltproc $STYLESHEET enumerated_list.xml > enumerated_list.fo
xmlformat.pl -i enumerated_list.xml

# definition list test
xsltproc $STYLESHEET definition_list.xml > definition_list.fo
xmlformat.pl -i definition_list.xml

# Field List test
xsltproc $STYLESHEET field_lists.xml > field_lists.fo
xmlformat.pl -i field_lists.xml

# section test
xsltproc $STYLESHEET section.xml > section.fo
xmlformat.pl -i section.xml

# bibliograhic field test
xsltproc $STYLESHEET bibliographic_fields.xml > bibliographic_fields.fo
xmlformat.pl -i bibliographic_fields.xml

# bibliograhic fields with toc test
xsltproc $STYLESHEET bibliographic_fields_toc.xml > bibliographic_fields_toc.fo
xmlformat.pl -i bibliographic_fields_toc.xml

# test for options_list
xsltproc  $STYLESHEET opt_list.xml   > opt_list.fo 
xmlformat.pl -i opt_list.fo

# test for options_list with definition layout
xsltproc --stringparam option-list-format "definition" \
$STYLESHEET opt_list.xml   >  opt_list_as_def.fo 
xmlformat.pl -i opt_list_as_def.fo

# literal block
xsltproc $STYLESHEET literal_block.xml > literal_block.fo


xsltproc $STYLESHEET line_block.xml > line_block.fo

xsltproc $STYLESHEET block.xml > block.fo
xsltproc $STYLESHEET doctest_blocks.xml > doctest_blocks.fo

xsltproc $STYLESHEET table_csv.xml > table_csv.fo

# with param of title on toop
xsltproc --stringparam table-title-placement "top" \
$STYLESHEET table_csv.xml > table_caption_top_csv.fo

xsltproc $STYLESHEET table_grid.xml > table_grid.fo
xsltproc $STYLESHEET table_simple.xml > table_simple.fo


# regular footnotes
xsltproc $STYLESHEET footnotes.xml > footnotes.fo

xsltproc --stringparam footnote-style "traditional" \
$STYLESHEET footnotes.xml > footnotes_traditional.fo

xsltproc --stringparam footnote-placement "endnote" \
$STYLESHEET endnotes.xml > endnotes.fo

xsltproc $STYLESHEET citation.xml > citation.fo

xsltproc $STYLESHEET hyperlinks.xml > hyperlinks.fo

xsltproc --stringparam internal-link-type "page" \
$STYLESHEET hyperlinks.xml > hyperlinks_page.fo

xsltproc --stringparam internal-link-type "page-link" \
$STYLESHEET hyperlinks.xml > hyperlinks_link_page.fo

xsltproc $STYLESHEET admonition.xml > admonition.fo

xsltproc $STYLESHEET image.xml > image.fo

xsltproc $STYLESHEET figure.xml > image.fo

xsltproc $STYLESHEET sidebar.xml > sidebar.fo
xsltproc $STYLESHEET rubric.xml > rubric.fo
xsltproc $STYLESHEET epigraph.xml > epigraph.fo
xsltproc $STYLESHEET highlights.xml > highlights.fo
xsltproc $STYLESHEET pull_quote.xml > pull_quote.fo
xsltproc $STYLESHEET compound_paragraph.xml > compound_paragraph.fo
xsltproc $STYLESHEET topic.xml > topic.fo
xsltproc $STYLESHEET container.xml > container.fo

echo converting FO to PDF
FILES=`ls *\.fo`

# get the number of files
NUM_FILES=0
for THE_FILE in $FILES 
do
    let "NUM_FILES += 1"
done


FILES_DONE=1
for THE_FILE in $FILES 
do
    echo converting $THE_FILE to PDF File $FILES_DONE of $NUM_FILES
    FILENAME=`basename $THE_FILE .fo`
    NEW_FILE=${FILENAME}.pdf
    fop -fo $THE_FILE -pdf $NEW_FILE
    let "FILES_DONE += 1"
done
