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
$STYLESHEET simple_header_footer.xml   >  header_footer.fo 
xmlformat.pl -i header_footer.fo

# simple page, with headers and footers, no inline; just paragraphs
# the --suppress-page-num has no effect
xsltproc --stringparam page-layout "simple" \
--stringparam suppress-first-page-header "True" \
$STYLESHEET simple_header_footer.xml    > header_footer2.fo 
xmlformat.pl -i header_footer2.fo

# first page, with headers and footers, no inline; just paragraphs
xsltproc --stringparam page-layout "simple" \
$STYLESHEET simple_header_footer.xml   >  first_page_header_footer.fo 
xmlformat.pl -i first_page_header_footer.fo

# first page, with headers and footers, no inline; just paragraphs
# suppress first page header and footer
xsltproc --stringparam page-layout "first" \
--stringparam suppress-first-page-header "True" \
--stringparam suppress-first-page-footer "True" \
$STYLESHEET simple_header_footer.xml   >  first_suppress_header__footer.fo 
xmlformat.pl -i first_suppress_header__footer.fo

# odd-even page, with headers and footers, no inline; just paragraphs
xsltproc --stringparam page-layout "odd-even" \
$STYLESHEET simple_header_footer.xml   >  odd_even_page_header_footer.fo 
xmlformat.pl -i odd_even_page_header_footer.fo

# first-odd-even page, with headers and footers, no inline; just paragraphs
xsltproc --stringparam page-layout "first-odd-even" \
$STYLESHEET simple_header_footer.xml   >  first_odd_even_page_header_footer.fo 
xmlformat.pl -i first_odd_even_page_header_footer.fo

# first-odd-even page, with headers and footers, no inline; just paragraphs
# suppress first header and footer
xsltproc --stringparam page-layout "first-odd-even" \
--stringparam suppress-first-page-header "True" \
--stringparam suppress-first-page-footer "True" \
$STYLESHEET simple_header_footer.xml   >  first_odd_even_page_header_footer_suppress_first.fo 
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

echo converting FO to PDF
FILES=`ls *\.fo`

# get the number of files
NUM_FILES=0
for THE_FILE in $FILES 
do
    let "NUM_FILES += 1"
done
echo $NUM_FILES


FILES_DONE=1
for THE_FILE in $FILES 
do
    echo converting $THE_FILE to PDF File $FILES_DONE of $NUM_FILES
    FILENAME=`basename $THE_FILE .fo`
    NEW_FILE=${FILENAME}.pdf
    fop -fo $THE_FILE -pdf $NEW_FILE
    let "FILES_DONE += 1"
done
