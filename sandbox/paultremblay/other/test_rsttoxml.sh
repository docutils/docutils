set -e -u
TEST_DIR=/Users/cejohnsonlouisville/Documents/docutils/trunk/sandbox/paultremblay/functional
rst_files=`find ${TEST_DIR} -name '*txt'`
for rst_file in $rst_files
do
    echo $rst_file
    rsttoxml.py $rst_file | xmlformat.pl > my_rst.xml
    rst2xml.py --no-xml-declaration --no-doctype --no-generator --trim-footnote-reference-space  $rst_file | xmlformat.pl >  standard_rst.xml
    diff my_rst.xml standard_rst.xml 
done


TEST_DIR=/Users/cejohnsonlouisville/Documents/docutils/trunk/sandbox/paultremblay/test_files/
rst_files=`ls ${TEST_DIR}/*rst`
for rst_file in $rst_files
do
    echo $rst_file
    rsttoxml.py $rst_file | xmlformat.pl > my_rst.xml
    rst2xml.py --no-xml-declaration --no-doctype --no-generator --trim-footnote-reference-space  $rst_file | xmlformat.pl >  standard_rst.xml
    diff my_rst.xml standard_rst.xml 
done
