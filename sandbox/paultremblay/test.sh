set -e
set -u
# RST_COMMAND="rst2xml.py --strip-comments --trim-footnote-reference-space"
# TEST_COMMAND="docutils_to_fo.sh --noclean --format --valid --pdf --test --strict"

files=`find functional\
    \! -name 'standard.txt'\
    \! -name 'standalone_rst_html4css1.txt'\
    \! -name 'dangerous.txt'\
    \! -name 'custom_roles.txt'\
    \! -name 'custom_roles_latex.txt'\
    \! -name 'standalone_rst_latex.txt'\
    \! -name 'standalone_rst_pseudoxml.txt'\
    \! -name 'standalone_rst_s5_html.txt'\
    \! -name 'latex.txt'\
    \! -name 'list_table.txt'\
    \! -name 'list_table.txt'\
    \! -name 'header_footer.txt'\
    \! -name 'standalone_rst_xetex.txt'\
    -name '*txt'`

# header_footer.txt has no body, and causes fop to throw exception
# serious error with unicode.txt: it crashes with 00AD
# standalone_rst_xetex.txt contains latex raw

for the_file in $files
do
    echo
    echo ========================================================
    echo working on file \"$the_file\"
    DIR=`dirname $the_file`
    if [ "$DIR" == "$the_file" ]; then 
        DIRNAME="."
    else
        DIRNAME=$DIR
    fi
    PARENT_DIR=`basename $DIRNAME`

    docutils_to_fo.sh --test $the_file
    echo ========================================================
done

# special cases for default
# get rid of strict 
echo ========================================================
echo working on standard.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/data/standard.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/standalone_rst_html4css1.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/dangerous.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/data/custom_roles.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/data/custom_roles_latex.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/standalone_rst_latex.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/standalone_rst_pseudoxml.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/standalone_rst_s5_html.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/data/latex.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/data/list_table.txt
docutils_to_fo.sh --noclean --format --valid --pdf   functional/input/standalone_rst_xetex.txt


# no body, so needs a special xsl stylesheet
docutils_to_fo.sh --noclean --format --valid --pdf -s test_files/xsl/no_body.xsl functional/input/data/header_footer.txt



files=`find test_files   -name '*rst'`
for the_file in $files
do
    DIR=`dirname $the_file`
    if [ "$DIR" == "$the_file" ]; then 
        DIRNAME="."
    else
        DIRNAME=$DIR
    fi
    PARENT_DIR=`basename $DIRNAME`

    if [ "$PARENT_DIR" != "custom" ]; then
        echo $the_file
        docutils_to_fo.sh --test $the_file
    fi
done

docutils_to_fo.sh --test -s test_files/xsl/endnotes.xsl test_files/custom/endnotes.rst
docutils_to_fo.sh --test -s test_files/xsl/footnotes_traditional.xsl test_files/footnotes.rst
docutils_to_fo.sh --test -s test_files/xsl/endnotes_traditional.xsl test_files/custom/endnotes.rst


