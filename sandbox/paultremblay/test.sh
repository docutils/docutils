set -e
set -u
# RST_COMMAND="rst2xml.py --strip-comments --trim-footnote-reference-space"
# TEST_COMMAND="docutils_to_fo.sh --noclean --format --valid --pdf --test --strict"



files=`find test_files -name '*rst'`
for the_file in $files
do
    echo $the_file
    DIR=`dirname $the_file`
    if [ "$DIR" == "$the_file" ]; then 
        DIRNAME="."
    else
        DIRNAME=$DIR
    fi
    PARENT_DIR=`basename $DIRNAME`

    if [ "$PARENT_DIR" != "custom" ]; then
        docutils_to_fo.sh --test $the_file
    fi
done

docutils_to_fo.sh --test -s test_files/xsl/endnotes.xsl test_files/custom/endnotes.rst
docutils_to_fo.sh --test -s test_files/xsl/footnotes_traditional.xsl test_files/footnotes.rst
docutils_to_fo.sh --test -s test_files/xsl/endnotes_traditional.xsl test_files/custom/endnotes.rst
