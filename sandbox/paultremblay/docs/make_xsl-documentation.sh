# $Id$
set -e
set -u #  warns if a variable is not assigned.

PWD=`pwd`
PWD=`basename $PWD`
if [ "$PWD" != 'docs' ];then
    echo 'please run the script only in its own directory (docs)'
    exit 1
fi

echo '#####################'
echo XSL-FO Documentation
echo '#####################'
echo

echo '==============='
echo Attribute Sets
echo '==============='


xsltproc xsl/document_stylesheet.xsl ../xsl_fo/option_list.xsl

