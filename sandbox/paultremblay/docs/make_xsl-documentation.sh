# $Id$
set -e
set -u #  warns if a variable is not assigned.

PWD=`pwd`
PWD=`basename $PWD`
if [ "$PWD" != 'docs' ];then
    echo 'please run the script only in its own directory (docs)'
    exit 1
fi

xsltproc xsl/document_parameters.xsl ../docutilsToFo/xsl_fo/parameters.xsl > doc.xml
xml2txt.py doc.xml > rst/parameters.rst
rst2html.py  rst/parameters.rst >  html/parameters.html 
xsltproc xsl/document_stylesheet.xsl xsl/document_stylesheet.xsl > doc.xml
xml2txt.py doc.xml > rst/attribute_sets.rst
rst2html.py  rst/attribute_sets.rst >  html/attribute_sets.html 

rst2html.py rst/howto.rst > html/howto.html
# rm doc.xml

