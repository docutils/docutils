XSLFO_HOME=/Users/cejohnsonlouisville/Documents/docutils/paultremblay/xsl_fo
set -e
set -u
#  $Id: oo_unzip.sh 214 2011-09-24 01:47:45Z cejohnsonlouisville $
Usage() {
    echo Converts a restructured text document to FO, then to PDF 
    echo "docutils_to_fo.sh [options] <RST file>"
    echo --format: format resulting files
    echo --noclean: no not remove files created
    echo --pdf: create a PDF document
    echo --valid: validate the FO document
}
CLEAN='true'
FORMAT=
TEST=
TESTQ=
PDF='false'
VALID='false'
 while [ $# -gt 0 ]
 do
     case "$1" in
         
 	-) STDIN='true';    shift; break;;          # ====> standard in
 	-h)     Usage;exit 0;;                 # ==> Help message.
         --help) Usage;exit 0;;
         --verbose) VERBOSE='true';;
         --format) FORMAT='true';;
         --test) CLEAN='false';FORMAT='true';VALID='true';PDF='true';TEST='true';;
         --testq) CLEAN='false';FORMAT='true';VALID='true';PDF='true';;
         --noclean) CLEAN='false';;
         --pdf) PDF='true';;
         --valid) VALID='true';;
         --out) shift;OUT=$1;;
         -o) shift;OUT=$1;;
 	-*)     echo "Illegal command $1"; Usage; exit 1;;                 # ===> illegal help
          *)     break;;			# first argument
     esac   
     shift
 done
MAIN_XSL=${XSLFO_HOME}/docutils_to_fo.xsl

EXT=`echo "$1"|awk -F . '{print $NF}'` 
BASENAME=`basename $1 .${EXT}`
DIR=`dirname $1`
if [ "$DIR" == "$1" ]; then 
    DIRNAME="."
else
    DIRNAME=$DIR
fi

RAW_XML=${DIRNAME}/docutils_to_fo_resulting_rst.xml
FO_FILE=${DIRNAME}/docutils_to_fo_resulting_fo.fo

if [ "$FORMAT" == "true" ]; then
    rst2xml.py --strip-comments --trim-footnote-reference-space $1 | xmlformat.pl > $RAW_XML
elif [ "$TEST" == "true" ];then 
    echo just a test
else
    rst2xml.py --strip-comments --trim-footnote-reference-space $1 > $RAW_XML
fi

if [ "$FORMAT" == "true" ]; then
    xsltproc $MAIN_XSL $RAW_XML  > $FO_FILE
    xmlformat.pl -i $FO_FILE
else
    xsltproc $MAIN_XSL $RAW_XML  > $FO_FILE
fi

if [ "$VALID" == 'true' ]; then
    validate_fo.sh $FO_FILE
fi

if [ "$PDF" == 'true' ]; then
    PDF_FILE=${DIRNAME}/${BASENAME}.pdf
    fop -fo $FO_FILE -pdf ${PDF_FILE}
fi

if [ "$CLEAN" == "true" ]; then
    rm -f $FO_FILE
    rm -f $RAW_XML
    if [ "$TEST" == 'true' ]; then
        rm $PDF_FILE
    fi
fi
