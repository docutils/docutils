XSLFO_HOME=/Users/cejohnsonlouisville/Documents/docutils/paultremblay/xsl_fo
if [ "$XSLFO_PDF" != "" ]; then
    PDF='true'
else
    PDF='false'
fi
set -e
set -u
Usage() {
    echo Converts a restructured text document to FO, then to PDF 
    echo "docutils_to_fo.sh [options] <RST file>"
    echo --format: format resulting files
    echo --noclean: no not remove files created
    echo --pdf: create a PDF document
    echo --valid: validate the FO document
    echo "-s | --stylesheet <stylesheet> : the stylesheet to use"
    echo "-o | --out: file to output to"
}
CLEAN='true'
FORMAT=
TEST=
TESTQ=
VALID='false'
STYLESHEET=
OUT=''
 while [ $# -gt 0 ]
 do
     case "$1" in
         
 	-) STDIN='true';    shift; break;;          # ====> standard in
 	-h)     Usage;exit 0;;                 # ==> Help message.
         --help) Usage;exit 0;;
         --verbose) VERBOSE='true';;
         --format) FORMAT='true';;
         --test) CLEAN='false';FORMAT='true';VALID='true';PDF='true';TEST='true';;
         --testq) CLEAN='false';VALID='true';FORMAT='true';;
         --noclean) CLEAN='false';;
         --pdf) PDF='true';;
         --valid) VALID='true';;
         --out) shift;OUT=$1;;
         -o) shift;OUT=$1;;
         --stylesheet) shift;STYLESHEET=$1;;
         -s) shift;STYLESHEET=$1;;
 	-*)     echo "Illegal command $1"; Usage; exit 1;;                 # ===> illegal help
          *)     break;;			# first argument
     esac   
     shift
 done

if [ "$STYLESHEET" == "" ]; then
    MAIN_XSL=${XSLFO_HOME}/docutils_to_fo.xsl
else
    MAIN_XSL=$STYLESHEET
fi

EXT=`echo "$1"|awk -F . '{print $NF}'` 
BASENAME=`basename $1 .${EXT}`
DIR=`dirname $1`
if [ "$DIR" == "$1" ]; then 
    DIRNAME="."
else
    DIRNAME=$DIR
fi

if [ ! -w "$DIRNAME" ]; then
    echo "has write permission"
fi

if [ "${OUT}" != "" ];then
    touch ${OUT} 
    rm -f ${OUT}
    EXT=`echo "${OUT}"|awk -F . '{print $NF}'` 
    if [ ${OUT} == ${EXT} ]; then
        OUT_BASE=${OUT}
    else
        OUT_BASE=`basename ${OUT} .${EXT}`
    fi
fi

if [ "${OUT}" != "" ]; then
    RAW_XML=${OUT_BASE}.xml
else
    RAW_XML=${DIRNAME}/${BASENAME}.xml
fi

if [ "${OUT}" != "" ]; then
    FO_FILE=${OUT_BASE}.fo
else
    FO_FILE=${DIRNAME}/${BASENAME}.fo
fi

rst2xml.py --strip-comments --trim-footnote-reference-space $1  $RAW_XML

if [ "$FORMAT" == "true" ]; then
    xmlformat.pl -i  $RAW_XML
fi

xsltproc --output $FO_FILE $MAIN_XSL $RAW_XML  

if [ "$FORMAT" == "true" ]; then
    xmlformat.pl -i $FO_FILE
fi

if [ "$VALID" == 'true' ]; then
    validate_fo.sh $FO_FILE
fi

if [ "$PDF" == 'true' ]; then
    PDF_FILE=${DIRNAME}/${BASENAME}.pdf
    fop -fo $FO_FILE -pdf ${PDF_FILE}
fi

if [ "$CLEAN" == "true" ]; then
    rm -f $RAW_XML
    if [ "$TEST" == 'true' ]; then
        rm $PDF_FILE
    fi
fi
