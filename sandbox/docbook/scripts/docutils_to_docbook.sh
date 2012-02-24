if [ "$DOCBOOK_XSL" == "" ];then
    echo Please set DOCBOOK_XSL to the directory with the stylesheets
    echo script now quiting
    exit 1
fi
if [ "$FOPCONF" == "" ];then
    FOPCONF=
fi
if [ "$XSLFO_PDF" != "" ]; then
    PDF='true'
else
    PDF='false'
fi
set -e
set -u
Usage() {
    echo Converts a restructured text document to Docbook, then to PDF \(if appropriate\) 
    echo "docutils_to_docbook.sh [options] <RST file>"
    echo --format: format resulting files
    echo --noclean: no not remove files created
    echo --pdf: create a PDF document
    echo --valid: validate the FO document
    echo --strict: quit when a template does not match \(or other error\)
    echo --asciiml: convert math in the math element to mathml
    echo --fo: convert to FO
    echo --html: convert to HTML
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
STRICT=''
ASCIIML='false'
FO='false'
HTML='false'
 while [ $# -gt 0 ]
 do
     case "$1" in
         
 	-) STDIN='true';    shift; break;;          # ====> standard in
 	-h)     Usage;exit 0;;                 # ==> Help message.
         --help) Usage;exit 0;;
         --verbose) VERBOSE='true';;
         --format) FORMAT='true';;
         --test) CLEAN='false';FORMAT='true';VALID='true';TEST='true';STRICT='true';PDF='true';;
         --testq) CLEAN='false';VALID='true';FORMAT='true';;
         --noclean) CLEAN='false';;
         --pdf) PDF='true';;
         --valid) VALID='true';;
         --strict) STRICT='true';;
         --asciiml) ASCIIML='true';;
         --fo) FO='true';;
         --html) HTML='true';;
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
    MAIN_XSL=${DOCBOOK_XSL}/docutils_to_docbook.xsl
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
    DOC_FILE=${OUT_BASE}_docbook.xml
else
    DOC_FILE=${DIRNAME}/${BASENAME}_docbook.xml
fi

if [ "$ASCIIML" == 'true' ]; then
    rst2xml.py --strip-comments --trim-footnote-reference-space --no-doctype $1\
        | rstxml2xml.py  >  $RAW_XML
else
    rst2xml.py --strip-comments --trim-footnote-reference-space --no-doctype $1 >  $RAW_XML
fi

if [ "$FORMAT" == "true" ]; then
    xmlformat.pl -i  $RAW_XML
fi

if [ "$STRICT" == "" ]; then
    xsltproc --output $DOC_FILE "$MAIN_XSL" $RAW_XML  
else
    xsltproc --stringparam strict true --output $DOC_FILE "$MAIN_XSL" $RAW_XML  
fi


if [ "$FORMAT" == "true" ]; then
    xmlformat.pl -i $DOC_FILE
fi

if [ "$VALID" == 'true' ]; then
    java  -jar $JING_DIR/bin/jing.jar -i ${VALIDATE_HOME}/relax/docbook.rng $DOC_FILE 
fi

FO_FILE=${DIRNAME}/${BASENAME}.fo

if [ "$PDF" == 'true' ]; then
    PDF_FILE=${DIRNAME}/${BASENAME}.pdf
    if [ "$FOPCONF" != '' ]; then
        fop  -c $FOPCONF -fo $FO_FILE -pdf ${PDF_FILE}
    else
        fop -fo  $FO_FILE -pdf ${PDF_FILE}
    fi
fi

if [ "$OUT" == "" ]  && [ "$PDF" == 'false' ] && [ "$FO" == "true" ];then 
    cat $FO_FILE
fi

if [ "$OUT" == "" ]  && [ "$PDF" == 'false' ] && [ "$HTML" == "true" ];then 
    xsltproc http://50.56.245.89/xsl-ns/xhtml/docbook.xsl $DOC_FILE
fi

if [ "$CLEAN" == "true" ]; then
    rm -f $RAW_XML
    if [ "$TEST" == 'true' ]; then
        rm $PDF_FILE
    fi
fi
