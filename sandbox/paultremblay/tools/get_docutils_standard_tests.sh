set -e
set -u
Usage() {
    echo Gets documents from docutils 
}

while [ $# -gt 0 ]
do
     case "$1" in
         
 	-) STDIN='true';    shift; break;;          # ====> standard in
 	-h)     Usage;exit 0;;                 # ==> Help message.
         --help) UsaET=$1;;
 	-*)     echo "Illegal command $1"; Usage; exit 1;;                 # ===> illegal help
          *)     break;;			# first argument
     esac   
     shift
done

DIR=$1

mkdir docs/user 2> /dev/null || echo
mkdir docs/user/images 2> /dev/null || echo
cp ${DIR}/docs/user/images/*  docs/user/images
cp -R ${DIR}/docs/user/rst docs/user
cp -R ${DIR}/test/functional .
