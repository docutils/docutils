set -e
set -u
docutils_to_fo.sh  --test test_files/header_footer.rst
docutils_to_fo.sh  --test test_files/footnotes.rst
docutils_to_fo.sh  --test test_files/toc.rst
