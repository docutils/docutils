set -e
set -u
RST_COMMAND="rst2xml.py --strip-comments --trim-footnote-reference-space"
docutils_to_fo.sh  --test test_files/header_footer.rst
docutils_to_fo.sh  --test test_files/footnotes.rst
$RST_COMMAND test_files/endnotes.rst test_files/endnotes.xml
fop -xml test_files/endnotes.xml -xsl test_files/endnotes.xsl -pdf test_files/endnotes.pdf
docutils_to_fo.sh  --test test_files/toc.rst
docutils_to_fo.sh  --test test_files/bibliographic_fields.rst

