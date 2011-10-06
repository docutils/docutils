set -e
set -u
RST_COMMAND="rst2xml.py --strip-comments --trim-footnote-reference-space"

cp test_files/xsl/endnotes.xsl .
$RST_COMMAND test_files/endnotes.rst test_files/endnotes.xml
fop -xml test_files/endnotes.xml -xsl endnotes.xsl -pdf test_files/endnotes.pdf
rm endnotes.xsl

cp test_files/xsl/footnotes_traditional.xsl .
$RST_COMMAND test_files/footnotes.rst test_files/footnotes_traditional.xml
fop -xml test_files/footnotes_traditional.xml -xsl footnotes_traditional.xsl -pdf test_files/footnotes_traditional.pdf
rm footnotes_traditional.xsl

cp test_files/xsl/endnotes_traditional.xsl .
$RST_COMMAND test_files/endnotes.rst test_files/endnotes_traditional.xml
fop -xml test_files/endnotes_traditional.xml -xsl endnotes_traditional.xsl -pdf test_files/endnotes_traditional.pdf
rm endnotes_traditional.xsl


docutils_to_fo.sh  --test test_files/header_footer.rst
docutils_to_fo.sh  --test test_files/footnotes.rst
docutils_to_fo.sh  --test test_files/toc.rst
docutils_to_fo.sh  --test test_files/bibliographic_fields.rst

