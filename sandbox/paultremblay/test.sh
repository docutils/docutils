set -e
set -u
# RST_COMMAND="rst2xml.py --strip-comments --trim-footnote-reference-space"
# TEST_COMMAND="docutils_to_fo.sh --noclean --format --valid --pdf --test --strict"

docutils_to_fo.sh --test -s test_files/xsl/endnotes.xsl test_files/endnotes.rst
docutils_to_fo.sh --test -s test_files/xsl/footnotes_traditional.xsl test_files/footnotes.rst
docutils_to_fo.sh --test -s test_files/xsl/endnotes_traditional.xsl test_files/endnotes.rst

docutils_to_fo.sh  --test test_files/header_footer.rst
docutils_to_fo.sh  --test test_files/footnotes.rst
docutils_to_fo.sh  --test test_files/toc.rst
docutils_to_fo.sh  --test test_files/bibliographic_fields.rst

