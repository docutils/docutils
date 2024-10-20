# Source and destination file names
test_source = "standalone_rst_docutils_xml.rst"
test_destination = "standalone_rst_docutils_xml.xml"

# Keyword parameters passed to publish_file()
writer = "docutils_xml"
settings_overrides = {
    'sectsubtitle_xform': True,
    'indents': True,  # format output with indents and newlines
    'validate': True,  # check conformance to Docutils Generic DTD
    }
