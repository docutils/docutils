# Source and destination file names
test_source = "standalone_rst_docutils_xml.rst"
test_destination = "standalone_rst_docutils_xml.xml"

# Keyword parameters passed to publish_file()
writer = "docutils_xml"
settings_overrides = {
    'sectsubtitle_xform': True,
    # format output with indents and newlines
    'indents': True,
    'validate': True,  # check conformance to Docutils Generic DTD
    }
