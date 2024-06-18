# Test the Docutils XML parser
#
# Source is the output sample of the XML writer test.
# We expect a round-trip without changes.

# Source and destination file names
test_source = '../expected/standalone_rst_docutils_xml.xml'
test_destination = 'standalone_rst_docutils_xml.xml'

# Keyword parameters passed to publish_file()
parser = 'xml'
writer = 'xml'
settings_overrides = {'indents': True}
