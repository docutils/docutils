# Source and destination file names.
test_source = "data/math.txt"
test_destination = "mathml_math.xhtml"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html"

# Extra setting

settings_overrides['math_output'] = 'MathML'
