# Test re-writing of stylesheet paths relative to output directory

# Source and destination file names.
test_source = "simple.txt"
test_destination = "stylesheet_path_html4css1.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html4css1"

# Settings
settings_overrides['stylesheet'] = ''
settings_overrides['stylesheet_path'] = 'data/ham.css,/missing.css'
settings_overrides['embed_stylesheet'] = False
