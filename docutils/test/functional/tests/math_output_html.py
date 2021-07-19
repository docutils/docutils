# Source and destination file names.
test_source = "data/math.txt"
test_destination = "math_output_html.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html"

# Extra settings
settings_overrides['math_output'] = 'HTML math.css'
# local copy of stylesheets:
# (Test runs in ``docutils/test/``, we need relative path from there.)
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')

    
