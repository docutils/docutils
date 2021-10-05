# Source and destination file names.
test_source = "misc_rst_html4css1.txt"
test_destination = "misc_rst_html4css1.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html4css1"

# Settings
# test for encoded attribute value in optional stylesheet name:
settings_overrides['stylesheet'] = 'foo&bar.css, html4css1.css'
settings_overrides['stylesheet_path'] = ''
# local copy of stylesheets:
# (Test runs in ``docutils/test/``, we need relative path from there.)
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')
