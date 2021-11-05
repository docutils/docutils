# Source and destination file names.
test_source = "misc_rst_html5.txt"
test_destination = "misc_rst_html5.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html5"

# Settings
# local copy of stylesheets:
# (Test runs in ``docutils/test/``, we need relative path from there.)
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')
settings_overrides['stylesheet_path'] = 'minimal.css,responsive.css'
settings_overrides['smart_quotes'] = 'yes'
settings_overrides['image_loading'] = 'embed'
settings_overrides['toc_backlinks'] = 'top'
settings_overrides['section_self_link'] = True
