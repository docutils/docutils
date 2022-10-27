# Keyword parameters passed to publish_file.
reader_name = 'standalone'
parser_name = 'rst'

# Settings.
settings_overrides['sectsubtitle_xform'] = True
settings_overrides['syntax_highlight'] = 'none'

# Source and destination file names.
test_source = "standalone_rst_html4css1.txt"
test_destination = "standalone_rst_html4css1.html"

# Keyword parameters passed to publish_file.
writer_name = "html4css1"

# Settings:
# local copy of stylesheets:
# (Test runs in ``docutils/test/``, we need relative path from there.)
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')
