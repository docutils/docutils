# Default settings for all tests.

settings_overrides = {
    '_disable_config': True,
    'report_level': 2,
    'halt_level': 5,
    'warning_stream': '',
    'input_encoding': 'utf-8',
    'embed_stylesheet': False,
    'auto_id_prefix': '%',
    # avoid "Pygments not found"
    'syntax_highlight': 'none'
}

# Source and destination file names.
test_source = "misc_rst_html4css1.txt"
test_destination = "misc_rst_html4css1.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html4css1"

# Settings
# test for encoded attribute value in optional stylesheet name,
# 'stylesheet' setting, values are used verbatim
settings_overrides['stylesheet'] = 'foo&bar.css, ../input/data/html4css1.css'
# reset to avoid conflict with 'stylesheet'
settings_overrides['stylesheet_path'] = ''
# stylesheet_dirs not used with 'stylesheet'
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')
