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
