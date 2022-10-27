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

# Keyword parameters passed to publish_file.
reader_name = 'standalone'
parser_name = 'rst'

# Settings.
settings_overrides['sectsubtitle_xform'] = True
settings_overrides['syntax_highlight'] = 'none'

# Source and destination file names:
test_source = 'standalone_rst_s5_html.txt'
test_destination = 'standalone_rst_s5_html_1.html'

# Keyword parameters passed to publish_file:
writer_name = 's5_html'

# Settings:
settings_overrides['theme'] = 'small-black'
# local copy of default stylesheet:
settings_overrides['stylesheet_path'] = 'functional/input/data/html4css1.css'
