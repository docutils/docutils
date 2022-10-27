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
