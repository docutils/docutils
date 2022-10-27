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
test_source = "field_list.txt"
test_destination = "field_name_limit.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html"

# Settings
settings_overrides['field_name_limit'] = 0  # no limit
settings_overrides['docinfo_xform'] = False
# local copy of stylesheets:
# (Test runs in ``docutils/test/``, we need relative path from there.)
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')
