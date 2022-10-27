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
test_source = "dangerous.txt"
test_destination = "dangerous.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html"

# Settings
settings_overrides['file_insertion_enabled'] = False
settings_overrides['raw_enabled'] = False
# local copy of default stylesheet:
settings_overrides['stylesheet_path'] = 'functional/input/data/html4css1.css'
