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
test_source = "latex_literal_block.txt"
test_destination = "latex_literal_block_verbatimtab.tex"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "latex"

# Extra setting we need
settings_overrides['syntax_highlight'] = 'none'
settings_overrides['stylesheet'] = 'docutils'
settings_overrides['legacy_column_widths'] = True
settings_overrides['use_latex_citations'] = False

settings_overrides['literal_block_env'] = 'verbatimtab'
