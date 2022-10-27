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

# Source and destination file names.
test_source = "standalone_rst_xetex.txt"
test_destination = "standalone_rst_xetex.tex"

# Keyword parameters passed to publish_file.
writer_name = "xetex"

# Settings
# use "smartquotes" transition:
settings_overrides['smart_quotes'] = True
# use docutils.sty and up-to-date class functions:
settings_overrides['stylesheet'] = 'docutils'
# Test the ToC generation by Docutils:
settings_overrides['use_latex_toc'] = False
settings_overrides['legacy_column_widths'] = True
settings_overrides['use_latex_citations'] = False
