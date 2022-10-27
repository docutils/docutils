# Keyword parameters passed to publish_file.
reader_name = 'standalone'
parser_name = 'rst'

# Settings.
settings_overrides['sectsubtitle_xform'] = True
settings_overrides['syntax_highlight'] = 'none'

# Source and destination file names.
test_source = "standalone_rst_latex.txt"
test_destination = "standalone_rst_latex.tex"

# Keyword parameters passed to publish_file.
writer_name = "latex"

# Settings
# use "smartquotes" transition:
settings_overrides['smart_quotes'] = True
settings_overrides['legacy_column_widths'] = True
settings_overrides['use_latex_citations'] = False
