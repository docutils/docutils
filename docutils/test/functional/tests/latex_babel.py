# Source and destination file names.
test_source = "latex_babel.txt"
test_destination = "latex_babel.tex"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "latex"

# Extra settings we need
settings_overrides['legacy_column_widths'] = False
settings_overrides['use_latex_citations'] = True
