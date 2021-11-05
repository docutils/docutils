# Source and destination file names.
test_source = "latex_leavevmode.txt"
test_destination = "latex_leavevmode.tex"

# Keyword parameters passed to publish_file.
writer_name = "latex"

# Settings
# use "smartquotes" transition:
settings_overrides['smart_quotes'] = True
settings_overrides['legacy_column_widths'] = True
settings_overrides['use_latex_citations'] = False
