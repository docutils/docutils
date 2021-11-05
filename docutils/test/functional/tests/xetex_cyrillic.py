# Source and destination file names.
test_source = "cyrillic.txt"
test_destination = "xetex-cyrillic.tex"

# Keyword parameters passed to publish_file.
writer_name = "xetex"

# Settings
settings_overrides['language_code'] = 'ru'
# use "smartquotes" transition:
settings_overrides['smart_quotes'] = True
settings_overrides['legacy_column_widths'] = True
settings_overrides['use_latex_citations'] = False
