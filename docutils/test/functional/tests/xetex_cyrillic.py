# Source and destination file names.
test_source = "cyrillic.txt"
test_destination = "xetex-cyrillic.tex"

# Keyword parameters passed to publish_file.
writer_name = "xetex"

# Settings
settings_overrides['language_code'] = 'ru'
# override the automatic addition of "unicode" option for "russian" language
settings_overrides['hyperref_options'] = 'unicode=false'
