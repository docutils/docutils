# Source and destination file names
test_source = "cyrillic.txt"
test_destination = "cyrillic.tex"

# Keyword parameters passed to publish_file()
writer = "latex"
settings_overrides = {
    'font_encoding': 'T1,T2A',
    'stylesheet': 'cmlgc',
    'language_code': 'ru',
    'smart_quotes': True,
    'legacy_column_widths': False,
    'use_latex_citations': True,
    }
