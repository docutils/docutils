# Source and destination file names
test_source = "cyrillic.rst"
test_destination = "xetex-cyrillic.tex"

# Keyword parameters passed to publish_file()
writer = "xetex"
settings_overrides = {
    'language_code': 'ru',
    # use "smartquotes" transition:
    'smart_quotes': True,
    'legacy_column_widths': True,
    'use_latex_citations': False,
    }
