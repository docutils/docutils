# Source and destination file names
test_source = "latex_leavevmode.rst"
test_destination = "latex_leavevmode.tex"

# Keyword parameters passed to publish_file()
writer = "latex"
settings_overrides = {
    'smart_quotes': True,
    'legacy_column_widths': True,
    'use_latex_citations': False,
    }
