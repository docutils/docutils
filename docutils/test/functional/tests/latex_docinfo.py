# Source and destination file names
test_source = "latex_docinfo.rst"
test_destination = "latex_docinfo.tex"

# Keyword parameters passed to publish_file()
writer = "latex"
settings_overrides = {
    'use_latex_docinfo': 1,
    'legacy_column_widths': False,
    'use_latex_citations': True,
    }
