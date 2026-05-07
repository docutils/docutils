# Source and destination file names
test_source = "footnotes.rst"
test_destination = "footnotes_latex.tex"

# Keyword parameters passed to publish_file()
writer = "latex"
settings_overrides = {
    'footnote_references': 'brackets',
    'legacy_column_widths': False,
    'use_latex_citations': True,
}
