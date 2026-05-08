# Source and destination file names
test_source = "data/footnote-references.rst"
test_destination = "footnote-references_latex.tex"

# Keyword parameters passed to publish_file()
writer = "latex"
settings_overrides = {
    'latex_footnotes': True,  # use LaTeX \footnote cmd
    'legacy_column_widths': False,
    'use_latex_citations': True,  # avoid FutureWarning
    }
