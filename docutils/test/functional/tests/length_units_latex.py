# Source and destination file names
test_source = "length_units.rst"
test_destination = "length_units_latex.tex"

# Keyword parameters passed to publish_file()
writer = "latex"
settings_overrides = {
    'legacy_column_widths': False,
    'use_latex_citations': True,
    'stylesheet': 'docutils,geometry,nohyperref',
    'documentoptions': 'a4paper,landscape',
    }
