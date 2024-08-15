# Source and destination file names
test_source = "standalone_rst_latex.rst"
test_destination = "latex_memoir.tex"

# Keyword parameters passed to publish_file()
writer = "latex"
settings_overrides = {
    'use_latex_docinfo': 1,
    'documentclass': "memoir",
    'template': "titlingpage.tex",
    # test the legacy class functions (since 0.18 default is False )
    'legacy_class_functions': True,
    'legacy_column_widths': True,
    'use_latex_citations': False,
    }
