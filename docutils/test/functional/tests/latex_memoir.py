# Source and destination file names.
test_source = "standalone_rst_latex.txt"
test_destination = "latex_memoir.tex"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "latex"

# Extra setting we need

settings_overrides['use_latex_docinfo'] = 1
settings_overrides['documentclass'] = "memoir"
settings_overrides['template'] = "titlingpage.tex"

# test the legacy class functions (since 0.18 default is False )
settings_overrides['legacy_class_functions'] = True
settings_overrides['legacy_column_widths'] = True
settings_overrides['use_latex_citations'] = False
