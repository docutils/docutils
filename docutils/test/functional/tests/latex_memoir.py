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
