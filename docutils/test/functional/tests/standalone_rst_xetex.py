with open('functional/tests/_standalone_rst_defaults.py') as _f:
    exec(_f.read())

# Source and destination file names.
test_source = "standalone_rst_xetex.txt"
test_destination = "standalone_rst_xetex.tex"

# Keyword parameters passed to publish_file.
writer_name = "xetex"

# Settings
# use "smartquotes" transition:
settings_overrides['smart_quotes'] = True
# use docutils.sty and up-to-date class functions:
settings_overrides['stylesheet'] = 'docutils'
# Test the ToC generation by Docutils:
settings_overrides['use_latex_toc'] = False
settings_overrides['legacy_column_widths'] = True
settings_overrides['use_latex_citations'] = False
