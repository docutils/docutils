# Source and destination file names
test_source = "standalone_rst_xetex.txt"
test_destination = "standalone_rst_xetex.tex"

# Keyword parameters passed to publish_file()
writer = "xetex"
settings_overrides = {
    'sectsubtitle_xform': True,
    # use "smartquotes" transition:
    'smart_quotes': True,
    # use docutils.sty and up-to-date class functions:
    'stylesheet': 'docutils',
    # Test the ToC generation by Docutils:
    'use_latex_toc': False,
    'legacy_column_widths': True,
    'use_latex_citations': False,
    }
