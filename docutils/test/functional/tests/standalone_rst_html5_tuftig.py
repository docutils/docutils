# Source and destination file names
test_source = "rst_html5_tuftig.rst"
test_destination = "rst_html5_tuftig.html"

# Keyword parameters passed to publish_file()
writer = "html5"
settings_overrides = {
    'sectsubtitle_xform': True,
    'smart_quotes': 'yes',
    'footnote_references': 'superscript',
    'table_style': 'booktabs numbered captionbelow',
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    'stylesheet_path': 'minimal.css, tuftig.css',
    }
