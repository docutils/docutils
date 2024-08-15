# Source and destination file names
test_source = "footnotes.rst"
test_destination = "footnotes_html5.html"

# Keyword parameters passed to publish_file()
writer = "html5"
settings_overrides = {
    'sectsubtitle_xform': True,
    'footnote_references': 'superscript',
    'section_self_link': True,
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    'stylesheet_path': 'minimal.css,responsive.css',
}
