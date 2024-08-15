# Source and destination file names
test_source = "standalone_rst_html4css1.rst"
test_destination = "standalone_rst_html4css1.html"

# Keyword parameters passed to publish_file()
writer = "html4css1"
settings_overrides = {
    'sectsubtitle_xform': True,
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    }
