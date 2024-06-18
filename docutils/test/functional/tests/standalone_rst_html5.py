# Source and destination file names
test_source = "standalone_rst_html5.txt"
test_destination = "standalone_rst_html5.html"

# Keyword parameters passed to publish_file()
writer = "html5"
settings_overrides = {
    'sectsubtitle_xform': True,
    # "smart" quotes:
    # 'smart_quotes': 'yes',
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    }
