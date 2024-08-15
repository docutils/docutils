# Source and destination file names
test_source = "misc_rst_html5.rst"
test_destination = "misc_rst_html5.html"

# Keyword parameters passed to publish_file()
writer = "html5"
settings_overrides = {
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    'stylesheet_path': 'minimal.css,responsive.css',
    'smart_quotes': 'yes',
    'image_loading': 'embed',
    'toc_backlinks': 'top',
    'section_self_link': True,
    }
