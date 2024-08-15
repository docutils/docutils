# Source and destination file names
test_source = "compact_lists.rst"
test_destination = "compact_lists.html"

# Keyword parameters passed to publish_file()
writer = "html4"
settings_overrides = {
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
}
