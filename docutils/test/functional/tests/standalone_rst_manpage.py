# Source and destination file names
test_source = "standalone_rst_manpage.rst"
test_destination = "standalone_rst_manpage.man"

# Keyword parameters passed to publish_file()
writer = "manpage"
settings_overrides = {
    'sectsubtitle_xform': True,
    }
