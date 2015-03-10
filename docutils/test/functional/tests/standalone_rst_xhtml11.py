# Test the xhtml11 writer:

# common reader/parser/transform defaults for standalone tests
exec(open('functional/tests/_standalone_rst_defaults.py').read())

# Source and destination file names.
test_source = "standalone_rst_xhtml11.txt"
test_destination = "standalone_rst_xhtml11.xhtml"

# Keyword parameters passed to publish_file.
writer_name = "xhtml11"

# Settings
# local copy of default stylesheet:
# (test runs in ``docutils/test/``, we need relative path from there.)
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')
