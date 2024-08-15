# Source and destination file names
test_source = "field_list.rst"
test_destination = "field_name_limit.html"

# Keyword parameters passed to publish_file()
writer = "html4"
settings_overrides = {
    'field_name_limit': 0,  # no limit
    'docinfo_xform': False,
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    }
