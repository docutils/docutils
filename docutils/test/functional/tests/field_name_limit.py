# Source and destination file names.
test_source = "field_list.txt"
test_destination = "field_name_limit.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html"

# Settings
settings_overrides['field_name_limit'] = 0 # no limit
settings_overrides['docinfo_xform'] = False
# local copy of stylesheets:
# (Test runs in ``docutils/test/``, we need relative path from there.)
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')
