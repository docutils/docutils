# Keyword parameters passed to publish_file.
reader_name = 'standalone'
parser_name = 'rst'

# Settings.
settings_overrides['sectsubtitle_xform'] = True
settings_overrides['syntax_highlight'] = 'none'

# Source and destination file names.
test_source = "standalone_rst_manpage.txt"
test_destination = "standalone_rst_manpage.man"

# Keyword parameters passed to publish_file.
writer_name = "manpage"
