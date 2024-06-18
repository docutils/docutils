# Source and destination file names
test_source = "standalone_rst_pseudoxml.txt"
test_destination = "standalone_rst_pseudoxml.txt"

# Keyword parameters passed to publish_file()
writer = "pseudoxml"
settings_overrides = {
    'sectsubtitle_xform': True,
    # enable INFO-level system messages in this test:
    'report_level': 1,
    }
