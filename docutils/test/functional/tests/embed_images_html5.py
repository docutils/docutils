with open('functional/tests/_standalone_rst_defaults.py') as _f:
    exec(_f.read())

# Source and destination file names.
test_source = "embed_images.txt"
test_destination = "embed_images_html5.html"

# Keyword parameters passed to publish_file.
writer_name = "html5"

# Settings:
settings_overrides['smart_quotes'] = 'yes'
settings_overrides['embed_images'] = 'yes'

