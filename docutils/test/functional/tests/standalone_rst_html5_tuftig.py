with open('functional/tests/_standalone_rst_defaults.py') as _f:
    exec(_f.read())

# Source and destination file names.
test_source = "rst_html5_tuftig.txt"
test_destination = "rst_html5_tuftig.html"

# Keyword parameters passed to publish_file.
writer_name = "html5"

# Settings:
settings_overrides['smart_quotes'] = 'yes'
settings_overrides['footnote_references'] = 'superscript'
settings_overrides['table_style'] = 'booktabs numbered captionbelow'
# local copy of stylesheets:
# (Test runs in ``docutils/test/``, we need relative path from there.)
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')
settings_overrides['stylesheet_path'] = 'minimal.css, tuftig.css'
