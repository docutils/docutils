# Source and destination file names:
test_source = 'standalone_rst_s5_html.rst'
test_destination = 'standalone_rst_s5_html_2.html'

# Keyword parameters passed to publish_file:
writer = 's5_html'
settings_overrides = {
    'sectsubtitle_xform': True,
    # local copy of default stylesheet:
    'stylesheet_path': 'functional/input/data/html4css1.css',
    'current_slide': 1,
    'report_level': 3,  # suppress "can't copy themes" warning
    }
