# Source and destination file names:
test_source = 'standalone_rst_s5_html.txt'
test_destination = 'standalone_rst_s5_html_1.html'

# Keyword parameters passed to publish_file:
writer = 's5_html'
settings_overrides = {
    'sectsubtitle_xform': True,
    'theme_url': 'ui/small-black',  # don't attempt to copy theme files
    'theme': None,  # explicitely reset overriding option
                    # (cf. ../../../docs/user/config.html#override)
    # local copy of default stylesheet:
    'stylesheet_path': 'functional/input/data/html4css1.css',
    }
