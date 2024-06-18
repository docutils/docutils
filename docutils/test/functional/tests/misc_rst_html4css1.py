# Source and destination file names
test_source = "misc_rst_html4css1.txt"
test_destination = "misc_rst_html4css1.html"

# Keyword parameters passed to publish_file()
writer = "html4css1"
settings_overrides = {
    # test for encoded attribute value in optional stylesheet name,
    # 'stylesheet' setting, values are used verbatim
    'stylesheet': 'foo&bar.css, ../input/data/html4css1.css',
    # reset to avoid conflict with 'stylesheet'
    'stylesheet_path': '',
    # stylesheet_dirs not used with 'stylesheet'
    'stylesheet_dirs': ('functional/input/data', ),
    }
