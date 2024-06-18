# Source and destination file names
test_source = "data/math.txt"
test_destination = "math_output_html.html"

# Keyword parameters passed to publish_file()
writer = "html4"
settings_overrides = {
    'math_output': 'HTML math.css',
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    }
