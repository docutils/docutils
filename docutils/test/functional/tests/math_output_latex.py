# Source and destination file names
test_source = "data/math.txt"
test_destination = "math_output_latex.html"

# Keyword parameters passed to publish_file()
writer = "html4"
settings_overrides = {
    'math_output': 'latex',
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data',),
    }
