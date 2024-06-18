# Source and destination file names
test_source = "data/math.txt"
test_destination = "math_output_mathml.html"

# Keyword parameters passed to publish_file()
writer = "html5"
settings_overrides = {
    'math_output': 'MathML',
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    }
