# Source and destination file names
test_source = "data/math.rst"
test_destination = "math_output_mathjax.html"

# Keyword parameters passed to publish_file()
writer = "html4"
settings_overrides = {
    'math_output': 'MathJax /usr/share/javascript/mathjax/MathJax.js',
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    }
