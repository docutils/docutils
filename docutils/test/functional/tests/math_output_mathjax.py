# Source and destination file names.
test_source = "data/math.txt"
test_destination = "math_output_mathjax.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html"

# Settings
settings_overrides['math_output'] \
    = 'MathJax /usr/share/javascript/mathjax/MathJax.js'
# local copy of default stylesheet:
# (Test runs in ``docutils/test/``, we need relative path from there.)
settings_overrides['stylesheet_dirs'] = ('.', 'functional/input/data')
