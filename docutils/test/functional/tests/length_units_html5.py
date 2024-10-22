import unittest
# PIL is optional but required with the "image" directive's "scale" option
from docutils.parsers.rst.directives.images import PIL
if PIL is None:
    raise unittest.SkipTest('requires PIL')
else:
    del unittest, PIL  # clean up namespace

# Source and destination file names
test_source = "length_units.rst"
test_destination = "length_units_html5.html"

# Keyword parameters passed to publish_file()
writer = "html5"
settings_overrides = {
    # location of stylesheets (relative to ``docutils/test/``)
    'stylesheet_dirs': ('functional/input/data', ),
    }
