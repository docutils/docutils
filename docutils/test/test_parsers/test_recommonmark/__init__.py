"""Optional tests with 3rd party CommonMark parser"""

import unittest
try:
    import docutils.parsers.recommonmark_wrapper  # noqa: F401
except ImportError as err:
    raise unittest.SkipTest(err)
