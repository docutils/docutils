"""Optional tests with 3rd party CommonMark parser"""

import unittest

from test import DocutilsTestSupport  # NoQA: F401  # before importing docutils!

import docutils.parsers

# TODO: test with alternative CommonMark parsers?
md_parser = 'recommonmark'
# md_parser = 'pycmark'
# md_parser = 'myst'
try:
    import recommonmark
    docutils.parsers.get_parser_class(md_parser)
except ImportError:
    raise unittest.SkipTest(f'Cannot test "{md_parser}". '
                            'Parser not found.')
else:
    if md_parser == 'recommonmark' and recommonmark.__version__ < '0.6.0':
        raise unittest.SkipTest('"recommonmark" parser too old, skip tests')
