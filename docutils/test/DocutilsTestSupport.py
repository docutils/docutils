# $Id$
# Authors: David Goodger <goodger@python.org>;
#          Garth Kidd <garth@deadlybloodyserious.com>
# Copyright: This module has been placed in the public domain.

"""
Exports the following:

:Modules:
    - `utils` is 'docutils.utils'
    - `tableparser` is 'docutils.parsers.rst.tableparser'

:Classes:
    - `StandardTestCase`
    - `CustomTestCase`
    - `CustomTestSuite`
    - `ParserTestCase`
    - `ParserTestSuite`
"""
__docformat__ = 'reStructuredText'

import difflib
import inspect
import os
import sys
import unittest

testroot = os.path.abspath(os.path.dirname(__file__) or os.curdir)
os.chdir(testroot)
sys.path.insert(0, os.path.normpath(os.path.join(testroot, '..')))
sys.path.insert(0, testroot)

from docutils import frontend, utils   # NoQA: E402
from docutils.parsers import rst   # NoQA: E402
from docutils.parsers.rst import roles   # NoQA: E402
from docutils.statemachine import StringList   # NoQA: E402


# Hack to make repr(StringList) look like repr(list):
StringList.__repr__ = StringList.__str__


StandardTestCase = unittest.TestCase


class CustomTestCase(unittest.TestCase):

    """
    Helper class, providing extended functionality over unittest.TestCase.

    See the compare_output method and the parameter list of __init__.

    Note: the modified signature is incompatible with
    the "pytest" and "nose" frameworks.
    """  # cf. feature-request #81

    compare = difflib.Differ().compare
    """Comparison method shared by all subclasses."""

    maxDiff = None

    def __init__(self, method_name, input, expected, id, suite_settings=None):
        """
        Initialise the CustomTestCase.

        Arguments:

        method_name -- name of test method to run.
        input -- input to the parser.
        expected -- expected output from the parser.
        id -- unique test identifier, used by the test framework.
        suite_settings -- settings overrides for this test suite.
        """
        self.id = id
        self.input = input
        self.expected = expected
        if suite_settings is not None:
            self.suite_settings = suite_settings.copy()
        else:
            self.suite_settings = {}

        super().__init__(method_name)

    def __str__(self):
        """
        Return string conversion. Overridden to give test id, in addition to
        method name.
        """
        return f'{self.id}; {unittest.TestCase.__str__(self)}'

    def __repr__(self):
        return f'<{self.id} {unittest.TestCase.__repr__(self)}>'

    def setUp(self):
        super().setUp()
        # Language-specific roles and roles added by the
        # "default-role" and "role" directives are currently stored
        # globally in the roles._roles dictionary.  This workaround
        # empties that dictionary.
        roles._roles = {}


class CustomTestSuite(unittest.TestSuite):

    """
    A collection of CustomTestCases.

    Provides test suite ID generation and a method for adding test cases.
    """

    id = ''
    """Identifier for the TestSuite. Prepended to the
    TestCase identifiers to make identification easier."""

    next_test_case_id = 0
    """The next identifier to use for non-identified test cases."""

    def __init__(self, tests=(), id=None, suite_settings=None):
        """
        Initialize the CustomTestSuite.

        Arguments:

        id -- identifier for the suite, prepended to test cases.
        suite_settings -- settings overrides for this test suite.
        """
        super().__init__(tests)
        self.suite_settings = suite_settings or {}
        if id is None:
            mypath = os.path.abspath(
                sys.modules[CustomTestSuite.__module__].__file__)
            outerframes = inspect.getouterframes(inspect.currentframe())
            for outerframe in outerframes[1:]:
                if outerframe[3] != '__init__':
                    callerpath = outerframe[1]
                    if callerpath is None:
                        # It happens sometimes.  Why is a mystery.
                        callerpath = os.getcwd()
                    callerpath = os.path.abspath(callerpath)
                    break
            mydir, myname = os.path.split(mypath)
            if not mydir:
                mydir = os.curdir
            if callerpath.startswith(mydir):
                self.id = callerpath[len(mydir) + 1:]  # caller's module
            else:
                self.id = callerpath
        else:
            self.id = id

    def addTestCase(self, test_case_class, method_name, input, expected,
                    id=None, **kwargs):
        """
        Create a CustomTestCase in the CustomTestSuite.
        Also return it, just in case.

        Arguments:

        test_case_class -- the CustomTestCase to add
        method_name -- a string; CustomTestCase.method_name is the test
        input -- input to the parser.
        expected -- expected output from the parser.
        id -- unique test identifier, used by the test framework.
        """
        if id is None:                  # generate id if required
            id = self.next_test_case_id
            self.next_test_case_id += 1
        # test identifier will become suiteid.testid
        tcid = '%s: %s' % (self.id, id)
        # generate and add test case
        tc = test_case_class(method_name, input, expected, tcid,
                             suite_settings=self.suite_settings.copy(),
                             **kwargs)
        self.addTest(tc)
        return tc


class ParserTestCase(CustomTestCase):

    """
    Output checker for the parser.

    Should probably be called ParserOutputChecker, but I can deal with
    that later when/if someone comes up with a category of parser test
    cases that have nothing to do with the input and output of the parser.
    """

    parser = rst.Parser()
    """Parser shared by all ParserTestCases."""

    settings = frontend.get_default_settings(rst.Parser)
    settings.report_level = 5
    settings.halt_level = 5
    settings.debug = False

    def test_parser(self):
        settings = self.settings.copy()
        settings.__dict__.update(self.suite_settings)
        document = utils.new_document('test data', settings)
        self.parser.parse(self.input, document)
        output = document.pformat()
        self.assertEqual(output, self.expected)


class ParserTestSuite(CustomTestSuite):

    """
    A collection of ParserTestCases.

    A ParserTestSuite instance manufactures ParserTestCases,
    keeps track of them, and provides a shared test fixture (a-la
    setUp and tearDown).
    """

    test_case_class = ParserTestCase

    def generateTests(self, dict):
        """
        Stock the suite with test cases generated from a test data dictionary.

        Each dictionary key (test type name) maps to a list of tests. Each
        test is a list: input, expected output.
        Tests should be self-documenting and not require external comments.
        """
        for name, cases in dict.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                self.addTestCase(
                      self.test_case_class, 'test_parser',
                      input=case_input, expected=case_expected,
                      id=f'totest[{name!r}][{casenum}]')


def exception_data(func, *args, **kwds):
    """
    Execute `func(*args, **kwds)` and return the resulting exception, the
    exception arguments, and the formatted exception string.
    """
    try:
        func(*args, **kwds)
    except Exception as detail:
        return (detail, detail.args,
                '%s: %s' % (detail.__class__.__name__, detail))
    return None, [], "No exception"
