# $Id$
# Authors: David Goodger <goodger@python.org>;
#          Garth Kidd <garth@deadlybloodyserious.com>
# Copyright: This module has been placed in the public domain.

"""
Exports the following:

:Modules:
    - `statemachine` is 'docutils.statemachine'
    - `nodes` is 'docutils.nodes'
    - `urischemes` is 'docutils.utils.urischemes'
    - `utils` is 'docutils.utils'
    - `transforms` is 'docutils.transforms'
    - `states` is 'docutils.parsers.rst.states'
    - `tableparser` is 'docutils.parsers.rst.tableparser'

:Classes:
    - `StandardTestCase`
    - `CustomTestCase`
    - `CustomTestSuite`
    - `TransformTestCase`
    - `TransformTestSuite`
    - `ParserTestCase`
    - `ParserTestSuite`
    - `ParserTransformTestCase`
    - `PEPParserTestCase`
    - `PEPParserTestSuite`
    - `GridTableParserTestCase`
    - `GridTableParserTestSuite`
    - `SimpleTableParserTestCase`
    - `SimpleTableParserTestSuite`
    - `WriterPublishTestCase`
    - `LatexWriterPublishTestCase`
    - `PseudoXMLWriterPublishTestCase`
    - `HtmlWriterPublishTestCase`
    - `PublishTestSuite`
    - `HtmlFragmentTestSuite`
    - `DevNull` (output sink)
"""
__docformat__ = 'reStructuredText'

import difflib
import inspect
import os
import pdb
import sys
import traceback
import unittest
from pprint import pformat

testroot = os.path.abspath(os.path.dirname(__file__) or os.curdir)
os.chdir(testroot)
sys.path.insert(0, os.path.normpath(os.path.join(testroot, '..')))
sys.path.append(os.path.normpath(os.path.join(testroot, '..', 'extras')))
sys.path.insert(0, testroot)

try:
    import docutils
    import docutils.core
    from docutils import frontend, nodes, statemachine, utils   # noqa: F401
    from docutils.utils import urischemes                       # noqa: F401
    from docutils.transforms import universal
    from docutils.parsers import rst
    from docutils.parsers.rst import states, tableparser, roles, languages  # noqa: F401, E501
    from docutils.readers import standalone, pep                # noqa: F401
    from docutils.statemachine import StringList, string2lines
except ImportError:
    # The importing module (usually __init__.py in one of the
    # subdirectories) may catch ImportErrors in order to detect the
    # absence of DocutilsTestSupport in sys.path.  Thus, ImportErrors
    # resulting from problems with importing Docutils modules must be
    # caught here.
    traceback.print_exc()
    raise SystemExit(1)


# Hack to make repr(StringList) look like repr(list):
StringList.__repr__ = StringList.__str__


class DevNull:

    """Output sink."""

    def write(self, string):
        pass

    def close(self):
        pass


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

    def __init__(self, method_name, input, expected, id,
                 run_in_debugger=True, suite_settings=None):
        """
        Initialise the CustomTestCase.

        Arguments:

        method_name -- name of test method to run.
        input -- input to the parser.
        expected -- expected output from the parser.
        id -- unique test identifier, used by the test framework.
        run_in_debugger -- if true, run this test under the pdb debugger.
        suite_settings -- settings overrides for this test suite.
        """
        self.id = id
        self.input = input
        self.expected = expected
        self.run_in_debugger = run_in_debugger
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

    def compare_output(self, _input, output, expected):
        """`output` and `expected` should be strings."""
        if isinstance(expected, bytes):
            expected = expected.decode('utf-8')
        if isinstance(output, bytes):
            output = output.decode('utf-8')
        # Normalise line endings:
        expected = expected and '\n'.join(expected.splitlines())
        output = output and '\n'.join(output.splitlines())
        self.assertEqual(output, expected)


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
                    id=None, run_in_debugger=False, **kwargs):
        """
        Create a CustomTestCase in the CustomTestSuite.
        Also return it, just in case.

        Arguments:

        test_case_class -- the CustomTestCase to add
        method_name -- a string; CustomTestCase.method_name is the test
        input -- input to the parser.
        expected -- expected output from the parser.
        id -- unique test identifier, used by the test framework.
        run_in_debugger -- if true, run this test under the pdb debugger.
        """
        if id is None:                  # generate id if required
            id = self.next_test_case_id
            self.next_test_case_id += 1
        # test identifier will become suiteid.testid
        tcid = '%s: %s' % (self.id, id)
        # generate and add test case
        tc = test_case_class(method_name, input, expected, tcid,
                             run_in_debugger=run_in_debugger,
                             suite_settings=self.suite_settings.copy(),
                             **kwargs)
        self.addTest(tc)
        return tc


class TransformTestCase(CustomTestCase):

    """
    Output checker for the transform.

    Should probably be called TransformOutputChecker, but I can deal with
    that later when/if someone comes up with a category of transform test
    cases that have nothing to do with the input and output of the transform.
    """

    settings = frontend.get_default_settings(rst.Parser)
    settings.report_level = 1
    settings.halt_level = 5
    settings.debug = False
    settings.warning_stream = DevNull()
    unknown_reference_resolvers = ()

    def __init__(self, *args, parser=None, transforms=None, **kwargs):
        assert transforms is not None, 'required argument'
        self.transforms = transforms
        """List of transforms to perform for this test case."""

        assert parser is not None, 'required argument'
        self.parser = parser
        """Input parser for this test case."""

        super().__init__(*args, **kwargs)

    def supports(self, format):
        return True

    def test_transforms(self):
        if self.run_in_debugger:
            pdb.set_trace()
        settings = self.settings.copy()
        settings.__dict__.update(self.suite_settings)
        document = utils.new_document('test data', settings)
        self.parser.parse(self.input, document)
        # Don't do a ``populate_from_components()`` because that would
        # enable the Transformer's default transforms.
        document.transformer.add_transforms(self.transforms)
        document.transformer.add_transform(universal.TestMessages)
        document.transformer.components['writer'] = self
        document.transformer.apply_transforms()
        output = document.pformat()
        self.compare_output(self.input, output, self.expected)

    def test_transforms_verbosely(self):
        if self.run_in_debugger:
            pdb.set_trace()
        print('\n', self.id)
        print('-' * 70)
        print(self.input)
        settings = self.settings.copy()
        settings.__dict__.update(self.suite_settings)
        document = utils.new_document('test data', settings)
        self.parser.parse(self.input, document)
        print('-' * 70)
        print(document.pformat())
        for transformClass in self.transforms:
            transformClass(document).apply()
        output = document.pformat()
        print('-' * 70)
        print(output)
        self.compare_output(self.input, output, self.expected)


class TransformTestSuite(CustomTestSuite):

    """
    A collection of TransformTestCases.

    A TransformTestSuite instance manufactures TransformTestCases,
    keeps track of them, and provides a shared test fixture (a-la
    setUp and tearDown).
    """

    def __init__(self, parser, suite_settings=None):
        self.parser = parser
        """Parser shared by all test cases."""

        super().__init__(suite_settings=suite_settings)

    def generateTests(self, dict, dictname='totest',
                      testmethod='test_transforms'):
        """
        Stock the suite with test cases generated from a test data dictionary.

        Each dictionary key (test type's name) maps to a tuple, whose
        first item is a list of transform classes and whose second
        item is a list of tests. Each test is a list: input, expected
        output, optional modifier. The optional third entry, a
        behavior modifier, can be 0 (temporarily disable this test) or
        1 (run this test under the pdb debugger). Tests should be
        self-documenting and not require external comments.
        """
        for name, (transforms, cases) in dict.items():
            for casenum in range(len(cases)):
                case = cases[casenum]
                run_in_debugger = False
                if len(case) == 3:
                    # TODO: (maybe) change the 3rd argument to a dict, so it
                    # can handle more cases by keyword ('disable', 'debug',
                    # 'settings'), here and in other generateTests methods.
                    # But there's also the method that
                    # HtmlPublishPartsTestSuite uses <DJG>
                    if case[2]:
                        run_in_debugger = True
                    else:
                        continue
                self.addTestCase(
                      TransformTestCase, testmethod,
                      transforms=transforms, parser=self.parser,
                      input=case[0], expected=case[1],
                      id='%s[%r][%s]' % (dictname, name, casenum),
                      run_in_debugger=run_in_debugger)


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
        if self.run_in_debugger:
            pdb.set_trace()
        settings = self.settings.copy()
        settings.__dict__.update(self.suite_settings)
        document = utils.new_document('test data', settings)
        self.parser.parse(self.input, document)
        output = document.pformat()
        self.compare_output(self.input, output, self.expected)


class ParserTestSuite(CustomTestSuite):

    """
    A collection of ParserTestCases.

    A ParserTestSuite instance manufactures ParserTestCases,
    keeps track of them, and provides a shared test fixture (a-la
    setUp and tearDown).
    """

    test_case_class = ParserTestCase

    def generateTests(self, dict, dictname='totest'):
        """
        Stock the suite with test cases generated from a test data dictionary.

        Each dictionary key (test type name) maps to a list of tests. Each
        test is a list: input, expected output, optional modifier. The
        optional third entry, a behavior modifier, can be 0 (temporarily
        disable this test) or 1 (run this test under the pdb debugger). Tests
        should be self-documenting and not require external comments.
        """
        for name, cases in dict.items():
            for casenum in range(len(cases)):
                case = cases[casenum]
                run_in_debugger = False
                if len(case) == 3:
                    if case[2]:
                        run_in_debugger = True
                    else:
                        continue
                self.addTestCase(
                      self.test_case_class, 'test_parser',
                      input=case[0], expected=case[1],
                      id='%s[%r][%s]' % (dictname, name, casenum),
                      run_in_debugger=run_in_debugger)


class PEPParserTestCase(ParserTestCase):

    """PEP-specific parser test case."""

    parser = rst.Parser(rfc2822=True, inliner=rst.states.Inliner())
    """Parser shared by all PEPParserTestCases."""

    settings = frontend.get_default_settings(rst.Parser, pep.Reader)
    settings.report_level = 5
    settings.halt_level = 5
    settings.debug = False


class PEPParserTestSuite(ParserTestSuite):

    """A collection of PEPParserTestCases."""

    test_case_class = PEPParserTestCase


# Optional tests with 3rd party CommonMark parser
# ===============================================

# TODO: test with alternative CommonMark parsers?
md_parser_name = 'recommonmark'
# md_parser_name = 'pycmark'
# md_parser_name = 'myst'
md_skip_msg = f'Cannot test "{md_parser_name}". Parser not found.'
try:
    md_parser_class = docutils.parsers.get_parser_class(
                                                md_parser_name)
except ImportError:
    md_parser_class = None
if md_parser_class and md_parser_name == 'recommonmark':
    import recommonmark
    if recommonmark.__version__ < '0.6.0':
        md_parser_class = None
        md_skip_msg = f'"{md_parser_name}" parser too old, skip tests'


@unittest.skipUnless(md_parser_class, md_skip_msg)
class RecommonmarkParserTestCase(ParserTestCase):

    """Test case for 3rd-party CommonMark parsers."""

    if md_parser_class:
        parser = md_parser_class()
        settings = frontend.get_default_settings(md_parser_class)
        settings.report_level = 5
        settings.halt_level = 5
        settings.debug = False


class RecommonmarkParserTestSuite(ParserTestSuite):

    """A collection of RecommonmarkParserTestCases."""

    test_case_class = RecommonmarkParserTestCase


class GridTableParserTestCase(CustomTestCase):

    parser = tableparser.GridTableParser()

    def test_parse_table(self):
        self.parser.setup(StringList(string2lines(self.input), 'test data'))
        try:
            self.parser.find_head_body_sep()
            self.parser.parse_table()
            output = self.parser.cells
        except Exception as details:
            output = '%s: %s' % (details.__class__.__name__, details)
        self.compare_output(self.input, pformat(output) + '\n',
                            pformat(self.expected) + '\n')

    def test_parse(self):
        try:
            output = self.parser.parse(StringList(string2lines(self.input),
                                                  'test data'))
        except Exception as details:
            output = '%s: %s' % (details.__class__.__name__, details)
        self.compare_output(self.input, pformat(output) + '\n',
                            pformat(self.expected) + '\n')


class GridTableParserTestSuite(CustomTestSuite):

    """
    A collection of GridTableParserTestCases.

    A GridTableParserTestSuite instance manufactures GridTableParserTestCases,
    keeps track of them, and provides a shared test fixture (a-la setUp and
    tearDown).
    """

    test_case_class = GridTableParserTestCase

    def generateTests(self, dict, dictname='totest'):
        """
        Stock the suite with test cases generated from a test data dictionary.

        Each dictionary key (test type name) maps to a list of tests. Each
        test is a list: an input table, expected output from parse_table(),
        expected output from parse(), optional modifier. The optional fourth
        entry, a behavior modifier, can be 0 (temporarily disable this test)
        or 1 (run this test under the pdb debugger). Tests should be
        self-documenting and not require external comments.
        """
        for name, cases in dict.items():
            for casenum in range(len(cases)):
                case = cases[casenum]
                run_in_debugger = False
                if len(case) == 4:
                    if case[-1]:
                        run_in_debugger = True
                    else:
                        continue
                self.addTestCase(self.test_case_class, 'test_parse_table',
                                 input=case[0], expected=case[1],
                                 id='%s[%r][%s]' % (dictname, name, casenum),
                                 run_in_debugger=run_in_debugger)
                self.addTestCase(self.test_case_class, 'test_parse',
                                 input=case[0], expected=case[2],
                                 id='%s[%r][%s]' % (dictname, name, casenum),
                                 run_in_debugger=run_in_debugger)


class SimpleTableParserTestCase(GridTableParserTestCase):

    parser = tableparser.SimpleTableParser()


class SimpleTableParserTestSuite(CustomTestSuite):

    """
    A collection of SimpleTableParserTestCases.
    """

    test_case_class = SimpleTableParserTestCase

    def generateTests(self, dict, dictname='totest'):
        """
        Stock the suite with test cases generated from a test data dictionary.

        Each dictionary key (test type name) maps to a list of tests. Each
        test is a list: an input table, expected output from parse(), optional
        modifier. The optional third entry, a behavior modifier, can be 0
        (temporarily disable this test) or 1 (run this test under the pdb
        debugger). Tests should be self-documenting and not require external
        comments.
        """
        for name, cases in dict.items():
            for casenum in range(len(cases)):
                case = cases[casenum]
                run_in_debugger = False
                if len(case) == 3:
                    if case[-1]:
                        run_in_debugger = True
                    else:
                        continue
                self.addTestCase(self.test_case_class, 'test_parse',
                                 input=case[0], expected=case[1],
                                 id='%s[%r][%s]' % (dictname, name, casenum),
                                 run_in_debugger=run_in_debugger)


class WriterPublishTestCase(CustomTestCase, docutils.SettingsSpec):

    """
    Test case for publish.
    """

    settings_default_overrides = {'_disable_config': True,
                                  'strict_visitor': True}
    writer_name = ''  # set in subclasses or constructor

    def __init__(self, *args, writer_name='', **kwargs):
        if writer_name:
            self.writer_name = writer_name
        super().__init__(*args, **kwargs)

    def test_publish(self):
        if self.run_in_debugger:
            pdb.set_trace()
        output = docutils.core.publish_string(
              source=self.input,
              reader_name='standalone',
              parser_name='restructuredtext',
              writer_name=self.writer_name,
              settings_spec=self,
              settings_overrides=self.suite_settings)
        self.compare_output(self.input, output, self.expected)


class PublishTestSuite(CustomTestSuite):

    def __init__(self, writer_name, suite_settings=None):
        """
        `writer_name` is the name of the writer to use.
        """
        super().__init__(suite_settings=suite_settings)
        self.test_class = WriterPublishTestCase
        self.writer_name = writer_name

    def generateTests(self, dict, dictname='totest'):
        for name, cases in dict.items():
            for casenum in range(len(cases)):
                case = cases[casenum]
                run_in_debugger = False
                if len(case) == 3:
                    if case[2]:
                        run_in_debugger = True
                    else:
                        continue
                self.addTestCase(
                      self.test_class, 'test_publish',
                      input=case[0], expected=case[1],
                      id='%s[%r][%s]' % (dictname, name, casenum),
                      run_in_debugger=run_in_debugger,
                      # Passed to constructor of self.test_class:
                      writer_name=self.writer_name)


class HtmlWriterPublishPartsTestCase(WriterPublishTestCase):

    """
    Test case for HTML writer via the publish_parts interface.
    """

    writer_name = 'html'

    settings_default_overrides = \
        WriterPublishTestCase.settings_default_overrides.copy()
    settings_default_overrides['stylesheet'] = ''

    def test_publish(self):
        if self.run_in_debugger:
            pdb.set_trace()
        parts = docutils.core.publish_parts(
            source=self.input,
            reader_name='standalone',
            parser_name='restructuredtext',
            writer_name=self.writer_name,
            settings_spec=self,
            settings_overrides=self.suite_settings)
        output = self.format_output(parts)
        # interpolate standard variables:
        expected = self.expected % {'version': docutils.__version__}
        self.compare_output(self.input, output, expected)

    standard_content_type_template = ('<meta http-equiv="Content-Type"'
                                      ' content="text/html; charset=%s" />\n')
    standard_generator_template = (
        '<meta name="generator"'
        ' content="Docutils %s: https://docutils.sourceforge.io/" />\n')
    standard_html_meta_value = (
        standard_content_type_template
        + standard_generator_template % docutils.__version__)
    standard_meta_value = standard_html_meta_value % 'utf-8'
    standard_html_prolog = (
        '<?xml version="1.0" encoding="%s" ?>\n'
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" '
        '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n')

    def format_output(self, parts):
        """Minimize & standardize the output."""
        # remove redundant parts & uninteresting parts:
        del parts['whole']
        assert parts['body'] == parts['fragment']
        del parts['body']
        del parts['body_pre_docinfo']
        del parts['body_prefix']
        del parts['body_suffix']
        del parts['head']
        del parts['head_prefix']
        del parts['encoding']
        del parts['version']
        # remove standard portions:
        parts['meta'] = parts['meta'].replace(self.standard_meta_value, '')
        parts['html_head'] = parts['html_head'].replace(
            self.standard_html_meta_value, '...')
        parts['html_prolog'] = parts['html_prolog'].replace(
            self.standard_html_prolog, '')
        output = []
        for key in sorted(parts.keys()):
            if not parts[key]:
                continue
            output.append("%r: '''%s'''"
                          % (key, parts[key]))
            if output[-1].endswith("\n'''"):
                output[-1] = output[-1][:-4] + "\\n'''"
        return '{' + ',\n '.join(output) + '}\n'


class HtmlPublishPartsTestSuite(CustomTestSuite):

    testcase_class = HtmlWriterPublishPartsTestCase

    def generateTests(self, dict, dictname='totest'):
        for name, (settings_overrides, cases) in dict.items():
            original_settings = self.suite_settings.copy()
            self.suite_settings.update(settings_overrides)
            for casenum in range(len(cases)):
                case = cases[casenum]
                run_in_debugger = False
                if len(case) == 3:
                    if case[2]:
                        run_in_debugger = True
                    else:
                        continue
                self.addTestCase(self.testcase_class, 'test_publish',
                                 input=case[0], expected=case[1],
                                 id='%s[%r][%s]' % (dictname, name, casenum),
                                 run_in_debugger=run_in_debugger)
            self.suite_settings = original_settings


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
