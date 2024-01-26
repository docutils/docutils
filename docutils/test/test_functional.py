#!/usr/bin/env python3
# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Perform tests with the data in the functional/ directory.

Please see the documentation on `functional testing`__ for details.

__ ../../docs/dev/testing.html#functional
"""

import difflib
from pathlib import Path
import shutil
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from docutils import core, SettingsSpec

FUNCTIONAL = Path('functional')
EXPECTED = FUNCTIONAL / 'expected'
INPUT = FUNCTIONAL / 'input'
OUTPUT = FUNCTIONAL / 'output'
TESTS = FUNCTIONAL / 'tests'

NO_EXPECTED_TEMPLATE = """\
Cannot find expected output at {exp}
If the output in {out}
is correct, move it to the expected/ dir and check it in:

  mv {out} {exp}
  svn add {exp}
  svn commit -m "<comment>" {exp}
"""

EXPECTED_OUTPUT_DIFFERS_TEMPLATE = """\
Expected and actual output differ.

{diff}

If the actual output is correct, please replace the
expected output and check it in:

  mv {out} {exp}
  svn add {exp}
  svn commit -m "<comment>" {exp}
"""

# Default settings for functional tests.
# Override "factory defaults",
# overridden by `settings_overrides` in the individual tests.
# cf. docs/api/runtime-settings.html#settings-priority
functional_tests_settings_spec = SettingsSpec()
functional_tests_settings_spec.settings_default_overrides = {
    '_disable_config': True,
    'halt_level': 5,
    'warning_stream': '',
    'input_encoding': 'utf-8',  # skip auto-detection
    'embed_stylesheet': False,
    'syntax_highlight': 'none'  # avoid "Pygments not found" warning
    }


def compare_output(output, destination_path, expected_path):

    try:
        expected = expected_path.read_text(encoding='utf-8')
    except OSError as err:
        raise OSError(NO_EXPECTED_TEMPLATE.format(exp=expected_path,
                                                  out=destination_path)
                      ) from err
    if output != expected:
        diff = ''.join(difflib.unified_diff(expected.splitlines(True),
                                            output.splitlines(True),
                                            expected_path.as_posix(),
                                            destination_path.as_posix()))
        raise AssertionError(
                  EXPECTED_OUTPUT_DIFFERS_TEMPLATE.format(
                      diff=diff, exp=expected_path, out=destination_path)
                  )


class FunctionalTests(unittest.TestCase):

    """Test case for one config file."""

    def setUp(self):
        """Clear output directory."""
        for entry in OUTPUT.rglob('*'):
            if entry.is_dir():
                shutil.rmtree(entry)
            elif entry.name != 'README.txt':
                entry.unlink()

    def test_functional(self):
        """Process test file."""
        for test_file in TESTS.glob("*.py"):
            with self.subTest(test_file=test_file.as_posix()):
                namespace = {}
                # Load variables from the current test file into the namespace
                exec(test_file.read_text(encoding='utf-8'), namespace)
                del namespace['__builtins__']  # clean-up

                # Full source, generated output, and expected output paths
                source_path = INPUT / namespace.pop('test_source')
                destination_path = OUTPUT / namespace['test_destination']
                expected_path = EXPECTED / namespace.pop('test_destination')

                # Get output (automatically written to the output/ directory
                # by publish_file):
                output = core.publish_file(
                    **namespace,
                    source_path=source_path.as_posix(),
                    destination_path=destination_path.as_posix(),
                    settings_spec=functional_tests_settings_spec,
                )
                # Get the expected output *after* writing the actual output.
                compare_output(output, destination_path, expected_path)


if __name__ == '__main__':
    unittest.main()
