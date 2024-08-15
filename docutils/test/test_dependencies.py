#! /usr/bin/env python3

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for the --record-dependencies option.
"""

from io import StringIO
from pathlib import Path
import os.path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import docutils.core
import docutils.utils
import docutils.io
from docutils.parsers.rst.directives.images import PIL

TEST_ROOT = Path(__file__).parent  # ./test/ from the docutils root
DATA_ROOT = TEST_ROOT / 'data'
CWD = Path(os.getcwd())


def relpath(path):
    # docutils.utils.DependencyList records POSIX paths,
    # i.e. "/" as a path separator even on Windows.
    return os.path.relpath(path, CWD).replace('\\', '/')


paths = {
    'include': relpath(DATA_ROOT / 'include.rst'),  # included rst file
    'raw': relpath(DATA_ROOT / 'raw.rst'),      # included raw "HTML file"
    'stylesheet': relpath(DATA_ROOT / 'stylesheet.rst'),
    # the "image" and "figure" directives expect a URI and use it literally
    'scaled-image': '../docs/user/rst/images/biohazard.png',
    'figure-image': '../docs/user/rst/images/title.png',
}

# avoid latex writer future warnings:
latex_settings_overwrites = {'legacy_column_widths': False,
                             'use_latex_citations': True}


class RecordDependenciesTests(unittest.TestCase):

    maxDiff = None

    def get_record(self, **kwargs):
        recordfile = 'record.rst'
        recorder = docutils.utils.DependencyList(recordfile)
        # (Re) create the record file by running a conversion:
        kwargs.setdefault('source_path', str(DATA_ROOT/'dependencies.rst'))
        kwargs.setdefault('settings_overrides', {})
        kwargs['settings_overrides'].update(_disable_config=True,
                                            record_dependencies=recorder)
        output = docutils.core.publish_file(destination=StringIO(),  # ignored
                                            **kwargs)
        recorder.close()
        # Read the record file:
        records = Path(recordfile).read_text(encoding='utf-8').splitlines()
        return records, output

    def test_dependencies_xml(self):
        # Note: currently, raw input files are read (and hence recorded) while
        # parsing even if not used in the chosen output format.
        # This should change (see parsers/rst/directives/misc.py).
        keys = ['include', 'raw']
        if PIL and os.path.exists('../docs/user/rst/images/'):
            keys += ['figure-image']
        expected = [paths[key] for key in keys]
        record, _output = self.get_record(writer='xml')
        # the order of the files is arbitrary
        self.assertEqual(sorted(expected), sorted(record))

    def test_dependencies_html(self):
        keys = ['include', 'raw']
        if PIL and os.path.exists('../docs/user/rst/images/'):
            keys += ['figure-image', 'scaled-image']
        expected = [paths[key] for key in keys]
        # stylesheets are tested separately in test_stylesheet_dependencies():
        settings = {'stylesheet_path': None,
                    'stylesheet': None,
                    'report_level': 4}  # drop warning if PIL is missing
        record, output = self.get_record(writer='html5',
                                         settings_overrides=settings)
        # the order of the files is arbitrary
        self.assertEqual(sorted(expected), sorted(record),
                         msg='output is:\n'+output)

    def test_dependencies_latex(self):
        # since 0.9, the latex writer records only really accessed files, too.
        # Note: currently, raw input files are read (and hence recorded) while
        # parsing even if not used in the chosen output format.
        # This should change (see parsers/rst/directives/misc.py).
        keys = ['include', 'raw']
        if PIL and os.path.exists('../docs/user/rst/images/'):
            keys += ['figure-image']
        expected = [paths[key] for key in keys]
        record, output = self.get_record(
                            writer='latex',
                            settings_overrides=latex_settings_overwrites)
        # the order of the files is arbitrary
        self.assertEqual(sorted(expected), sorted(record),
                         msg='output is:\n'+output)

    def test_csv_dependencies(self):
        csvsource = str(DATA_ROOT / 'csv_dep.rst')
        record, output = self.get_record(source_path=csvsource)
        self.assertEqual([relpath(DATA_ROOT / 'csv_data.txt')], record,
                         msg='output is:\n'+output)

    def test_stylesheet_dependencies(self):
        stylesheet = paths['stylesheet']
        settings = {'stylesheet_path': paths['stylesheet'],
                    'stylesheet': None}
        settings.update(latex_settings_overwrites)
        settings['embed_stylesheet'] = False
        record, _output = self.get_record(writer='html',
                                          settings_overrides=settings)
        self.assertTrue(stylesheet not in record,
                        f'{stylesheet!r} should not be in {record!r}')
        record, _output = self.get_record(writer='latex',
                                          settings_overrides=settings)
        self.assertTrue(stylesheet not in record,
                        f'{stylesheet!r} should not be in {record!r}')

        settings['embed_stylesheet'] = True
        record, _output = self.get_record(writer='html',
                                          settings_overrides=settings)
        self.assertTrue(stylesheet in record,
                        f'{stylesheet!r} should be in {record!r}')
        settings['embed_stylesheet'] = True
        record, _output = self.get_record(writer='latex',
                                          settings_overrides=settings)
        self.assertTrue(stylesheet in record,
                        f'{stylesheet!r} should be in {record!r}')

    def tearDown(self) -> None:
        os.unlink("record.rst")


if __name__ == '__main__':
    unittest.main()
