#! /usr/bin/env python

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Test module for the --record-dependencies option.
"""

import os.path
import unittest
import sys
import DocutilsTestSupport              # must be imported before docutils
import docutils.core
import docutils.utils


class RecordDependenciesTests(unittest.TestCase):

    # docutils.utils.DependencyList records relative URLs, not platform paths,
    # so use "/" as a path separator even on Windows (not os.path.join).

    def get_record(self, **settings):
        recordfile = 'record.txt'
        settings.setdefault('source_path',
                            os.path.join('data', 'dependencies.txt'))
        settings.setdefault('settings_overrides', {})
        settings['settings_overrides'] = settings['settings_overrides'].copy()
        settings['settings_overrides']['_disable_config'] = 1
        if not settings['settings_overrides'].has_key('record_dependencies'):
            settings['settings_overrides']['record_dependencies'] = \
                docutils.utils.DependencyList(recordfile)
        docutils.core.publish_file(destination=DocutilsTestSupport.DevNull(),
                                   **settings)
        settings['settings_overrides']['record_dependencies'].close()
        return open(recordfile).read().splitlines()

    def test_dependencies(self):
        self.assertEqual(self.get_record(),
                         ['data/include.txt', 'data/raw.txt'])
        self.assertEqual(self.get_record(writer_name='latex'),
                         ['data/include.txt',
                          'data/raw.txt',
                          # this is a URL, not a path:
                          'some_image.png'])

    def test_csv_dependencies(self):
        try:
            import csv
            self.assertEqual(
                self.get_record(source_path=os.path.join('data',
                                                         'csv_dep.txt')),
                ['data/csv_data.txt'])
        except ImportError:
            pass

    def test_stylesheet_dependencies(self):
        # Parameters to publish_file.
        s = {'settings_overrides': {}}
        so = s['settings_overrides']
        so['embed_stylesheet'] = 0
        so['stylesheet_path'] = os.path.join('data', 'stylesheet.txt')
        so['stylesheet'] = None
        s['writer_name'] = 'html'
        self.assert_(os.path.join('data', 'stylesheet.txt') not in
                     self.get_record(**s))
        so['embed_stylesheet'] = 1
        self.assert_('data/stylesheet.txt' in self.get_record(**s))
        del so['embed_stylesheet']
        s['writer_name'] = 'latex'
        self.assert_('data/stylesheet.txt' in self.get_record(**s))


if __name__ == '__main__':
    unittest.main()
