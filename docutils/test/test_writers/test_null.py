#!/usr/bin/env python3

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test for Null writer.
"""

import unittest

from test import DocutilsTestSupport  # NoQA: F401

from docutils.core import publish_string


class WriterPublishTestCase(unittest.TestCase):
    def test_publish(self):
        writer_name = 'null'
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    output = publish_string(
                        source=case_input,
                        writer_name=writer_name,
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                        },
                    )
                    if isinstance(output, bytes):
                        output = output.decode('utf-8')
                    self.assertEqual(output, case_expected)


totest = {}

totest['basic'] = [
["""\
This is a paragraph.
""",
None]
]

if __name__ == '__main__':
    import unittest
    unittest.main()
