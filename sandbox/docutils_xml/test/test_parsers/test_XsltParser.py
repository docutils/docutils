# Copyright (C) 2013 Stefan Merten

# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License,
# or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

"""
Test XmlParser.
"""

import unittest
import docutils.utils, docutils.frontend
import StringIO

from __init__ import DocutilsTestSupport

from docutils_xml.parsers.xslt import XsltParser
from docutils_xml.writers.xslt import XsltWriter

###############################################################################

class XsltParserTestCase(DocutilsTestSupport.ParserTestCase):
    """
    Output checker for XsltParser.
    """

    identityXslt = u"""\
<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="/">
    <xsl:copy-of select="/"/>
  </xsl:template>
</xsl:stylesheet>
"""

    parser = XsltParser(StringIO.StringIO(identityXslt))
    """Parser shared by all XsltParserTestCases."""

    option_parser = docutils.frontend.OptionParser(components=(
            XsltParser, ))

    def test_parser(self):
        settings = self.settings.copy()
        settings.__dict__.update(self.suite_settings)
        document = docutils.utils.new_document('test data', settings)
        self.parser.parse(self.input, document)
        writer = XsltWriter()
        output = writer.write(document, docutils.io.StringOutput())
        self.compare_output(self.input, output, self.expected)

###############################################################################

class XsltParserTestSuite(DocutilsTestSupport.ParserTestSuite):

    test_case_class = XsltParserTestCase

###############################################################################

totest = {}

totest['simple'] = (
    ( u"""<?xml version="1.0"?>
<rootOnly/>
""",
    u"""<?xml version="1.0"?>
<rootOnly/>
""" ),
    )

###############################################################################

def suite():
    s = XsltParserTestSuite()
    s.generateTests(totest)
    return s

###############################################################################

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
