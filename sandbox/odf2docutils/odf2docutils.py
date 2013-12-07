#! /usr/bin/env python
# -*- coding: iso-8859-1 -*-

# Copyright (C) 2013 Stefan Merten

# odf2docutils.py is free software; you can redistribute it and/or modify
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
Convert an Open Document Format (ODF) file to docutils XML.
"""

__docformat__ = 'reStructuredText' # Formatted to be rendered by epydoc

###############################################################################
###############################################################################
# Import

import sys

import docutils, docutils.core, docutils.frontend
import docutils.io, docutils.readers, docutils.writers.docutils_xml
import docutils_xml.io

###############################################################################
###############################################################################
# Constants

###############################################################################
###############################################################################
# Variables

###############################################################################
###############################################################################
# Functions

###############################################################################
###############################################################################
# Classes

class SettingsSpec(docutils.SettingsSpec):
    """
    Settings specifications for odf2docutils.
    """

    xsltOptions = ( '-x', '--xslt' )

    settings_spec = (
        "odf2docutils options", None,
        (( "Use Python implementation for conversion instead of the XSLT implementation (default).",
           ( '-p', '--python' ),
           { 'validator': docutils.frontend.validate_boolean,
             'action': 'store_true',
             'default': True, }, ),
         ( "Use XSLT for conversion instead of the Python implementation.",
           xsltOptions,
           { 'validator': docutils.frontend.validate_boolean,
             'action': 'store_false',
             'dest': 'python', },
           ), ),
        )

    config_section = "odf2docutils"

##############################################################################

class OdfFileInput(docutils_xml.io.ZipFileInput):
    """
    Input for ODF files.
    """

    contentPath = 'content.xml'

##############################################################################
##############################################################################
# Now work

if __name__ == '__main__':
    # Check options to set correct parser and writer
    usePython = True

    # The following "General Docutils Options" are effectively ignored:
    #
    #   Used in `transforms.frontmatter.DocTitle` used by
    #   `readers.standalone.Reader`:
    #     --title
    #
    #   Used in `transforms.universal.Decorations` used by `readers.Reader`:
    #     --generator
    #     --no-generator
    #     --date
    #     --time
    #     --no-datestamp
    #     --source-link
    #     --source-url
    #     --no-source-link
    #
    #   Used in `transforms.parts.Contents` used by `parsers.rst.directives...`:
    #     --toc-entry-backlinks
    #     --toc-top-backlinks
    #     --no-toc-backlinks
    #
    #   Used in `transforms.parts.SectNum` used by `parsers.rst.directives...`:
    #     --section-numbering
    #     --no-section-numbering
    #
    #   Used in `transforms.universal.StripComments` used by `readers.Reader`:
    #     --strip-comments
    #     --leave-comments
    #
    #   Used in `transforms.universal.StripClassesAndElements` used by
    #   `writers.Writer`:
    #     --strip-elements-with-class
    #     --strip-class
    #
    #   Used in `writers.html4css1.HTMLTranslator`:
    #     --footnote-backlinks
    #     --no-footnote-backlinks
    #
    #   Used in `core.Publisher`:
    #     --input-encoding
    #
    #   Used in `parsers.rst.directives...`
    #     --input-encoding-error-handler

    settingsSpec = SettingsSpec()
    for arg in sys.argv:
        if arg in settingsSpec.xsltOptions:
            usePython = False
    if usePython:
        from odf2docutilslib.python import Parser
        from docutils.writers.docutils_xml import Writer
    else:
        from odf2docutilslib.xslt import Parser, Writer

    pub = docutils.core.Publisher(docutils.readers.Reader(), Parser(), Writer(),
                                  source_class=OdfFileInput,
                                  destination_class=docutils.io.BinaryFileOutput)
    pub.process_command_line(settings_spec=settingsSpec,
                             description="Reads ODF file <source> (default is stdin) and writes Docutils XML to <destination> (default is stdout).")
    if pub.settings.python != usePython:
        raise AssertionError("Internal error: Assumed setting of --xslt/--python not confirmed by explicit parsing")
    pub.publish()

# TODO Use XSLT variant as export filter for LibreOffice - which works from
#      scratch using https://help.libreoffice.org/Common/Creating_XML_Filters
