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
Support writer for `XsltParser`.
"""

__docformat__ = 'reStructuredText' # Formatted to be rendered by epydoc

###############################################################################
###############################################################################
# Import

import docutils.writers

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

###############################################################################

class XsltWriter(docutils.writers.Writer):
    """
    Writer transparently writing the result from an `XsltParser`. May be used
    only together with `XsltParser`. Use `docutils.io.BinaryFileOutput` as
    output class for files so binary output is passed through unchanged.

    Please note that `supported` must be set in subclass since the output
    format is determined by the XSLT.
    """

    def translate(self):
        self.output = str(self.document.xslt_result)
