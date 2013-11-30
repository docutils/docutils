#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2009-2013 Stefan Merten

# xml2rst.py is free software; you can redistribute it and/or modify
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
Convert a docutils XML file to reStructuredText syntax.
"""

###############################################################################
###############################################################################
# Import

from xml2rstlib import rst_xslt

###############################################################################
###############################################################################
# Variables

###############################################################################
###############################################################################
# General functions

###############################################################################
###############################################################################
# Specialized functions

###############################################################################
###############################################################################
# Classes

########################################################################
##############################################################################
# Now work

if __name__ == '__main__':
    rst_xslt.main()

##############################################################################
##############################################################################

# TODO Move from XSLT to Python implementation step by step by replacing
#      XSLT-code by Python code through extensions and other means
