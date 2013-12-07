# Copyright (C) 2010-2013 Stefan Merten

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
Glue code for XSLT and Python based conversion.
"""

###############################################################################
###############################################################################
# Import

import os.path

from xml2rstlib import markup

from docutils_xml.parsers.xslt import XsltParser, XPathExtension
from docutils_xml.writers.xslt import XsltWriter

import docutils.frontend, docutils.core

__docformat__ = 'reStructuredText' # Formatted to be rendered by epydoc

###############################################################################
###############################################################################
# Constants

XsltNm = 'xml2rst.xsl'
"""
:type: str

Name of the main XSLT source file.
"""

DocutilsSectionChars = '!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~'

###############################################################################
###############################################################################
# Functions

def validate_titleMarkup(setting, value, option_parser,
                         config_parser=None, config_section=None):
    """
    Validate option value for title markup.

    The parameters are reverse engineered.

    :Parameters:

      setting : str
        The name of the option for this setting.

      value : str
        The value given for this option.

      option_parser : ?
        ?

      config_parser : ?
        ?

      config_section : ?
        ?

    :return: The canonical value for `setting` from `value`.
    """
    tail = value
    while tail:
        head = tail[0:2]
        if len(head) < 2:
            raise ValueError("Odd number of characters (%r)" % ( value, ))
        tail = tail[2:]
        ( ou, ch ) = head
        if ou not in "ou":
            raise ValueError("Invalid overline / underline specification '%c'"
                             % ( ou, ))
        if ch not in DocutilsSectionChars:
            raise ValueError("Invalid character '%c' for title markup"
                             % ( ch, ))
    return value

###############################################################################
###############################################################################
# Classes

class RstText(XPathExtension):
    """
    XPath extension functions for computing valid reStructuredText markup for
    plain text.
    """

    namespace = "http://www.merten-home.de/xml2rst"

    def plain(self, context, string, indent, literal):
        """
        Output a plain text preventing further interpretation by
        reStructuredText. Text may contain linefeeds.

        :Parameters:

          context : `lxml.etree._XSLTContext`
            The evaluation context.

            context.context_node : `Element`
              The context node.

            contect.eval_context : dict
              A dictionary to store state.

          string
            The (smart) string to turn into output text.

          indent
            The (smart) string to use for indent in case of internal
            linefeeds.

          literal
            The (smart) boolean determining whether this should be output
            literally.
        """
        return markup.Text.plain(self._stringParameter(string),
                                 self._stringParameter(indent),
                                 self._boolParameter(literal))

    # indent
    # directive
    # field_names
    # substitution
    # inline markup
    # token
    # label
    # start_delimiter
    # end_delimiter
    # target_definition

###############################################################################

class Parser(XsltParser):

    settings_spec = (
        "xml2rst options", None,
        (( """\
Configures title markup style.

The value of the parameter must be a string made up of a sequence of character
pairs. The first character of a pair is ``o`` (overline) or ``u`` (underline)
and the second character is the character to use for the markup.

The first and the second character pair is used for document title and
subtitle, the following pairs are used for section titles where the third pair
is used for the top level section title.

Defaults to ``o=o-u=u-u~u`u,u.``.
""",
           ( '-a', '--adornment' ),
           { 'validator': validate_titleMarkup,
             'default': 'o=o-u=u-u~u`u,u.', }, ),
( """\
Configures whether long text lines in paragraphs should be folded and to which
length.

This option is for input with no internal line feeds in plain text strings. If
there are internal line feeds in plain text strings these should be preferred.

If folding is enabled text strings not in a line feed preserving
context are first white-space normalized and then broken according to
the folding rules. Folding rules put out the first word and continue
to do so with the following words unless the next word would cross
the folding boundary. Words are delimited by white-space.

Defaults to 0, i.e. no folding.
""",
           ( '-f', '--fold' ),
           { 'validator': docutils.frontend.validate_nonnegative_int,
             'default': 0, }, ), ),
        )

###############################################################################

class Writer(XsltWriter):
    """
    Writer for this parser.
    """

    supported = ( 'restructuredtext', )

###############################################################################
###############################################################################
# Specialized functions

def main():
    # Find XSLT in library
    xsltP = os.path.join(os.path.dirname(__file__), XsltNm)
    try:
        xsltF = open(xsltP)
    except IOError, e:
        raise Exception("Can't open XSLT file %r: %s" % ( xsltP, e, ))

    docutils.core.publish_cmdline(parser=Parser(xsltF, RstText()),
                                  writer=Writer(),
                                  description="Reads Docutils XML from <source> and writes reStructuredText to <destination>")
