#! /usr/bin/env python
# -*- coding: iso-8859-1 -*-

# Based on sample.py,v 4.1.2.6 2006/04/14 13:59:26 cvs Exp

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

Do

	perldoc odf2docutils.py

for a man page.
"""

"""
=head1 NAME

odf2docutils.py -- convert an Open Document Format (ODF) file to docutils XML

=head1 SYNOPSIS

B<odf2docutils.py> [B<-v>] I<odf> [I<xml>]

B<odf2docutils.py> B<--help>

=head1 DESCRIPTION

Converts an Open Document Format (ODF) file to docutils XML. Docutils XML can
then be converted to reStructuredText by using B<xml2rst>.

=cut
"""

###############################################################################
###############################################################################
# Import

import sys
import os.path
import re

from optparse import OptionParser, OptionGroup, OptionValueError, Option

from odf2docutilslib import convert

###############################################################################
###############################################################################
# Constants

###############################################################################
###############################################################################
# Variables

"""
@var options: Options given on the command line
@type options: optparse.Values
"""
global options

###############################################################################
###############################################################################
# General functions

def pod2Head(pod):
    """
    @param pod: Snippet in POD format to be analyzed.
    @type pod: str

    @return: String of first `=headX' entry in POD snippet or empty string if
             none found.
    @rtype: str
    """
    for line in pod.split("\n"):
        if line.startswith("=head"):
            return line[len("=headX"):].strip()
    return ""

###############################################################################

def pod2Description(pod):
    """
    @param pod: Snippet in POD format to be analyzed.
    @type pod: str

    @return: Stripped text from all lines not being a POD line command.
    @rtype: str
    """
    result = ""
    for line in pod.split("\n"):
        if not line.startswith("="):
            result = result.strip() + " " + line.strip()
    return result.strip()

###############################################################################

def pod2OptionList(pod):
    """
    Return option names found in POD snippet. Option names are recognized in
    `=item B<option>' constructs.

    @param pod: Snippet in POD format to be analyzed.
    @type pod: str

    @return: All option names contained in POD snippet as a list.
    @rtype: [ str, ..., ]
    """
    result = [ ]
    for line in pod.split("\n"):
        found = re.search("^=item\s*B<(-[^>]+)>", line)
        if found:
            result.append(found.group(1))
    return result

###############################################################################

def pod2OptionKeywords(pod):
    """
    Return a dict mapping `OptionParser.add_option' keywords to values found in
    POD snippet.

    @param pod: Snippet in POD format to be analyzed.
    @type pod: str

    @return: Mapping for all values found. Currently `help' and `dest' are
             filled.
    @rtype: { keyword: value, ..., }
    """
    result = { 'help': "", }
    for line in pod.split("\n"):
        if line.startswith("=cut"):
            break
        found = re.search("^=item\s*B<--?([^>]+)>(?:=|\s*)", line)
        if found:
            result['help'] = ""
            optionName = found.group(1)
            found = re.search("I<([^>]+)>", line)
            if found:
                result['dest'] = found.group(1)
            elif len(optionName) > 1:
                result['dest'] = optionName
        else:
            result['help'] += line + "\n"
    result['help'] = result['help'].strip()
    if result.has_key('dest'):
        result['dest'] = result['dest'].replace("-", "_")
    else:
        errorExit(1, ( "Internal error: Missing `dest' in documentation string:",
                       pod, ))
    return result

###############################################################################

def pod2Argument(pod):
    """
    Return a list of two strings for `OptionGroup.__init__' describing the
    argument found in POD snippet.

    @param pod: Snippet in POD format to be analyzed.
    @type pod: str

    @return: Name of the argument and its description.
    @rtype: [ argument, description, ]
    """
    argument = ""
    description = ""
    for line in pod.split("\n"):
        if line.startswith("=cut"):
            break
        found = re.search("^=item\s*I<([^>]+)>", line)
        if found:
            description = ""
            argument = found.group(1)
        else:
            description += line + "\n"
    description = description.strip()
    return [ argument, description, ]

###############################################################################

def parseOptions():
    """
    Sets options and returns arguments.

    @return: Name of input file and optionally of output file.
    @rtype: ( str, [str,] )
    """
    global options
    pod = """

=head1 OPTIONS

=cut
    """
    optionParser = OptionParser("usage: %prog [option]... <odf> [<xml>]")

    pod = """

=head2 General options

=over 4

=cut
    """
    generalGroup = OptionGroup(optionParser, pod2Head(pod),
                               pod2Description(pod))

    pod = """

=item B<-v>

=item B<--verbose>

Operate verbose.

=cut
    """
    generalGroup.add_option(action="store_true",
                            *pod2OptionList(pod), **pod2OptionKeywords(pod))
    optionParser.add_option_group(generalGroup)

    pod = """

=back

=head2 Arguments

=over 4

=cut
    """
    argumentGroup = OptionGroup(optionParser, pod2Head(pod),
                                pod2Description(pod))
    optionParser.add_option_group(argumentGroup)

    pod = """

=item I<odf>

The ODF input file containing Open Document Format.

In principle any type of Open Document Format is accepted. However, only text
(C<*.odt>) and presentation (C<*.odp>) make really sense to convert to docutils
XML.

=cut
    """
    argument1Group = OptionGroup(optionParser, *pod2Argument(pod))
    optionParser.add_option_group(argument1Group)

    pod = """

=item I<xml>

The optional output file containing docutils XML.

If not given output is put to C<STDOUT>.

=cut
    """
    argument2Group = OptionGroup(optionParser, *pod2Argument(pod))
    optionParser.add_option_group(argument2Group)

    pod = """

=back

=cut
    """
    ( options, args, ) = optionParser.parse_args()

    if len(args) < 1:
        optionParser.error("An input file is required")
    if len(args) > 2:
        optionParser.error("At most two arguments are allowed")

    return args

###############################################################################

def errorOut(lines):
    """
    Outputs messages as error.

    @param lines: Messages to be output as single lines.
    @type lines: ( str, ..., )

    @return: 0
    @rtype: int
    """
    scriptName = os.path.basename(sys.argv[0])
    for line in lines:
        print >>sys.stderr, ("%s: %s" % ( scriptName, line, ))
    return 0

###############################################################################

def verboseOut(lines):
    """
    Outputs messages as a verbose message.

    @param lines: Messages to be output as single lines.
    @type lines: ( str, ..., )

    @return: 0
    @rtype: int
    """
    if options.verbose:
        errorOut([ "## " + line
                   for line in lines ])
    return 0

###############################################################################

def errorExit(code, lines):
    """
    Exit program with an error message.

    @param code: Exit Code to use.
    @type code: int

    @param lines: Strings to output as error message.
    @type lines: ( str, ..., )

    @return: Does not return.
    """
    errorOut(lines)
    sys.exit(code)

###############################################################################
###############################################################################
# Specialized functions

###############################################################################
###############################################################################
# Classes

##############################################################################
##############################################################################
# Now work

if __name__ == '__main__':
    arguments = parseOptions()
    inF = arguments[0]
    if len(arguments) > 1:
        outF = arguments[1]
    else:
        outF = None
    try:
        convert(inF, outF, options)
    except Exception, e:
        errorExit(1, e)
