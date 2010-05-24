#!/usr/bin/env python

# Copyright (C) 2010 Stefan Merten

# rstdiff.py is free software; you can redistribute it and/or modify
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
Generates a structural diff from two reStructuredText input documents
and produces an annotated result.
"""

__docformat__ = 'reStructuredText'

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

import os, re, sys

import docutils
from docutils import frontend, writers, nodes, SettingsSpec
from docutils.core import Publisher
from docutils.utils import SystemMessage, Reporter, new_reporter
from docutils.frontend import OptionParser, make_paths_absolute
from docutils.nodes import Node

from treediff import TreeMatcher, HashableNodeImpl

###############################################################################
# Command line specification

description = ("""Generates a structural diff from two reStructuredText input
documents and produces an annotated result.  """)

writerOption = 'writer'
writerDefault = 'xml'
writerArgRE1 = '^--' + writerOption + '=' + '(.*)$'

settings_spec = (
    'rstdiff options',
    None,
    (('Select writer to write output with (default "xml").',
      ['--' + writerOption],
      {}),
     )
    )

settings_defaults = {'output_encoding_error_handler': 'xmlcharrefreplace',
                     writerOption: writerDefault}

config_section = 'rstdiff'

usage = '%prog [options]... <old> [<new> [<output>]]'

###############################################################################
# Classes for three argument command lines

class Publisher3Args(Publisher):

    def setup_option_parser(self, usage=None, description=None,
                            settings_spec=None, config_section=None,
                            **defaults):
        if config_section:
            if not settings_spec:
                settings_spec = SettingsSpec()
            settings_spec.config_section = config_section
            parts = config_section.split()
            if len(parts) > 1 and parts[-1] == 'application':
                settings_spec.config_section_dependencies = ['applications']
        #@@@ Add self.source & self.destination to components in future?
        option_parser = OptionParser3Args(
            components=(self.parser, self.reader, self.writer, settings_spec),
            defaults=defaults, read_config_files=1,
            usage=usage, description=description)
        return option_parser

class OptionParser3Args(OptionParser):

    def check_values(self, values, args):
        """Store positional arguments as runtime settings."""
        values._old_source, values._new_source, values._destination = self.check_args(args)
        make_paths_absolute(values.__dict__, self.relative_path_settings,
                            os.getcwd())
        values._config_files = self.config_files
        return values

    def check_args(self, args):
        old_source = new_source = destination = None
        if not args:
            self.error('At least 1 argument required.')
        else:
            old_source = args.pop(0)
            if old_source == '-':           # means stdin
                old_source = None
        if args:
            new_source = args.pop(0)
            if new_source == '-':           # means stdin
                new_source = None
        if args:
            destination = args.pop(0)
            if destination == '-':      # means stdout
                destination = None
        if args:
            self.error('Maximum 3 arguments allowed.')
        if old_source is None and new_source is None:
            self.error('Old and new source may not both use stdin.')
        if (old_source and old_source == destination
            or new_source and new_source == destination):
            self.error('Do not specify the same file for both source and '
                       'destination.  It will clobber the source file.')
        return old_source, new_source, destination

###############################################################################
# Hashable

class HashableDocutilsNodeImpl(HashableNodeImpl):
    """Implements equality for a docutils `Node`."""

    def __init__(self):
        super(self.__class__, self).__init__(Node)

    def dispatchClass(self, function, node, *args):
        """Dispatch a call of type `function` for the class of `node` using
        arguments `node` and `args`. Default is to dispatch for imaginary class
        "UNKNOWN"."""
        try:
            method = getattr(self,
                             "%s_%s" % ( function, node.__class__.__name__, ))
        except AttributeError:
            method = getattr(self,
                             "%s_%s" % ( function, "UNKNOWN", ))
        return method(node, *args)

    ###########################################################################
    # Implementation of abstract methods

    def rootHash(self, node):
        """Return a hash for the root only. Subclasses must override
        this."""
        return self.dispatchClass('rootHash', node)

    def rootHash_UNKNOWN(self, node):
        return hash(node.__class__)

    def rootEq(self, node, other):
        """Returns root equality of `node` and an `other` node. ``True`` if
        the two nodes as roots are equal without considering their
        children. This should be true if one node can be replaced by
        the other and all changes can be represented without changing
        the node itself. Subclasses must override this."""
        # Only nodes of the same class can be equal
        if node.__class__ != other.__class__:
            return False
        return self.dispatchClass('rootEq', node, other)

    def rootEq_UNKNOWN(self, node, other):
        # Unless we know better two roots of the same type are considered equal
        return True

    def childHash(self, node):
        """Return a hash for the node as a child. Subclasses must override
        this."""
        return self.dispatchClass('childHash', node)

    def childHash_UNKNOWN(self, node):
        # TODO Is this correct *and* good?
        return hash(node.__class__)

    def childEq(self, node, other):
        """Returns equality of `node` and an `other` node as children.
        ``True`` if the child features of the two nodes are equal
        without considering the root. Subclasses must override
        this."""
        # Only nodes of the same class can be equal
        # TODO Is this correct?
        if node.__class__ != other.__class__:
            return False
        return self.dispatchClass('childEq', node, other)

    def childEq_UNKNOWN(self, node, other):
        # We don't know how to compare two nodes of same type as children
        return False

    def getChildren(self, node):
        """Return the children of `node` as a list. Subclasses must override
        this."""
        return self.dispatchClass('getChildren', node)

    def getChildren_UNKNOWN(self, node):
        return node.children

    ###########################################################################
    # Real comparison

###############################################################################
# Main

def processCommandLine():
    """Process command line and return a `Publisher`."""
    # Determine writer here so options can be given normally
    preWriter = writerDefault
    for arg in sys.argv:
        match = re.search(writerArgRE1, arg)
        if match:
            preWriter = match.group(1)

    pub = Publisher3Args()
    pub.set_reader('standalone', None, 'restructuredtext')
    pub.set_writer(preWriter)

    settingsSpec = SettingsSpec()
    settingsSpec.settings_spec = settings_spec
    settingsSpec.settings_defaults = settings_defaults
    pub.process_command_line(usage=usage, description=description,
                             settings_spec=settingsSpec,
                             config_section=config_section)
    if pub.settings.writer != preWriter:
        new_reporter('<cmdline>',
                     pub.settings).severe("Internal error: Mismatch of pre-parsed (%r) and real (%r) writer"
                                          % ( preWriter, pub.settings.writer, ))
    return pub

def readTree(pub, sourceName):
    """Read and return a tree from `sourceName`."""
    # Reset reader - just in case it keeps state from a previous invocation
    pub.set_reader('standalone', None, 'restructuredtext')
    pub.set_source(None, sourceName)
    return pub.reader.read(pub.source, pub.parser, pub.settings)

def doDiff(pub, oldTree, newTree):
    """Create a difference from `oldTree` to `newTree`. Returns the opcodes
    necessary to transform `oldTree` to `newTree`."""
    hashableNodeImpl = HashableDocutilsNodeImpl()
    matcher = TreeMatcher(hashableNodeImpl, oldTree, newTree)
    return matcher.get_opcodes()

if __name__ == '__main__':
    pub = processCommandLine()

    pub.set_destination(None, None)

    oldTree = readTree(pub, pub.settings._old_source)
    newTree = readTree(pub, pub.settings._new_source)

    opcodes = doDiff(pub, oldTree, newTree)

    from pprint import pprint
    print(newTree)
    print(oldTree)
    pprint(opcodes, sys.stdout, 2, 40, None)
