#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This is the Docutils (Python Documentation Utilities) package.

Package Structure
=================

Modules:

- __init__.py: Contains the package docstring only (this text).

- core.py: Contains the ``Publisher`` class and ``publish()`` convenience
  function.

- frontend.py: Command-line and common processing for Docutils front-ends.

- io.py: Provides a uniform API for low-level input and output.

- nodes.py: Docutils document tree (doctree) node class library.

- optik.py: Option parsing and command-line help; from Greg Ward's
  http://optik.sf.net/ project, included for convenience.

- roman.py: Conversion to and from Roman numerals. Courtesy of Mark
  Pilgrim (http://diveintopython.org/).

- statemachine.py: A finite state machine specialized for
  regular-expression-based text filters.

- urischemes.py: Contains a complete mapping of known URI addressing
  scheme names to descriptions.

- utils.py: Contains the ``Reporter`` system warning class and miscellaneous
  utilities.

Subpackages:

- languages: Language-specific mappings of terms.

- parsers: Syntax-specific input parser modules or packages.

- readers: Context-specific input handlers which understand the data
  source and manage a parser.

- transforms: Modules used by readers and writers to modify DPS
  doctrees.

- writers: Format-specific output translators.
"""

__docformat__ = 'reStructuredText'

__version__ = '0.2.2'
"""``major.minor.micro`` version number.  The ``micro`` number is bumped any
time there's a change in the API incompatible with one of the front ends."""


class ApplicationError(StandardError): pass
class DataError(ApplicationError): pass


class OptionSpec:

    """Runtime option specification base class."""

    cmdline_options = ()
    """Command-line option specification.  Override in subclasses.
    Used by `docutils.frontend.OptionParser`.

    One or more sets of option group title, description, and a list/tuple of
    tuples: ``('help text', [list of option strings], {keyword arguments})``.
    Group title and/or description may be `None`; no group title implies no
    group, just a list of single options."""

    relative_path_options = ()
    """Options containing filesystem paths.  Override in subclasses.
    Used by `docutils.frontend.OptionParser`.

    Options listed here are to be interpreted relative to the current working
    directory."""


class Component(OptionSpec):

    """Base class for Docutils components."""

    supported = ()
    """Names for this component.  Override in subclasses."""

    def supports(self, format):
        """
        Is `format` supported by this component?

        To be used by transforms to ask the component (Reader or Writer)
        controlling the transform if that component supports a certain input
        context or output format.
        """
        return format in self.supported
