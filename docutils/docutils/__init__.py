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

- nodes.py: Docutils document tree (doctree) node class library.

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
__version__ = '0.1+'


class ApplicationError(StandardError): pass
class DataError(ApplicationError): pass


class Component:

    """
    Base class for Docutils components.
    """

    supported = ()
    """Names for this component.  Override in subclasses."""

    cmdline_options = ()
    """Command-line option specification.  A list/tuple of tuples:
    ``('help text', [list of option strings], {keyword arguments})``."""

    def supports(self, format):
        """
        Is `format` supported by this component?

        To be used by transforms to ask the component (Reader or Writer)
        controlling the transform if that component supports a certain input
        context or output format.
        """
        return format in self.supported
