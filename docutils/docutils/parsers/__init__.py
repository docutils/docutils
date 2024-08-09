# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
This package contains Docutils parser modules.
"""

from __future__ import annotations

__docformat__ = 'reStructuredText'

import importlib
from typing import TYPE_CHECKING, overload

from docutils import Component, frontend, transforms

if TYPE_CHECKING:
    from typing import Final, Literal

    from docutils import nodes
    from docutils.parsers import (
        commonmark_wrapper,
        docutils_xml,
        null,
        rst,
        recommonmark_wrapper,
    )
    from docutils.transforms import Transform

    from myst_parser import docutils_ as myst_wrapper


class Parser(Component):
    settings_spec = (
        'Generic Parser Options',
        None,
        (('Disable directives that insert the contents of an external file; '
          'replaced with a "warning" system message.',
          ['--no-file-insertion'],
          {'action': 'store_false', 'default': True,
           'dest': 'file_insertion_enabled',
           'validator': frontend.validate_boolean}),
         ('Enable directives that insert the contents '
          'of an external file. (default)',
          ['--file-insertion-enabled'],
          {'action': 'store_true'}),
         ('Disable the "raw" directive; '
          'replaced with a "warning" system message.',
          ['--no-raw'],
          {'action': 'store_false', 'default': True, 'dest': 'raw_enabled',
           'validator': frontend.validate_boolean}),
         ('Enable the "raw" directive. (default)',
          ['--raw-enabled'],
          {'action': 'store_true'}),
         ('Maximal number of characters in an input line. Default 10 000.',
          ['--line-length-limit'],
          {'metavar': '<length>', 'type': 'int', 'default': 10_000,
           'validator': frontend.validate_nonnegative_int}),
         ('Validate the document tree after parsing.',
          ['--validate'],
          {'action': 'store_true',
           'validator': frontend.validate_boolean}),
         ('Do not validate the document tree. (default)',
          ['--no-validation'],
          {'action': 'store_false', 'dest': 'validate'}),
         )
        )
    component_type: Final = 'parser'
    config_section: Final = 'parsers'

    def get_transforms(self) -> list[type[Transform]]:
        return super().get_transforms() + [transforms.universal.Validate]

    def parse(self, inputstring: str, document: nodes.document) -> None:
        """Override to parse `inputstring` into document tree `document`."""
        raise NotImplementedError('subclass must override this method')

    def setup_parse(self, inputstring: str, document: nodes.document) -> None:
        """Initial parse setup.  Call at start of `self.parse()`."""
        self.inputstring = inputstring
        # provide fallbacks in case the document has only generic settings
        document.settings.setdefault('file_insertion_enabled', False)
        document.settings.setdefault('raw_enabled', False)
        document.settings.setdefault('line_length_limit', 10_000)
        self.document = document
        document.reporter.attach_observer(document.note_parse_message)

    def finish_parse(self) -> None:
        """Finalize parse details.  Call at end of `self.parse()`."""
        self.document.reporter.detach_observer(
            self.document.note_parse_message)


@overload
def get_parser_class(parser_name: Literal['null']) -> type[null.Parser]:
    ...


@overload
def get_parser_class(
    parser_name: Literal['rst', 'restructuredtext']
) -> type[rst.Parser]:
    ...


@overload
def get_parser_class(
    parser_name: Literal['xml', 'docutils_xml']
) -> type[docutils_xml.Parser]:
    ...


@overload
def get_parser_class(
    parser_name: Literal['recommonmark']
) -> type[recommonmark_wrapper.Parser]:
    ...


@overload
def get_parser_class(
    parser_name: Literal['myst']
) -> type[myst_wrapper.Parser]:
    ...


@overload
def get_parser_class(
    parser_name: Literal['commonmark', 'markdown']
) -> type[commonmark_wrapper.Parser]:
    ...


@overload
def get_parser_class(parser_name: str) -> type[Parser]:
    ...


def get_parser_class(parser_name: str) -> type[Parser]:
    """Return the Parser class from the `parser_name` module."""
    name = parser_name.lower()

    # short names for known parsers
    if name == 'null':
        from docutils.parsers import null
        return null.Parser
    if name in {'rst', 'restructuredtext', 'rest', 'restx', 'rtxt'}:
        from docutils.parsers import rst
        return rst.Parser
    if name in {'docutils_xml', 'xml'}:
        from docutils.parsers import docutils_xml
        return docutils_xml.Parser

    try:
        # 3rd-party Markdown parsers
        # (pycmark works out of the box)
        if name == 'recommonmark':
            from docutils.parsers import recommonmark_wrapper
            return recommonmark_wrapper.Parser
        if name == 'myst':
            from myst_parser import docutils_ as myst_wrapper
            return myst_wrapper.Parser

        # dispatcher for 3rd-party Markdown parsers
        if name in {'commonmark', 'markdown'}:
            from docutils.parsers import commonmark_wrapper
            return commonmark_wrapper.Parser

        # fallback to importing a fully-qualified name
        module = importlib.import_module(name)
    except ImportError as err:
        raise ImportError(f'Parser "{parser_name}" not found. {err}')
    else:
        return module.Parser
