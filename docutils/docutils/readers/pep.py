#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Python Enhancement Proposal (PEP) Reader.
"""

__docformat__ = 'reStructuredText'


import sys
import os
import re
from docutils import nodes
from docutils.readers import standalone
from docutils.transforms import peps, references
from docutils.parsers import rst


class Reader(standalone.Reader):

    supported = ('pep',)
    """Contexts this reader supports."""

    transforms = (references.Substitutions,
                  peps.Headers,
                  references.Footnotes,
                  references.Hyperlinks,)

    def __init__(self, parser, parser_name):
        """`parser` should be ``None``."""
        if parser is None:
            parser = rst.Parser(rfc2822=1, inliner=Inliner())
        standalone.Reader.__init__(self, parser, '')


class Inliner(rst.states.Inliner):

    """
    Extend `rst.Inliner` to also parse standalone PEP & RFC references.
    """

    rfc_url = 'http://www.faqs.org/rfcs/rfc%d.html'
    pep_url = 'pep-%04d.html'
    pep_ref_pattern = re.compile(r"""
          (
            (pep-\d+(.txt)?)
          |
            (PEP\s+(?P<pepnum>\d+))
          |
            (RFC(-|\s+)?(?P<rfcnum>\d+))
          )
          """, re.VERBOSE)

    def standalone_refs(self, match, lineno):
        text = match.group(0)
        if text.startswith('pep-'):
            ref = os.path.splitext(text)[0] + ".html"
        elif text.startswith('PEP'):
            pepnum = int(match.group('pepnum'))
            ref = self.pep_url % pepnum
        elif text.startswith('RFC'):
            rfcnum = int(match.group('rfcnum'))
            ref = self.rfc_url % rfcnum
        else:
            raise MarkupMismatch
        unescaped = rst.states.unescape(text, 0)
        return [nodes.reference(rst.states.unescape(text, 1), unescaped,
                                refuri=ref)]

    implicit = (rst.states.Inliner.implicit
                + ((pep_ref_pattern, standalone_refs),))
    """PEP-specific list of (pattern, dispatch method) pairs."""
