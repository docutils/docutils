# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Standalone file Reader for the reStructuredText markup syntax.
"""

__docformat__ = 'reStructuredText'


import sys
from docutils import readers
from docutils.transforms import frontmatter, references
from docutils.parsers.rst import Parser


class Reader(readers.Reader):

    supported = ('standalone',)
    """Contexts this reader supports."""

    document = None
    """A single document tree."""

    settings_spec = (
        'Standalone Reader',
        'The standalone reader defines a list of transforms.',
        (
          ('Disable doctitle transform.  By default the transform is enabled.',
            ['--no-doc-title'],
            {'default': 1, 'action': 'store_false' },
          ),
          ('Disable docinfo transform.  By default the transform is enabled.',
            ['--no-doc-info'],
            {'default': 1, 'action': 'store_false' },
          ),
        )
        )
 
    default_transforms = (references.Substitutions,
                          frontmatter.DocTitle,
                          frontmatter.DocInfo,
                          references.ChainedTargets,
                          references.AnonymousHyperlinks,
                          references.IndirectHyperlinks,
                          references.Footnotes,
                          references.ExternalTargets,
                          references.InternalTargets,)
