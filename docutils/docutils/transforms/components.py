#! /usr/bin/env python
"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Docutils component-related transforms.
"""

__docformat__ = 'reStructuredText'

import sys, os, re, time
from docutils import nodes, utils, ApplicationError, DataError
from docutils.transforms import Transform, TransformError


class Filter(Transform):

    """
    Include or exclude elements which depend on a specific Docutils component.

    For use with `nodes.pending` elements.  A "pending" element's dictionary
    attribute ``details`` must contain a key matching the dependency component
    type (e.g. ``details['writer']`` for a "pending" element whose ``stage``
    attribute is 'last writer').  The value is the name of a specific format
    or context of that component (e.g. ``details['writer'] = 'html'``).  If
    the Docutils component which called this transform supports that format or
    context, the "pending" element is replaced by the contents of
    ``details['nodes']`` (a list of nodes); otherwise, the "pending" element
    is removed.

    For example, the reStructuredText "meta" directive creates a "pending"
    element containing a "meta" element (in ``pending.details['nodes']``).
    Only writers supporting the "html" format will include the "meta" element;
    it will be deleted from the output of all other writers.
    """

    def transform(self):
        pending = self.startnode
        component_type = pending.stage.split()[-1] # 'reader' or 'writer'
        component_name = pending.details[component_type]
        if self.component.supports(component_name):
            pending.parent.replace(pending, pending.details['nodes'])
        else:
            pending.parent.remove(pending)
