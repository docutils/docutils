# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Transforms for structural elements.
"""

__docformat__ = 'reStructuredText'

from docutils import nodes
from docutils.transforms import Transform

class Transition(Transform):

    """
    Replace a pending transition by a real transition, moving
    transitions at the end of sections up the tree.  Complain on
    transitions after a title, at the beginning or end of the
    document, and after another transition.

    For example, transform this::

        <section>
            ...
            <transition>
        <section>
            ...

    into this::

        <section>
            ...
        <transition>
        <section>
            ...
    """

    default_priority = 810

    def apply(self):
        node = self.startnode
        lineno = node.line
        index = node.parent.index(node)
        error = None
        if (index == 0 or
            isinstance(node.parent[0], nodes.title) and
            (index == 1 or
             isinstance(node.parent[1], nodes.subtitle) and
             index == 2)):
            assert (isinstance(node.parent, nodes.document) or
                    isinstance(node.parent, nodes.section))
            error = self.document.reporter.error(
                'Document or section may not begin with a transition.',
                line=lineno)
        elif (isinstance(node.parent[index - 1], nodes.pending) and
              isinstance(node.parent[index - 1].transform, self.__class__) or
              isinstance(node.parent[index - 1], nodes.transition)):
            error = self.document.reporter.error(
                'At least one body element must separate transitions; '
                'adjacent transitions are not allowed.', line=lineno)
        if error:
            # Insert before node and update index.
            node.parent.insert(index, error)
            index += 1
        # Create a real transition for later insertion.
        transition = nodes.transition(node.rawsource)
        while index == len(node.parent) - 1:
            node = node.parent
            if node.parent is None:
                # Transition at the end of document.  Do not move the
                # transition up, and place an error behind.
                error = self.document.reporter.error(
                    'Document may not end with a transition.',
                    line=lineno)
                self.startnode.parent.insert(
                    self.startnode.parent.index(self.startnode),
                    [transition, error])
                self.startnode.parent.remove(self.startnode)
                return
            index = node.parent.index(node)
        node.parent.insert(index + 1, transition)
        self.startnode.parent.remove(self.startnode)
