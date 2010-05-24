# Studies for using difflib on structures

from difflib import SequenceMatcher

from pprint import pprint

import sys

from treediff import TreeMatcher, HashableNodeImpl

__docformat__ = 'reStructuredText'

aI = ( 1, 2, 3, 5, 6, 7, )
bI = ( 2, 3, 4, 8, 6 )

sm = SequenceMatcher(None, aI, bI)

# print(sm.find_longest_match(0, len(aI), 0, len(bI)))
# print(sm.get_matching_blocks())
# print(list(sm.get_grouped_opcodes(0))) # Useful for context diffs
# print(sm.get_opcodes()) # The way to go

class TreeNode(object):
    """An example tree node to play with."""

    tag = None
    children = ( )

    def __init__(self, tag, children=( )):
        self.tag = tag
        self.children = children

class HashableTreeNodeImpl(HashableNodeImpl):
    """A `HashableNodeImpl` for `TreeNode`."""

    def __init__(self):
        super(self.__class__, self).__init__(TreeNode)

    def rootHash(self, node):
        return hash(node.tag)

    def childHash(self, node):
        return hash(node)

    def rootEq(self, node, other):
        return type(node) == type(other) and node.tag == other.tag

    def childEq(self, node, other):
        return node == other

    def getChildren(self, node):
        return node.children

hashableNodeImpl = HashableTreeNodeImpl()

aT = ( TreeNode('first'),
       TreeNode('second', (
            TreeNode('second.first'),
            TreeNode('second.second'),
            )),
       TreeNode('third', (
            TreeNode(2),
            )),
       TreeNode('fourth'),
       )

bT = ( TreeNode('first'),
       TreeNode('second', (
            TreeNode('second.first', (
                    TreeNode('second.first.first'),
                    )),
            TreeNode('second.second1'),
            TreeNode('second.second'),
            )),
       TreeNode('second1', (
            TreeNode(2),
            )),
       TreeNode('third', (
            TreeNode(2),
            )),
       TreeNode('fourth1'),
       )

sm = SequenceMatcher(None, aT, bT)
top = sm.get_opcodes()
pprint(top)
print('---')

# Use a pseudo root with different root nodes.
pprint(TreeMatcher(hashableNodeImpl,
                   TreeNode('a', aT), TreeNode('b', bT)).get_opcodes(),
       sys.stdout, 2, 40, None)

# Use a pseudo root with equal root nodes.
pprint(TreeMatcher(hashableNodeImpl,
                   TreeNode(None, aT), TreeNode(None, bT)).get_opcodes(),
       sys.stdout, 2, 40, None)

# To generate a diff tree:
#
# * ``equal`` opcodes need a copy
#
# * ``insert`` and ``delete`` opcodes must be processed as such
#
# * ``replace`` opcodes need to be analyzed recursively to find a
#   minimal set of changes
