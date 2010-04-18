# Studies for using difflib on structures

from difflib import SequenceMatcher

from hashable import HashableNodeImpl

from pprint import pprint

import sys

__docformat__ = 'reStructuredText'

aI = ( 1, 2, 3, 5, 6, 7, )
bI = ( 2, 3, 4, 8, 6 )

sm = SequenceMatcher(None, aI, bI)

# print(sm.find_longest_match(0, len(aI), 0, len(bI)))
# print(sm.get_matching_blocks())
# print(list(sm.get_grouped_opcodes(0))) # Useful for context diffs
# print(sm.get_opcodes()) # The way to go

class TreeNode(object):

    tag = None
    children = ( )

    def __init__(self, tag, children=( )):
        self.tag = tag
        self.children = children

class HashableTreeNodeImpl(HashableNodeImpl):

    def __init__(self):
        super(self.__class__, self).__init__(TreeNode)

    def rootHash(self, node):
        return hash(node.tag)

    def childHash(self, node):
        """Return a hash for the children only. Subclasses should override
        this."""
        return hash(node)

    def rootEq(self, node, other):
        return type(node) == type(other) and node.tag == other.tag

    def childEq(self, node, other):
        return node == other

    def getChildren(self, node):
        return node.children

# To generate a diff tree:
#
# * ``equal`` opcodes need a copy
#
# * ``insert`` and ``delete`` opcodes must be processed as such
#
# * ``replace`` opcodes need to be analyzed recursively to find a
#   minimal set of changes

def resolveDeepReplace(hashableNodeImpl, opcodes, a, b):
    """Resolves ``replace`` elements in `opcodes` pertaining to `a` and
    `b`. Returns opcodes including nested elements for these cases.
    `hashableNodeImpl` is the `HashableNodeImpl` used to control the hashable
    feature."""
    result = [ ]
    for i in xrange(len(opcodes)):
        ( opcode, aBeg, aEnd, bBeg, bEnd ) = opcodes[i]
        if opcode != 'replace':
            result.append(opcodes[i])
            continue
        try:
            hashableNodeImpl.pushRootOnly(True)
            sm = SequenceMatcher(None, a[aBeg:aEnd], b[bBeg:bEnd])
            rootOpcodes = sm.get_opcodes()
            for j in xrange(len(rootOpcodes)):
                ( subOpcode, aSubBeg, aSubEnd, bSubBeg, bSubEnd ) = rootOpcodes[j]
                if subOpcode != 'equal':
                    result.append(( subOpcode, aBeg + aSubBeg, aBeg + aSubEnd,
                                    bBeg + bSubBeg, bBeg + bSubEnd, ))
                else:
                    for k in xrange(aSubEnd - aSubBeg):
                        aIdx = aBeg + aSubBeg + k
                        bIdx = bBeg + bSubBeg + k
                        result.append(('descend',
                                       aIdx, aIdx + 1, bIdx, bIdx + 1,
                                       resolveRootEqual(hashableNodeImpl,
                                                        a[aIdx], b[bIdx]), ))
        finally:
            hashableNodeImpl.popRootOnly()
    return result

def resolveRootEqual(hashableNodeImpl, aElem, bElem):
    """Considers children of `aElem` and `bElem` which have equal roots.
    Returns opcodes for the children. `hashableNodeImpl` is the
    `HashableNodeImpl` used to control the hashable feature."""
    a = hashableNodeImpl.getChildren(aElem)
    b = hashableNodeImpl.getChildren(bElem)
    try:
        hashableNodeImpl.pushRootOnly(False)
        sm = SequenceMatcher(None, a, b)
        nestedOpcodes = sm.get_opcodes()
        return resolveDeepReplace(hashableNodeImpl, nestedOpcodes, a, b)
    finally:
        hashableNodeImpl.popRootOnly()

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
# Use a pseudo root
pprint(resolveRootEqual(hashableNodeImpl,
                        TreeNode(None, aT), TreeNode(None, bT)),
       sys.stdout, 2, 40, None)
