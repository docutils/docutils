# Studies for using difflib on structures

from difflib import SequenceMatcher

from hashable import HashableImpl

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

class HashableTreeNodeImpl(HashableImpl):

    """Consider only the root node (or include the children)."""
    # TODO Needs a push/pop methods
    rootOnly = False

    def __init__(self):
        super(self.__class__, self).__init__(TreeNode)

    def impl__hash__(self, this):
        rootHash = self.rootHash(this)
        if self.rootOnly:
            return rootHash
        else:
            return rootHash + self.childrenHash(this)

    def impl__eq__(self, this, other):
        if not self.rootEq(this, other):
            return False
        if self.rootOnly:
            return True
        return self.childrenEq(this, other)

    def rootHash(self, this):
        """Return a hash for the root only."""
        return hash(this.tag)

    def childrenHash(self, this):
        """Return a hash for the children only."""
        return reduce(lambda x, y: x + y,
                      [ hash(child)
                        for child in this.children ], 0)

    def rootEq(self, this, other):
        """True if the two nodes as roots are equal without considering their
        children. This should be true if one node can be replaced by
        the other and all changes can be represented without changing
        the node itself."""
        return type(this) == type(other) and this.tag == other.tag

    def childrenEq(self, this, other):
        """True if the children of the two nodes are equal without considering
        the root features."""
        if len(this.children) != len(other.children):
            return False
        for i in xrange(len(this.children)):
            if not (this.children[i] == other.children[i]):
                return False
        return hashableImpl

# To generate a diff tree:
#
# * ``equal`` opcodes need a copy
#
# * ``insert`` and ``delete`` opcodes must be processed as such
#
# * ``replace`` opcodes need to be analyzed recursively to find a
#   minimal set of changes

def resolveDeepReplace(hashableImpl, opcodes, a, b):
    """Resolves ``replace`` elements in `opcodes` pertaining to `a` and
    `b`. Returns opcodes including nested elements for these cases.
    `hashableImpl` is the `HashableImpl` used to control the hashable
    feature."""
    result = [ ]
    for i in xrange(len(opcodes)):
        ( opcode, aBeg, aEnd, bBeg, bEnd ) = opcodes[i]
        if opcode != 'replace':
            result.append(opcodes[i])
            continue
        try:
            savedRootOnly = hashableImpl.rootOnly
            hashableImpl.rootOnly = True
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
                                       resolveRootEqual(hashableImpl,
                                                        a[aIdx], b[bIdx]), ))
        finally:
            hashableImpl.rootOnly = savedRootOnly
    return result

def resolveRootEqual(hashableImpl, aElem, bElem):
    """Considers children of `aElem` and `bElem` which have equal roots.
    Returns opcodes for the children. `hashableImpl` is the
    `HashableImpl` used to control the hashable feature."""
    a = aElem.children
    b = bElem.children
    try:
        savedRootOnly = hashableImpl.rootOnly
        hashableImpl.rootOnly = False
        sm = SequenceMatcher(None, a, b)
        nestedOpcodes = sm.get_opcodes()
        return resolveDeepReplace(hashableImpl, nestedOpcodes, a, b)
    finally:
        hashableImpl.rootOnly = savedRootOnly

hashableImpl = HashableTreeNodeImpl()

aT = ( TreeNode('first'),
       TreeNode('second', (
            TreeNode('second.first'),
            )),
       TreeNode('third', (
            TreeNode(2),
            )),
       )

bT = ( TreeNode('first'),
       TreeNode('second', (
            TreeNode('second.first', (
                    TreeNode('second.first.first'),
                    )),
            TreeNode('second.second'),
            )),
       TreeNode('second1', (
            TreeNode(2),
            )),
       TreeNode('third', (
            TreeNode(2),
            )),
       TreeNode('fourth'),
       )

sm = SequenceMatcher(None, aT, bT)
top = sm.get_opcodes()
print(top)
print('---')
# Use a pseudo root
print(resolveRootEqual(hashableImpl, TreeNode(None, aT), TreeNode(None, bT)))
