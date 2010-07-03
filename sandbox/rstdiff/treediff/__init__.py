#!/usr/bin/env python

# Copyright (C) 2010 Stefan Merten

# rstdiff.py is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License,
# or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

from difflib import SequenceMatcher

__docformat__ = 'reStructuredText'

###############################################################################
# Hashable

class HashableDescriptor(object):
    """A descriptor to plug into a class to be made hashable."""

    """Name of function to override."""
    override = None
    """Arity of function to override."""
    arity = None
    hashableImpl = None
    debug = False

    def __init__(self, override, arity, hashableImpl):
        """Initialize a descriptor for function `override` with `arity` using
        `hashableImpl`."""
        self.override = override
        self.arity = arity
        self.hashableImpl = hashableImpl

    def __get__(self, instance, owner):
        """Implements the descriptor protocol. Returns a function to use on
        `instance`."""
        if self.debug:
            print('__get__ called on ' + owner.__name__ + ' by '
                  + self.override)
        try:
            fct = self.hashableImpl.getFunction(self.override)
        except:
            if self.debug:
                print('***Exception***')
            raise AttributeError('Can not access method %r'
                                 % ( self.override, ))
        if self.arity == 0:
            return lambda: fct(instance)
        elif self.arity == 1:
            return lambda other: fct(instance, other)
        else:
            raise AttributeError('Can not create function for %r with arity %d'
                                 % ( self.override, self.arity, ))

class HashableImpl(object):
    """Implements hashability and reflexive equality for a class."""

    debug = False

    def __init__(self, cls):
        """Set methods to implement equality in `cls`."""
        setattr(cls, '__hash__', HashableDescriptor('__hash__', 0, self))
        setattr(cls, '__eq__', HashableDescriptor('__eq__', 1, self))
        setattr(cls, '__ne__', HashableDescriptor('__ne__', 1, self))

    def getFunction(self, name):
        """Return the function named `name`."""
        # Get the function from the *real* object
        return getattr(self, 'impl' + name)

    def impl__hash__(self, this):
        """Return `this.__hash__`(). Derived classes must override this."""
        if self.debug:
            print('impl__hash__ called')
        return id(this)

    def impl__eq__(self, this, other):
        """Return `this.__eq__`(other). Derived classes must override this."""
        if self.debug:
            print('impl__eq__ called')
        return id(this) == id(other)

    def impl__ne__(self, this, other):
        """Return `this.__ne__`(other). Derived classes must override this."""
        if self.debug:
            print('impl__ne__ called')
        return not self.impl__eq__(this, other)

class HashableNodeImpl(HashableImpl):
    """An abstract implementation of `HashableImpl` for nodes in a tree.
    Allows switching between of root only comparison and deep
    comparison."""

    """Consider only the root node (or include the children)."""
    _rootOnly = False
    """Stack for `_rootOnly`"""
    __rootOnlies = [ ]

    def __init__(self, cls):
        HashableImpl.__init__(self, cls)

    def pushRootOnly(self, newRootOnly):
        """Set `newRootOnly` as new `rootOnly` value. If ``True`` then only
        information in the root is considered."""
        self.__rootOnlies.append(self._rootOnly)
        self._rootOnly = newRootOnly

    def popRootOnly(self):
        """Pop and return last `rootOnly` value."""
        self._rootOnly = self.__rootOnlies.pop()

    def impl__hash__(self, node):
        """Returns the hash for node `node`. Subclasses may override this but
        overriding `rootHash` and `childrenHash` may make more sense."""
        rootHash = self.rootHash(node)
        if self._rootOnly:
            return rootHash
        else:
            return rootHash + self.childrenHash(node)

    def impl__eq__(self, node, other):
        """Returns equality between `node` and an `other` node. Subclasses may
        override this but overriding `rootEq` and `childrenEq` may
        make more sense."""
        if not self.rootEq(node, other):
            return False
        if self._rootOnly:
            return True
        return self.childrenEq(node, other)

    def childrenHash(self, node):
        """Return a hash for the children only. Subclasses may override
        this but overriding `childHash` may make more sense."""
        return reduce(lambda x, y: x + y,
                      [ self.childHash(child)
                        for child in self.getChildren(node) ], 0)

    def childrenEq(self, node, other):
        """Returns children equality of `node` and an `other` node. ``True``
        if the children of the two nodes are equal without considering
        the root. Subclasses may override this but overriding
        `childEq` may make more sense."""
        nodeChildren = self.getChildren(node)
        otherChildren = self.getChildren(other)
        if len(nodeChildren) != len(otherChildren):
            return False
        for i in xrange(len(nodeChildren)):
            if not self.childEq(nodeChildren[i], otherChildren[i]):
                return False
        return True

    def rootHash(self, node):
        """Return a hash for the root only. Subclasses must override
        this."""
        raise NotImplementedError()

    def childHash(self, node):
        """Return a hash for the node as a child. Subclasses must override
        this."""
        raise NotImplementedError()

    def rootEq(self, node, other):
        """Returns root equality of `node` and an `other` node. ``True`` if
        the two nodes as roots are equal without considering their
        children. This should be true if one node can be replaced by
        the other and all changes can be represented without changing
        the node itself. Subclasses must override this."""
        raise NotImplementedError()

    def childEq(self, node, other):
        """Returns equality of `node` and an `other` node as children.
        ``True`` if the child features of the two nodes are equal
        without considering the root. Subclasses must override
        this."""
        raise NotImplementedError()

    def getChildren(self, node):
        """Return the children of `node` as a list. Subclasses must override
        this."""
        raise NotImplementedError()        

###############################################################################
# Tree matcher

class TreeMatcher(object):
    """Objects of this class are able to match trees. This is similar in
spirit to `difflib.SequenceMatcher'"""

    a = None
    b = None
    hashableNodeImpl = None
    isJunk = None

    def __init__(self, hashableNodeImpl, a, b, isJunk=None):
        """Construct a TreeMatcher for matching trees `a` and `b`.

`a` and `b` must be the root nodes of two trees to be compared.
`hashableNodeImpl` must be an implementation of `HashableNodeImpl`
governing the comparison of the nodes in the trees.

If `isJunk` is given it must be a one-argument function returning
`True` if the given argument should be considered as junk. """

        self.a = a
        self.b = b
        self.hashableNodeImpl = hashableNodeImpl
        self.isJunk = isJunk

    def get_opcodes(self):
        """Return list of 5- or 6-tuples describing how to turn `a` into `b`.

Each tuple is of the form (tag, i1, i2, j1, j2, [sub]).  The first tuple
has i1 == j1 == 0, and remaining tuples have i1 == i2 from the
tuple preceding it, and likewise for j1 == the previous j2.

The tags are strings, with these meanings:

'replace':  a[i1:i2] should be replaced by b[j1:j2]
'delete':   a[i1:i2] should be deleted.
            Note that j1==j2 in this case.
'insert':   b[j1:j2] should be inserted at a[i1:i1].
            Note that i1==i2 in this case.
'equal':    a[i1:i2] == b[j1:j2]
'descend':  Descend on nodes a[i1] and b[i1]. In this case
            sub is a list of opcodes pertaining to the list of children
            of the two nodes.
            Note that i2==i1+1 and j2==j1+1 in this case.

Note that if the roots of the trees are not root-equal then the result
is only a 'replace' of one tree by the other.
"""

        self.hashableNodeImpl.pushRootOnly(True)
        try:
            sm = SequenceMatcher(self.isJunk, [ self.a, ], [ self.b, ])
            rootOpcodes = sm.get_opcodes()
            if rootOpcodes[0][0] == 'equal':
                return [ ( 'descend', 0, 1, 0, 1,
                           self._resolveRootEqual(self.a, self.b), ) ]
            else:
                return rootOpcodes
        finally:
            self.hashableNodeImpl.popRootOnly()

    def _resolveRootEqual(self, aElem, bElem):
        """Considers children of `aElem` and `bElem` which have equal roots.
        Returns opcodes for the children."""
        a = self.hashableNodeImpl.getChildren(aElem)
        b = self.hashableNodeImpl.getChildren(bElem)
        self.hashableNodeImpl.pushRootOnly(False)
        try:
            sm = SequenceMatcher(self.isJunk, a, b)
            nestedOpcodes = sm.get_opcodes()
            return self._resolveDeepReplace(nestedOpcodes, a, b)
        finally:
            self.hashableNodeImpl.popRootOnly()

    def _resolveDeepReplace(self, opcodes, a, b):
        """Resolves ``replace`` elements in `opcodes` pertaining to `a` and
        `b`. Returns opcodes including nested elements for these cases."""
        result = [ ]
        for i in xrange(len(opcodes)):
            ( opcode, aBeg, aEnd, bBeg, bEnd ) = opcodes[i]
            if opcode != 'replace':
                result.append(opcodes[i])
                continue
            self.hashableNodeImpl.pushRootOnly(True)
            try:
                sm = SequenceMatcher(self.isJunk, a[aBeg:aEnd], b[bBeg:bEnd])
                rootOpcodes = sm.get_opcodes()
                for j in xrange(len(rootOpcodes)):
                    ( subOpcode, aSubBeg, aSubEnd,
                      bSubBeg, bSubEnd ) = rootOpcodes[j]
                    if subOpcode != 'equal':
                        result.append(( subOpcode,
                                        aBeg + aSubBeg, aBeg + aSubEnd,
                                        bBeg + bSubBeg, bBeg + bSubEnd, ))
                    else:
                        for k in xrange(aSubEnd - aSubBeg):
                            aIdx = aBeg + aSubBeg + k
                            bIdx = bBeg + bSubBeg + k
                            result.append(('descend',
                                           aIdx, aIdx + 1, bIdx, bIdx + 1,
                                           self._resolveRootEqual(a[aIdx],
                                                                  b[bIdx]), ))
            finally:
                self.hashableNodeImpl.popRootOnly()
        return result

