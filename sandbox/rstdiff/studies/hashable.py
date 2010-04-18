# A study to check how a Docutils node can be made hashable

from docutils.nodes import Node

__docformat__ = 'reStructuredText'

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

class HashableDocutilsNodeImpl(HashableImpl):
    """Implements equality for a docutils `Node`."""

    def __init__(self):
        super(self.__class__, self).__init__(Node)

if __name__ == '__main__':
    hashableImpl = HashableDocutilsNodeImpl()
    hashableImpl.debug = True

    n1 = Node()
    n2 = Node()
    print(n1 == n1)
    print(n1 == n2)
    print(n1 != n2)
    h = { n1: 'bla' }
