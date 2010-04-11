# A study to check how a Docutils node can be made hashable

from docutils.nodes import Node

class HashableDescriptor(object):
    """A descriptor to plug into a class to be made hashable."""

    override = None
    arity = None
    hashableImpl = None
    debug = False

    def __init__(self, override, arity, hashableImpl):
        self.override = override
        self.arity = arity
        self.hashableImpl = hashableImpl

    def __get__(self, instance, owner):
        if self.debug:
            print('__get__ called on ' + owner.__name__ + ' by '
                  + self.override)
        try:
            fct = self.hashableImpl.getFunction(self.override)
        except:
            if self.debug:
                print('***Exception***')
            raise AttributeError('Can not access method ' + repr(self.override))
        if self.arity == 0:
            return lambda: fct(instance)
        else:
            return lambda other: fct(instance, other)

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
    """Implements equality for a `Node`."""

    def __init__(self):
        super(self.__class__, self).__init__(Node)

if __name__ == '__main__':
    hashableImpl = HashableNodeImpl()
    hashableImpl.debug = True

    n1 = Node()
    n2 = Node()
    print(n1 == n1)
    print(n1 == n2)
    print(n1 != n2)
    h = { n1: 'bla' }
