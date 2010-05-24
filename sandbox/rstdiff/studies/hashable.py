# A study to check how a Docutils node can be made hashable

from docutils.nodes import Node

from treediff import HashableImpl

__docformat__ = 'reStructuredText'

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
