"""
This is the example from David Goodger's DPS document pysource-reader.txt.
"""

__docformat__ = "restructuredtext"

class Keeper(Storer):

    """
    Extend `Storer`.  Class attribute `instances` keeps track of
    the number of `Keeper` objects instantiated.
    """

    instances = 0
    """How many `Keeper` objects are there?"""

    def __init__(self):
        """
        Extend `Storer.__init__()` to keep track of instances.

        Keep count in `self.instances` and data in `self.data`.
        """
        Storer.__init__(self)
        self.instances += 1

        self.data = []
        """Store data in a list, most recent last."""

    def storedata(self, data):
        """
        Extend `Storer.storedata()`; append new `data` to a list
        (in `self.data`).

        Use :method:`Keeper.storedata` to store the object's data in
        `Keeper.data`:instance_attribute:.
        """
        self.data = data
