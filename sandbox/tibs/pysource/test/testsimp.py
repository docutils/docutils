a = 'b'
class Fred:
    """A *silly* demonstration."""

    def __init__(self, b=1, c='jim', d=None, f={'a':1,a:1},
                 g=[x for x in [1,2,3] if x > 2]):
        """Initialise ourselves."""
        self.list = g

    def __call__(self,a):
        """Call ourselves."""
        pass
