"""A simple test file for input to ast-mining.py.

But it isn't a "raw" string, so:

    \\*escape* \\`with` "\\\\"
"""

__docformat__ = "reST"

import string

one = 1

"""Documentation after `one` (intervening blank line)"""

two,three = 2,3
"""Documenatation after ``two,three=2,3``"""

four,five = two,three

six = [four,five]
"""Documentation after ``six=[four,five]``"""

global gmodule
gmodule = 1
"""Global at the module level"""

def func(a,b=1,c='jim',d=None,e=[],f={'a':1},g=(1,2,3)):
    r"""Function at the module level

    This *is* a "raw" string, so:

        \*escape* \`with` "\\"
    """

    from string import lstrip,rstrip

    a = 3

    global gfunc
    gfunc = 1
    """Global referenced only within `func()`"""

    global one
    one = 2

    class InFunc:
        pass

    def infunc():
        """Function defined inside function `func()`"""
        global ginner
        ginner = 1
        """Global referenced only within `infunc()`"""

        class Silly:
            pass

        def silly_fn():
            pass

def func2(a,*args,**kargs):
    pass

def func3((a,b,c),d):
    pass

class Fred:
    """Class at the module level."""

    a = 1
    """Documentation for class value `a`"""

    global gclass
    gclass = 2
    """Global referenced only within `Fred`"""

    def __init__(self):
        """Initialisation for `Fred`"""
        self.b = 2
        """`self.b` within `Fred.__init__()`"""

        c = 3
        """Local variable within `Fred.__init__()`"""

        global gmeth
        gmeth = 3
        """Global referenced only within a method"""

        global gagain
        gagain = 3

        class Insider:
            """Class defined inside `Fred.__init__()`"""

            a = 'jim'
            """Local name within class inside `Fred.__init__()`"""

            global gclass2
            gclass2 = 4
            """Global referenced only within a class within a method"""

            global gunused
            global gagain

            def fred(self):
                global gunused2,gfunc2
                gfunc2 = 5
                """Global referenced only within a method in a
                class in a method
                """

                def fredinner():
                    global ginner2
                    global gagain
                    ginner2 = 6
                    gagain  = 6

                    infredinner = 7

class Jim(Fred):
    pass

            
