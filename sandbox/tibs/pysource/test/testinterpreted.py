"""Test interpreted text

This module (`testinterpreted`) contains one :class:`Fred`
(and one hopes `Fred` will also work) and a function *and*
a method called `fred`.

NB: the above sentence *should*, when transformed into HTML
or some other output format, hopefully, contain text something
like::

    contains one class Fred

i.e., the ``:class:`Fred``` should probably cause the "insertion"
of the word "class". Or so I think.
""" #'

__docformat__ = "reST"

class Fred:
    """This class (`Fred`), in :module:`testinterpreted`, contains
    a method `fred`.
    """

    jim = None
    """A class value, a default.
    """

    def fred(self):
        """This method (`fred`) is in class `Fred`
        """
        fred = 3
        """This name (`fred`) is in :function:`fred` in class `Fred`.
        """

def fred(fred):
    """This function (:function:`fred`) is *not* in class `Fred`.

    It has one argument, :parameter:`fred`.

    If I just say `fred`, what *should* that resolve to?
    """
    fred.jim = 3
    """We assume that `fred` must be something with a `jim` attribute...
    """
