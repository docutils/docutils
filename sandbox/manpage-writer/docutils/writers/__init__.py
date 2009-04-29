# docutils/writers/__init__.py
# Python package for in-development writers.

import pkgutil

# Ensure we can also reach modules in the system package
__path__ = pkgutil.extend_path(__path__, __name__)

# Make this directory come last when importing from this package
__path__.reverse()

# Make this package gain all the attributes of the system package
_path_prev = __path__
import __init__
globals().update(vars(__init__))
__path__ = _path_prev
del _path_prev

# Make this directory come first when importing from this package
__path__.reverse()
