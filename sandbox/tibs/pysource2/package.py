"""package.py - support for calculating package documentation.

:Author:    Tibs
:Contact:   tibs@tibsnjoan.co.uk
:Revision:  $Revision$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.
"""

__docformat__ = 'reStructuredText'

import os
from docutils.readers.python.moduleparser import Node, parse_module

DEBUG = 0

class NotAPackageException(Exception):
    pass


# ----------------------------------------------------------------------
class Package(Node):
    """This class represents a Python package.

    `filename` is the name of the package - i.e., the package name.
    This may be extended/altered/expanded to include/disambiguate the
    name of the package, the "full" name of the package (e.g., if it is
    a sub-package) and the full path of the package, as needs indicate.

    Note that a package must, by definition, include at least one module,
    i.e., __init__.py (otherwise, it isn't a package).
    """

    def __init__(self, filename):
        """Initialise a Package.

        Note that this does *not* take a "node" argument, since a Package
        is not actually quite like the Module and other sub-nodes.

        @@@ (Actually, there's a case to say that Node should be able to take
        a "node" value of None and cope, in which case our life would be
        easier - I may work on that later on...)
        """
        # Hackery - the following two lines copied from Node itself.
        self.children = []
        self.lineno = None
        self.filename = filename

    def attlist(self):
        return Node.attlist(self, filename=self.filename)



# ----------------------------------------------------------------------
class NotPython(Node):
    """This class is used to represent a non-Python file.

    @@@ If the file isn't Python, should we try for reStructuredText?
    """

    def __init__(self, filename):
        """Initialise a NotPython instance.

        @@@ Same caveats as Package.
        """
        # Hackery - the following two lines copied from Node itself.
        self.children = []
        self.lineno = None
        self.filename = filename

    def attlist(self):
        return Node.attlist(self, filename=self.filename)


# ----------------------------------------------------------------------
def parse_package_or_module(path):
    """Parse a package or module for documentation purposes.

    `path` should either be a directory representing a Python package, or
    a single Python file.
    """
    path = os.path.normpath(path)
    if os.path.isdir(path):
        return parse_package(path)
    else:
        return parse_file(path,path)

def parse_package(package_path):
    """Parse a package for documentation purposes.

    `package_path` should be the system path of the package directory, which is
    not necessarily the same as the Python path...
    """

    if DEBUG: print "Parsing package",package_path

    package_path = os.path.normpath(package_path)
    dir,file = os.path.split(package_path)
    if dir == "":
        dir = "."
    return parse_subpackage(dir,file)

def parse_subpackage(package_path,subpackage):
    """Parse a subpackage for documentation purposes.

    `package_path` should be the system path of the package directory,
    and `subpackage` is the (file) name of the subpackage therein. It
    is assumed that this is already known to be a directory.
    """

    sub_path = os.path.join(package_path,subpackage)

    if DEBUG: print "Parsing sub-package",sub_path

    files = os.listdir(sub_path)
    if "__init__.py" not in files:
        raise NotAPackageException,\
              "Directory '%s' is not a Python package"%sub_path

    node = Package(subpackage)

    # Should we sort the files? Well, if we don't have them in a predictable
    # order, it is harder to test the result(!), and also I believe that it
    # is easier to use the output if there is some obvious ordering. Of course,
    # the question then becomes whether packages and modules should be in the
    # same sequence, or separated.
    files.sort()

    for filename in files:
        fullpath = os.path.join(sub_path,filename)
        if os.path.isdir(fullpath):
            try:
                node.append(parse_subpackage(sub_path,filename))
            except NotAPackageException:
                pass
        else:
            # We do *not* want to try .pyc or .pyo files - we can guarantee
            # that they won't parse (the Python compiler code gets unhappy
            # about NULL bytes therein), and we definitely don't want an
            # entry for such files in our documentation.
            # Similarly, I work on Linux, and don't want to consider files
            # that end with "~" (this last is a bit nasty...)
            if os.path.splitext(filename)[1] not in (".pyc",".pyo") and \
               filename[-1] != "~":
                node.append(parse_file(fullpath,filename))
    return node

def parse_file(fullpath,filename):
    """Parse a single file (which we hope is a Python file).

    * `fullpath` is the full path of the file
    * `filename` is the name we want to use for it in the docutils tree

    Returns a docutils parse tree for said file.
    """

    if DEBUG: print "Parsing file",fullpath

    # @@@ Should we worry about the extension of the file?
    # Trying to use that to predict the contents can be a problem
    # - we already know that we have to worry about ".pyw" as well
    # as ".py", not to mention the possibility (e.g., on Unix) of
    # having removed the extension in order to make an executable
    # file "look" more like a Unix executable. On the whole, it's
    # probably better to try to parse a file, and worry about it
    # not parsing if/when that occurs.
    module = open(fullpath)
    try:
        module_body = module.read()
        try:
            module_node = parse_module(module_body,filename)
        except SyntaxError:
            # OK - it wasn't Python - so what *should* we do with it?
            module_node = NotPython(filename)
            if DEBUG: print "    (not Python)"
        return module_node
    finally:
        module.close()



# ----------------------------------------------------------------------
if __name__ == "__main__":
    result = parse_package("trivial_package")
    print result
