"""package.py - support for calculating package documentation.
"""

import os
from docutils.readers.python.moduleparser import Node

class NotAPackageException(Exception):
    pass

class NoSuchDirectoryException(Exception):
    pass


class Package(Node):
    """This class represents a Python package.

    `filename` is the name of the package - i.e., the package name.
    This may be extended/altered/expanded to include/disambiguate the
    name of the package, the "full" name of the package (e.g., if it is
    a sub-package) and the full path of the package, as needs indicate.
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


def parse_package(package_path):
    """Parse a package for documentation purposes.

    `package_path` should be the system path of the package directory, which is
    not necessarily the same as the Python path...

    Note that the final result is expected to return a Docutils tree, not
    a string - but for the moment a string is easier.
    """

    package_path = os.path.normpath(package_path)
    if not os.path.exists(package_path):
        raise NoSuchDirectoryException,\
              "Directory '%s' does not exist"%package_path

    if not os.path.isdir(package_path):
        raise NotAPackageException,\
              "Directory '%s' is not a Python package"%package_path

    dir,file = os.path.split(package_path)
    if dir == "":
        dir = "."
    return parse_subpackage(dir,file)

def parse_subpackage(package_path,subpackage,indent=""):
    """Parse a subpackage for documentation purposes.

    `package_path` should be the system path of the package directory,
    and `subpackage` is the (file) name of the subpackage therein. It
    is assumed that this is already known to be a directory.

    The indentation is purely for debugging purposes, and should not
    (of course) actually be used in the returned value.
    """

    sub_path = os.path.join(package_path,subpackage)
    files = os.listdir(sub_path)
    if "__init__.py" not in files:
        raise NotAPackageException,\
              "Directory '%s' is not a Python package"%sub_path

    node = Package(subpackage)
    ###text = '%s<Package filename="%s">\n'%(indent,subpackage)

    for file in files:
        if os.path.isdir(os.path.join(sub_path,file)):
            try:
                ###text += parse_subpackage(sub_path,file,indent+"  ")
                node.append(parse_subpackage(sub_path,file))
            except NotAPackageException:
                pass

    ###return text
    return node

if __name__ == "__main__":
    result = parse_package("trivial_package")
    print result
