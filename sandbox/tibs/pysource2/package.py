"""package.py - support for calculating package documentation.
"""

import os

class NotAPackageException(Exception):
    pass

class NoSuchDirectoryException(Exception):
    pass

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

    text = '%s<Package filename="%s">\n'%(indent,subpackage)

    for file in files:
        if os.path.isdir(os.path.join(sub_path,file)):
            try:
                text += parse_subpackage(sub_path,file,indent+"  ")
            except NotAPackageException:
                pass

    return text

if __name__ == "__main__":
    result = parse_package("trivial_package")
    print result
