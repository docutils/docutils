"""transform.py - create a docutils Document tree from a Package or Module tree
"""

from docutils.utils import new_document
import docutils.nodes as nodes

def make_document(tree):
    """Return a docutils Document tree constructed from this Python tree.

    The tree given must be either a Package or Module tree.
    """
    document = new_document("Package trivial_package")
    section = nodes.section(id="package-trivial-package",
                            name="package trivial_package")
    title = nodes.title(text="Package trivial_package")
    section.append(title)
    document.append(section)
    return document
