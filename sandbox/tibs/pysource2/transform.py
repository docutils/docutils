"""transform.py - create a docutils Document tree from a Package or Module tree

:Author:    Tibs
:Contact:   tibs@tibsnjoan.co.uk
:Revision:  $1.0$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.
"""

from docutils.utils import new_document
import docutils.nodes as nodes

def make_document(tree):
    """Return a docutils Document tree constructed from this Python tree.

    The tree given must be either a Package or Module tree.
    """

    # @@@ Assumes a Package at the head of the tree
    document = new_document("Package %s"%tree.filename)
    section = make_package_section(tree)
    document.append(section)
    return document

def make_package_section(package,parent=None):
    """Return a docutils tree constructed from this Package tree
    """
    if parent:
        title = "Package %s.%s"%(parent.filename,package.filename)
    else:
        title = "Package %s"%(package.filename)
    section = nodes.section(id=nodes.make_id(title),
                            name=nodes.fully_normalize_name(title))
    title = nodes.title(text=title)
    section.append(title)

    for child in package.children:
        if child.__class__.__name__ == "Package":
            subsection = make_package_section(child,parent=package)
            section.append(subsection)
    return section
