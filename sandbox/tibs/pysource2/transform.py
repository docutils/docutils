"""transform.py - create a docutils Document tree from a Package or Module tree

:Author:    Tibs
:Contact:   tibs@tibsnjoan.co.uk
:Revision:  $Revision$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.
"""

__docformat__ = 'reStructuredText'

import os
from docutils.utils import new_document
import docutils.nodes as nodes
from package import Package, NotPython
from docutils.readers.python.moduleparser import Module, Class, Docstring

def make_document(tree,settings=None):
    """Return a docutils Document tree constructed from this Python tree.

    The tree given must be either a Package or Module tree.
    """

    # @@@ Can it ever be anything other than a package or module?
    # I'd assert not - the module is the basic "smallest unit".
    # Should we test that?
    if isinstance(tree,Package):
        document = new_document("Package %s"%tree.filename,settings)
        section = make_package_section(tree)
    else:
        document = new_document("Module %s"%os.path.splitext(tree.filename)[0],
                                settings)
        section = make_module_section(tree)
    document.append(section)
    return document

def make_package_section(tree,parent_name=None):
    """Return a docutils tree constructed from this Package tree
    """
    if parent_name:
        tree_name = "%s.%s"%(parent_name,tree.filename)
    else:
        tree_name = tree.filename
    title = "Package %s"%(tree_name)

    # @@@ Do I really want to normalise (case fold, in particular)
    # the id/name for this section? Python names can legitimately
    # distinguish case, and whilst that's not terribly useful at
    # the file level (since not all OS/filesystems keep such a
    # distinction), it certainly is a valid possibility *within*
    # a file...
    #
    # make_id() produces a name that starts with [a-z] and continues
    # with a-z, 0-9 and hyphen (or something like that).
    #
    # fully_normalize_name() reduces spaces to single spaces (OK),
    # but also lowercases.
    #
    # @@@ Think more on usage here, I guess
    section = nodes.section(CLASS="package",id=nodes.make_id(title),
                            name=nodes.fully_normalize_name(title))
    title = nodes.title(text=title)
    section.append(title)

    # @@@ I'm enforcing an order of modules before non-python files before
    # subpackages here
    #     - do I really care?
    #     - do I want some other way order?
    #     - is this the best way to do it (e.g., I could sort the children
    #       into order first instead)
    for child in tree.children:
        if isinstance(child,Module):
            subsection = make_module_section(child,tree_name)
            section.append(subsection)
    for child in tree.children:
        if isinstance(child,NotPython):
            subsection = make_not_python_section(child,tree_name)
            section.append(subsection)
    for child in tree.children:
        if isinstance(child,Package):
            subsection = make_package_section(child,tree_name)
            section.append(subsection)
    return section

def make_module_section(tree,parent_name=None):
    """Return a docutils tree constructed from this Module sub-tree
    """
    module_name = os.path.splitext(tree.filename)[0]
    if parent_name:
        tree_name = "%s.%s"%(parent_name,module_name)
    else:
        tree_name = module_name
    title = "Module %s"%(tree_name)

    # @@@ Same considerations on id/name as above
    section = nodes.section(CLASS="module",id=nodes.make_id(title),
                            name=nodes.fully_normalize_name(title))
    title = nodes.title(text=title)
    section.append(title)

    # Assume that the docstring must be the first child
    if len(tree.children) > 0 and \
       isinstance(tree.children[0],Docstring):
        section.append(make_docstring(tree.children[0]))

    # @@@ Again, I'm looking for classes before anything else
    for child in tree.children:
        if isinstance(child,Class):
            subsection = make_class_section(child,tree_name)
            section.append(subsection)

    return section

def make_not_python_section(tree,parent_name=None):
    """Return a docutils tree constructed from this NotPython (file) sub-tree
    """
    if parent_name:
        tree_name = "%s.%s"%(parent_name,tree.filename)
    else:
        tree_name = tree.filename
    title = "File %s"%(tree_name)

    # @@@ Same considerations on id/name as above
    section = nodes.section(CLASS="file",id=nodes.make_id(title),
                            name=nodes.fully_normalize_name(title))
    title = nodes.title(text=title)
    section.append(title)
    paragraph = nodes.paragraph(text="File ")
    paragraph.append(nodes.literal(text=tree.filename))
    paragraph.append(nodes.Text(" is not a Python module."))
    section.append(paragraph)
    return section

def make_class_section(tree,parent_name):
    """Return a docutils tree constructed from this Class sub-tree
    """
    tree_name = "%s.%s"%(parent_name,tree.name)
    title = "Class %s"%(tree_name)

    # @@@ Same considerations on id/name as above
    section = nodes.section(CLASS="class",id=nodes.make_id(title),
                            name=nodes.fully_normalize_name(title))
    title = nodes.title(text=title)
    section.append(title)

    # Assume that the docstring must be the first child
    if len(tree.children) > 0 and \
       isinstance(tree.children[0],Docstring):
        section.append(make_docstring(tree.children[0]))

    # @@@ Don't forget that we want base classes to be named at
    # some point

    return section

def make_docstring(docstring):
    return nodes.literal_block(text=docstring.text,CLASS="docstring")
