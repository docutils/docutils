#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

"""

from docutils import nodes
from docutils.nodes import Element, TextElement, Structural, Inline, Part


# =====================
#  Structural Elements
# =====================

class package_section(Structural, Element): pass
class module_section(Structural, Element): pass
class class_section(Structural, Element): pass
class method_section(Structural, Element): pass
class function_section(Structural, Element): pass
class module_attribute_section(Structural, Element): pass
class class_attribute_section(Structural, Element): pass
class instance_attribute_section(Structural, Element): pass

# Structural Support Elements
# ---------------------------

class inheritance_list(Part, Element): pass
class parameter_list(Part, Element): pass
class parameter_item(Part, Element): pass
class optional_parameters(Part, Element): pass
class parameter_tuple(Part, Element): pass
class parameter_default(Part, TextElement): pass
class initial_value(Part, TextElement): pass
class import_item(Part, TextElement): pass

# =================
#  Inline Elements
# =================

# These elements cannot become references until the second
# pass.  Initially, we'll use "reference" or "name".

class package(Part, Inline, TextElement): pass
class module(Part, Inline, TextElement): pass


class inline_class(Part, Inline, TextElement):

    tagname = 'class'


class method(Part, Inline, TextElement): pass
class function(Part, Inline, TextElement): pass
class variable(Inline, TextElement): pass
class parameter(Part, Inline, TextElement): pass
class inline_type(Inline, TextElement):
    tagname = 'type'
class class_attribute(Part, Inline, TextElement): pass
class module_attribute(Part, Inline, TextElement): pass
class instance_attribute(Part, Inline, TextElement): pass
class exception_class(Inline, TextElement): pass
class warning_class(Inline, TextElement): pass

# Collect all the classes we've written above
node_class_names = []
def build_node_class_names():
    for name, var in globals().items():
        if type(var) is types.ClassType \
               and issubclass(var, nodes.Node) \
               and name.lower() == name:
            node_class_names.append(var.tagname or name)

# Register the new node names with GenericNodeVisitor and
# SpecificNodeVisitor:
nodes._add_node_class_names(node_class_names)
