#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

"""

from dps import nodes
from dps.nodes import Element, TextElement, Structural, Component, Inline


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

class inheritance_list(Component, Element): pass
class parameter_list(Component, Element): pass
class parameter_item(Component, Element): pass
class optional_parameters(Component, Element): pass
class parameter_tuple(Component, Element): pass
class parameter_default(Component, TextElement): pass
class initial_value(Component, TextElement): pass


# =================
#  Inline Elements
# =================

class package(Component, Inline, TextElement): pass
class module(Component, Inline, TextElement): pass


class inline_class(Component, Inline, TextElement):

    tagname = 'class'


class method(Component, Inline, TextElement): pass
class function(Component, Inline, TextElement): pass
class variable(Inline, TextElement): pass
class parameter(Component, Inline, TextElement): pass
class type(Inline, TextElement): pass
class class_attribute(Component, Inline, TextElement): pass
class module_attribute(Component, Inline, TextElement): pass
class instance_attribute(Component, Inline, TextElement): pass
class exception_class(Inline, TextElement): pass
class warning_class(Inline, TextElement): pass


class SpecificVisitor(nodes.SpecificVisitor):

    """
    """

    def visit_class_attribute(self, node, ancestry): pass
    def visit_class_attribute_section(self, node, ancestry): pass
    def visit_class_section(self, node, ancestry): pass
    def visit_exception_class(self, node, ancestry): pass
    def visit_function(self, node, ancestry): pass
    def visit_function_section(self, node, ancestry): pass
    def visit_inheritance_list(self, node, ancestry): pass
    def visit_initial_value(self, node, ancestry): pass
    def visit_inline_class(self, node, ancestry): pass
    def visit_instance_attribute(self, node, ancestry): pass
    def visit_instance_attribute_section(self, node, ancestry): pass
    def visit_method(self, node, ancestry): pass
    def visit_method_section(self, node, ancestry): pass
    def visit_module(self, node, ancestry): pass
    def visit_module_attribute(self, node, ancestry): pass
    def visit_module_attribute_section(self, node, ancestry): pass
    def visit_module_section(self, node, ancestry): pass
    def visit_optional_parameters(self, node, ancestry): pass
    def visit_package(self, node, ancestry): pass
    def visit_package_section(self, node, ancestry): pass
    def visit_parameter(self, node, ancestry): pass
    def visit_parameter_default(self, node, ancestry): pass
    def visit_parameter_item(self, node, ancestry): pass
    def visit_parameter_list(self, node, ancestry): pass
    def visit_parameter_tuple(self, node, ancestry): pass
    def visit_type(self, node, ancestry): pass
    def visit_variable(self, node, ancestry): pass
    def visit_warning_class(self, node, ancestry): pass
