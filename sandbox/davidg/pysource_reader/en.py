#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

"""

interpreted = {
      'package': nodes.package,
      'module': nodes.module,
      'class': nodes.inline_class,
      'method': nodes.method,
      'function': nodes.function,
      'variable': nodes.variable,
      'parameter': nodes.parameter,
      'type': nodes.type,
      'class attribute': nodes.class_attribute,
      'classatt': nodes.class_attribute,
      'instance attribute': nodes.instance_attribute,
      'instanceatt': nodes.instance_attribute,
      'module attribute': nodes.module_attribute,
      'moduleatt': nodes.module_attribute,
      'exception class': nodes.exception_class,
      'exception': nodes.exception_class,
      'warning class': nodes.warning_class,
      'warning': nodes.warning_class,}
"""Mapping of interpreted text role name to nodes.py class."""
