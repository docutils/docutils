#! /usr/bin/env python3
# :Copyright: © 2024 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
#
# Revision: $Revision$
# Date: $Date$

"""A Docutils-XML parser.

   Provisional: The API is not fixed yet.
   Defined objects may be renamed or changed in any Docutils release
   without prior notice.
"""

import xml.etree.ElementTree as ET

from docutils import nodes, parsers


class Parser(parsers.Parser):

    """A Docutils-XML parser."""

    supported = ('xml', 'docutils-xml')
    """Aliases this parser supports."""

    config_section = 'xml parser'
    config_section_dependencies = ('parsers',)
    settings_default_overrides = {'doctitle_xform': False,
                                  'validate': True,
                                  }

    def parse(self, inputstring, document):
        """
        Parse `inputstring` and populate `document`, a "document tree".

        Provisional.
        """
        self.setup_parse(inputstring, document)

        # get ElementTree
        root = ET.fromstring(inputstring)
        # convert ElementTree to Docutils Document Tree
        if root.tag == 'document':
            convert_attribs(document, root.attrib)
            for element in root:
                document.append(element2node(element))
        else:
            document.append(element2node(root))

        self.finish_parse()


def element2node(element):
    """
    Convert an `etree` element and its children to Docutils doctree nodes.

    Return a `docutils.nodes.Element` instance.

    Internal.
    """
    # Get the corresponding `nodes.Element` instance:
    nodeclass = getattr(nodes, element.tag)
    node = nodeclass()

    # Attributes: convert and add to `node.attributes`.
    convert_attribs(node, element.attrib)

    # Append text (wrapped in a `nodes.Text` instance)
    append_text(node, element.text)

    # Append children and their tailing text
    for child in element:
        node.append(element2node(child))
        # Text after a child node
        append_text(node, child.tail)

    return node


def convert_attribs(node, a):
    # Convert doctree element attribute values from string to their datatype,
    for key, value in a.items():
        if key.startswith('{'):
            continue  # skip duplicate attributes with namespace URL
        node.attributes[key] = nodes.ATTRIBUTE_VALIDATORS[key](value)


def append_text(node, text):
    if not text:
        return
    if isinstance(node, nodes.TextElement):
        node.append(nodes.Text(text))
    elif text.strip():
        # no TextElement: ignore formatting whitespace
        # but append other text (node becomes invalid!)
        node.append(nodes.Text(text.strip()))
