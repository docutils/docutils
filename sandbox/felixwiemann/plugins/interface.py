"""
Designing the urgently-needed plugin support...

Let's implement an extension for rendering keys.

This module doesn't work, it's just there for showing the design.
(Kind of test-first.  The required interfaces will be implemented in
Docutils later.)

reST source:
    :key:`Ctrl+C`
HTML rendering:
    <html><body><span style="border: 1px solid black;">Ctrl</span>+<span style="border: 1px solid black;">C</span></body></html>

Things not (yet) covered here:

* Adding a Component.
* Adding an option.
* Testing.
"""

class KeyNode(nodes.Inline, nodes.TextElement):

    """
    This is the node which stores the key texts (like "Ctrl" or "C").
    """

    def __init__(self, key):
        self += nodes.Text(key, key)

    visitors = {
        'html4css1': ('<span style="border: 1px solid black;" class="key">',
                      '</span>'),
        'latex2e': ('\\fbox{', '}'),
        }
    """
    Simple replacement for visit_/depart_ methods in Visitor
    classes: A dictionary mapping writer names to visit-departure
    pairs.  visit and departure are either strings (then they're
    simply inserted into the data stream; the visitor should know how
    to do this) or they're functions which are then called like this:
    function(node, visitor)

    (In this context, 'node' is the equivalent to 'self', basically.)

    Note that the keys should be strings, not classes, because the
    writer module might be unavailable (e.g. we might want support
    for the DocBook writer, which isn't included in the standard
    Docutils distribution).

    Maybe the writer (visitor) support should rather be implemented
    outside the node class?  Any suggestion for an elegant design?
    """

class KeyRole(docutils.parsers.rst.Role):

    """
    This is the role implementation for the reST parser.

    To register this class, the Docutils extension system does this,
    basically:

    1. KeyRole.extends => 'restructuredtext parser'
       (The `extends` attribute is defined in the base class
       `parsers.rst.Role` for convenience.)
    2. Find the component which matches 'restructuredtext parser'.
       => a reST parser instance
    3. Call rest_parser_instance.register(KeyRole).
       (The reST parser recognizes that it's a role and adds it.)
    """

    names = ['key', 'shortcut']
    """
    Names of this role.

    This means we can use :key:`Ctrl+C` or :shortcut:`Ctrl+C`.

    There is no i18n support yet, because using a dictionary like
    {'en': ['key', 'shortcut']} seems a little bit too complex (since
    translations usually aren't necessary) and the current i18n system
    needs a redesign anyway.
    """

    raw = 1
    """
    The run() method wants to get a raw string, so we set raw to 1.
    (Backslashes aren't interpreted then, but that isn't important in
    the case of our key role.)

    If `raw` were 0, the run() method would get a list with one Text
    node.  Backslashes would be interpreted, and if there were
    nested-inline-markup support, the list might contain any Inline
    elements.
    """

    def run(self, contents):
        """
        For the source text ":key:`Ctrl+C`", this method is called as
        keyrole_instance.run('Ctrl+C').

        lineno, inliner etc. aren't passed as parameters but they can
        be grabbed from instance variables if needed (like
        self.lineno).  This avoids long useless copy'n'paste parameter
        lists.

        Return value is a tuple of a node list and a system-message
        list.
        """

        if (not contents or
            contents.startswith('+') or contents.endswith('+')
            or ' ' in contents):
            # Not a valid key combination.
            # Now we want to throw an error, but that isn't easy.
            # Theoretically, we could do:
            msg = self.inliner.reporter.error(
                'Invalid key string: %s' % contents, lineno=self.lineno)
            prb = self.inliner.problematic(contents, contents, msg)
            return [prb], [msg]
            # But this contains a lot of redundancy, given that it's
            # such a common case.  It would be better to have a
            # shortcut like this instead:
            raise docutils.parsers.rst.RoleError('Invalid key string')
            # or even
            raise self.Error('Invalid key string')
            # which would both do the same as the three lines above.

        # Now comes our highly sophisticated key combination parsing
        # algorithm.
        keys = contents.split('+')
        nodes = [KeyNode(keys[0])]
        for i in keys[1:]:
            nodes.append(nodes.Text('+'))
            nodes.append(KeyNode(i))
        return [nodes], []


class KeyExtension(docutils.Extension):

    """
    This is the class describing the extension.

    As it's a subclass of docutils.Extension, it is automatically
    recognized by Docutils, so we don't need to register the extension
    anywhere.
    """

    names = ['key']
    """For ".. require:: key" in reST."""

    nodes = [KeyNode]
    """Nodes this extension adds.  Do we need this?"""

    extension_classes = [KeyRole]
    """
    Classes to be registered at the component instances.

    Is there a more precise name than `extension_classes`?  After all,
    the KeyRole class isn't an extension by itself, but a *part* of an
    extension.  We need a concise term for such classes.  Some ideas:

    * enhancers
    * extenders
    * extension_components
    * extension_classes (that's what I chose, but it isn't nice)
    """
