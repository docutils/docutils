"""
Designing the urgently-needed plugin support...

This module doesn't work, it's just there for showing the design, from
a plugin's point of view.  (Kind of test-first.  The required
interfaces will be implemented in Docutils later.)

Let's implement an extension for rendering keys:

reST source:
    :key:`Ctrl+C`
HTML rendering:
    <html><body><span style="border: 1px solid black;">Ctrl</span>+<span style="border: 1px solid black;">C</span></body></html>

That isn't a particularly challenging task, but it's probably best to
stick to something simple first.  (Later we could try to make a plugin
out of a math patch, but that will to require more code.)

Things not (yet) covered here:

* Adding a Component (this may require a generic frontend).
* Writing a test case for an extension.
"""


class KeyNode(nodes.Inline, nodes.TextElement):

    """
    This is the node which stores the key texts (like "Ctrl" or "C").
    """

    def __init__(self, key):
        self += nodes.Text(key, key)


def html_visit_keynode(visitor, node):
    """
    This is a visit_ method which looks like any normal Visitor
    method.

    It is referenced from the writer support class below.
    """
    if visitor.settings['key_html_tt']:
        # Now insert a string into the writer's data stream.  There is
        # visitor.body.append(...) for the HTML visitor, but why not
        # just return the string?  That's much easier.  The visitor
        # should know where to put it.
        return visitor.starttag('tt',
                                style='border: 1px solid black',
                                CLASS='key')
    else:
        return visitor.starttag('span',
                                style='border: 1px solid black',
                                CLASS='key')

def html_depart_keynode(visitor, node):
    """
    Now this is the depart method.
    """
    if visitor.settings['key_html_tt']:
        return '</tt>'
    else:
        return '</span>'


class KeyNodeHTMLSupport(docutils.WriterExtension):

    """
    This class describes HTML writer handling for the KeyNode, i.e. it
    provides visit_ and depart_ methods.  (By the way, is there any
    term for visit_/depart_ methods?)
    """

    extends = 'html4css1'
    """The writer this extension extends."""

    node = 'KeyNode'
    """
    The node we are implementing support for.

    It's a string, because it should be possible to provide support
    for nodes which aren't available (e.g. because they are part of
    another plugin).
    """

    handlers = (html_visit_keynode, html_depart_keynode)
    """
    A pair of visit and departure functions.
    """

    settings_spec = (('Render key buttons in <tt> tags in HTML.',
                      ['--key-html-tt'],
                      {'default': 0, 'action': 'store_true'}),
                     ('Render key buttons in <span> tags in HTML.',
                      ['--key-html-span'],
                      {'dest': 'key_html_tt', 'action': 'store_false'},))
    """
    Sequence of settings.

    There is no 'validator' now, but that's actually not needed.
    Docutils can choose the right validator automatically.
    """


class KeyNodeLaTeXSupport(docutils.WriterExtension):
    
    """Support for the LaTeX writer.  See KeyNodeHTMLSupport."""
    
    extends = 'latex2e'
    
    node = 'KeyNode'
    
    handlers = ('\\fbox{', '}')
    """
    Here we have strings instead of functions.  They are simply
    inserted into the data stream; the visitor should know how to do
    this.

    This is shorter and simpler than using lambdas, e.g. ``(lambda:
    '\\fbox{', lambda: '}')``.
    """


class KeyRole(docutils.ParserExtension):

    """
    This is the role implementation for the reST parser.

    It is only registered at the parser if the parser requests it.

    The reST parser, for example, issues a request when it encounters
    ".. require:: something".  The request procedure might look like
    this:

    Let's say the reST parser encounters a ".. require:: key".  Then
    it calls docutils.register_extension_by_id(self, 'key').  The
    register function determines that the first parameter (self) is a
    component instance of type Parser (so it only considers
    ParserExtensions) and that its name is 'restructuredtext' (so it
    only considers extensions whose `extends` attribute is
    'restructuredtext').

    For all matching extensions, the register function then looks at
    the `id` attribute.  If the second parameter ('key' in this
    example) matches `id`, the extension is registered at the
    component instance passed as first parameter.
    """

    extends = 'restructuredtext'
    """The component this extension extends."""

    id = 'key'
    """
    The id under which this extension is known.

    In this case, it's used for ".. require:: key" in reST.

    The presence of an `id` attribute means that the extension isn't
    loaded automatically but only on request.  (Is that too much
    magic?)
    """

    type = 'role'
    """
    The type of this extension.

    Might also be 'directive', for example.  This attribute is read by
    the reST parser.
    """

    # The rest of this class definition is specific to reST roles:

    name = 'key'
    """
    Name of this role.

    This means we can write :key:`Ctrl+C`.

    There is no i18n support yet, because using a dictionary like
    {'en': 'key'} seems a little bit too complex (since translations
    usually aren't necessary) and the current i18n system needs a
    redesign anyway.
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
            # But this causes a lot of redundancy, given that it's
            # such a common case.  It would be better to have a
            # shortcut like this instead:
            raise self.parser.RoleError('Invalid key string')
            # which does the same as the three lines above.

        # Now comes our highly sophisticated key combination parsing
        # algorithm.
        keys = contents.split('+')
        nodes = [KeyNode(keys[0])]
        for i in keys[1:]:
            nodes.append(nodes.Text('+'))
            nodes.append(KeyNode(i))
        return [nodes], []
