"""This module explores a different approach to building doctree elements...

The "normal" way of building a DOCUTILS tree involves, well, building a tree
structure. So one might do::

    section = docutils.nodes.section()
    section += docutils.nodes.title(text="Title")
    para = docutils.nodes.paragraph()
    section += para
    para += docutils.nodes.Text("Some ")
    para += docutils.nodes.strong(text="strong text.")

That's all very nice, if one is *thinking* in terms of a tree structure,
but it is not, for me, a very natural way to construct a text.

    (OK - I *know* in practice one would also have imported `paragraph`,
    etc., from `docutils.nodes`, but that is not my point.)

This module allows one to use a more LaTex style of construction, with
begin and end delimitors for DOCUTILS nodes. Thus the above example becomes::

    build.start("section")
    build.add("title","Title")
    build.start("paragraph","Some ")
    build.add("strong","Strong text.")
    build.end("section")

(As a convenience, paragraphs are automatically ended.)

A slightly shorter, and possibly more obfuscated, way of writing this
would be::

    build.start("section",build.make("title","Title")
    build.start("paragraph","Some ",build.make("strong","Strong text."))
    build.end("section")

Sometimes I think that sort of approach makes more sense, sometimes not.
""" # we need a " to keep [X]Emacs Python mode happy.

import string
import docutils.nodes
import docutils.utils

__docformat__ = "reST"


# ----------------------------------------------------------------------
class group(docutils.nodes.Element):
    """Group is a way of grouping together elements.

    Compare it to HTML <div> and <span>, or to TeX (?check?)
    \begingroup and \endgroup.

    It takes the special attribute `style`, which indicates what
    sort of thing it is grouping - for instance, "docstring" or
    "attributes".

    Although it (should be) supplied by the standard DOCUTILS tree,
    reST itself does not use `group`. It is solely used by
    extensions, such as ``pysource``.

    In the default HTML Writer, `group` renders invisibly
    (that is, it has no effect at all on the formatted output).
    """

    pass


# ----------------------------------------------------------------------
class BuildTree:

    def __init__(self, with_groups=1, root=None):
        self.stack = []
        """A stack of tuples of the form ("classname",classinstance).
        """

        self.root = root
        """A memory of the first item on the stack (notionally, the
        "document") - we need this because if we `start` a document,
        fill it up, and then `end` it, that final `end` will remove
        the appropriate instance from the stack, leaving no record.
        Thus this is that record.
        """
        if root is not None:
            self._stack_add(root)

        self.with_groups = with_groups

    def finish(self):
        """Call this to indicate we have finished.

        It will grumble if anything is left unclosed, but will
        return the "root" instance of the DOCUTILS tree we've been
        building if all is well...
        """
        if len(self.stack) > 0:
            raise ValueError,"Items still outstanding on stack: %s"%\
                  self._stack_as_string()
        else:
            return self.root

    def add(self,thing,*args,**keywords):
        """Add a `thing` DOCUTILS node at the current level.

        For instance::

            build.add("paragraph","Some simple text.")

        If `thing` is "text" then it will automagically be converted
        to "Text" (this makes life easier for the user, as all of the
        other DOCUTILS node classes they are likely to use start with a
        lowercase letter, and "Text" is the sole exception).

        See `make` (which this uses) for more details of the arguments.
        """
        if thing == "group" and not self.with_groups:
            return

        instance = self.make(thing,*args,**keywords)
        self._stack_append(instance)

    def addsubtree(self,subtree):
        """Add a DOCUTILS subtree to the current item.
        """
        self._stack_append(subtree)

    def current(self):
        """Return the "current" item.

        That is, return the item to which `add()` will add DOCUTILS nodes.
        """
        return self._stack_current()

    def start(self,thing,*args,**keywords):
        """Add a `thing` DOCUTILS node, starting a new level.

        `thing` should be either the name of a docutils.nodes class, or
        else a class itself.

        If `thing` is "text" then it will automagically be converted
        to "Text" (this makes life easier for the user, as all of the
        other DOCUTILS node classes they are likely to use start with a
        lowercase letter, and "Text" is the sole exception).

        For instance::

            build.start("bullet_list")

        As a convenience, if `thing` is a paragraph, and if the current
        item is another paragraph, this method will end the old paragraph
        before starting the new.

        Note that if `thing` is "document", some extra magic is worked
        internally. If the keywords `warninglevel` and `errorlevel` are
        given, they will be passed to a docutils.utils.Reporter instance,
        as well as being passed down to the `document` class's initialiser.

        See `make` (which this uses) for more details of the arguments.
        """
        name = self._nameof(thing)

        if name == "group" and not self.with_groups:
            return

        if name == "paragraph" and self._stack_ends("paragraph"):
            self.end("paragraph")

        if name == "document":
            if self.root:
                return
            if len(self.stack) > 0:
                raise ValueError,\
                      "Cannot insert 'document' except at root of stack"
            warninglevel = keywords.get("warninglevel",2)
            errorlevel = keywords.get("errorlevel",4)
            reporter = docutils.utils.Reporter('fubar', warninglevel,
                errorlevel)
            instance = docutils.nodes.document(reporter,"en")
        else:
            instance = self.make(thing,*args,**keywords)

        if len(self.stack) == 0:
            self.root = instance
        else:
            self._stack_append(instance)

        self._stack_add(instance)

    def end(self,thing):
        """End the level started below a `thing` DOCUTILS node.

        `thing` should be either the name of a docutils.nodes class, or
        else a class itself.

        For instance::

            build.end("bullet_list")

        As a convenience, if the last item constructed was actually
        a paragraph, and `thing` is the container for said paragraph,
        then the paragraph will be automatically ended.

        Otherwise, for the moment at least, the `thing` being ended
        must be the last thing that was begun (in the future, we *might*
        support automatic "unrolling" of the stack, but not at the
        moment).
        """
        name = self._nameof(thing)

        if thing == "group" and not self.with_groups:
            return

        if self._stack_ends("paragraph") and name != "paragraph":
            self.end("paragraph")

        self._stack_remove(name)

    def make(self,thing,*args,**keywords):
        """Return an instance of `docutils.nodes.thing`

        Attempts to regularise the initialisation of putting initial
        text into an Element and a TextElement...

        `thing` should be either the name of a docutils.nodes class, or
        else a class itself (so, for instance, one might call
        ``build.make("paragraph")`` or
        ``build.make(docutils.nodes.paragraph)``),
        or else None.

        If `thing` is "text" then it will automagically be converted
        to "Text" (this makes life easier for the user, as all of the
        other DOCUTILS node classes they are likely to use start with a
        lowercase letter, and "Text" is the sole exception).

        If `thing` is an Element subclass, then the arguments are just
        passed straight through - any *args list is taken to be children
        for the element (strings are coerced to Text instances), and any
        **keywords are taken as attributes.

        If `thing` is an TextElement subclass, then if the first
        item in *args is a string, it is passed down as the `text`
        parameter. Any remaining items from *args are used as child
        nodes, and any **keywords as attributes.

        If `thing` is a Text subclass, then a single argument is expected
        within *args, which must be a string, to be used as the Text's
        content.

        For instance::

            n1 = build.make("paragraph","Some ",
                           build.make("emphasis","text"),
                           ".",align="center")
            n2 = build.make(None,"Just plain text")
        """

        #print "make: %s, %s, %s"%(thing,args,keywords)

        # Temporary special case - since group is not (yet) in docutils.nodes...
        if thing == "group":
            thing = group

        if thing == None:
            dps_class = docutils.nodes.Text
        elif type(thing) == type(""):
            if thing == "text":
                thing = "Text"
            try:
                dps_class = getattr(docutils.nodes,thing)
            except AttributeError:
                raise ValueError,"docutils.nodes does not define '%s'"%thing
        else:
            dps_class = thing

        # NB: check for TextElement before checking for Element,
        # since TextElement is itself a subclass of Element!
        if issubclass(dps_class,docutils.nodes.TextElement):
            # Force the use of the argument list as such, by insisting
            # that the `rawsource` and `text` arguments are empty strings
            args = self._convert_args(args)
            dps_instance = dps_class("","",*args,**keywords)
        elif issubclass(dps_class,docutils.nodes.Element):
            # Force the use of the argument list as such, by insisting
            # that the `rawsource` arguments is an empty string
            args = self._convert_args(args)
            dps_instance = dps_class("",*args,**keywords)
        elif issubclass(dps_class,docutils.nodes.Text):
            if len(args) > 1:
                raise ValueError,\
                      "Text subclass %s may only take one argument"%\
                      self._nameof(thing)
            elif len(args) == 1:
                text = args[0]
            else:
                text = ""
            if keywords:
                raise ValueError,\
                      "Text subclass %s cannot use keyword arguments"%\
                      self._nameof(thing)
            dps_instance = dps_class(text)
        else:
            raise ValueError,"%s is not an Element or TextElement"%\
                  self._nameof(thing)

        #print "   ",dps_instance
        return dps_instance

    def _convert_args(self,args):
        """Return the arguments, with strings converted to Texts.
        """
        newargs = []
        for arg in args:
            if type(arg) == type(""):
                newargs.append(docutils.nodes.Text(arg))
            else:
                newargs.append(arg)
        return newargs

    def __getattr__(self,name):
        """Return an appropriate DOCUTILS class, for instantiation.
        """
        return getattr(docutils.nodes,name)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def _nameof(self,thing):
        if thing is None:
            return "Text"
        elif type(thing) == type(""):
            return thing
        else:
            return thing.__name__

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Stack maintenance

    def _stack_ends(self,name):
        """Return true if the stack ends with the named entity.
        """
        return self.stack[-1][0] == name

    def _stack_add(self,instance):
        """Add a new level to the stack.
        """
        self.stack.append((instance.__class__.__name__,instance))

    def _stack_remove(self,name):
        """Remove the last level from the stack

        (but only if it is of the right sort).
        """
        if len(self.stack) == 0:
            raise ValueError,"Cannot end %s - nothing outstanding to end"%\
                  (name)
        if name != self.stack[-1][0]:
            raise ValueError,"Cannot end %s - last thing begun was %s"%\
                  (name,self.stack[-1][0])
        del self.stack[-1]

    def _stack_append(self,instance):
        """Append an instance to the last item on the stack.
        """
        if len(self.stack) > 0:
            self.stack[-1][1].append(instance)
        else:
            raise ValueError,"Cannot add %s to current level" \
                  " - nothing current"%(instance.__class__.__name__)

    def _stack_current(self):
        """Return the "current" element from the stack

        That is, the element to which we would append any new instances
        with `_stack_append()`
        """
        return self.stack[-1][1]

    def _stack_as_string(self):
        names = []
        for name,inst in self.stack:
            names.append(name)
        return string.join(names,",")


# ----------------------------------------------------------------------
if __name__ == "__main__":
    build = BuildTree()
    #print build.make("paragraph",text="fred")
    #print build.paragraph(text="fred")

    print "Building a section"
    build.start("section")
    build.add("title","Fred")
    build.start("paragraph")
    build.add("text","This is some text.")
    build.add("strong","Really.")
    build.start("paragraph","Another paragraph")
    build.end("section")
    print build.finish()

    #print "Building a broken section"
    #build.start("section")
    #build.add("title","Fred")
    #build.start("paragraph")
    #print build.finish()
