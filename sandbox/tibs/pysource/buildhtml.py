"""Convenience mechanisms for writing HTML

Similar in concept to buildtree.py (qv), but very different in the details.
"""

import sys
import string

__docformat__ = "reST"

EMPTY = ["br","hr"]
"""Elements that do not (may not) have content.
"""

INLINE = ["em","strong","samp","code","tt","text"]
"""Elements that may occur 'inline' - within a paragraph, etc.

Note that we include the 'pseudo-element' "text".
"""

NEWLINE_AFTER = ["html","head","body","table","address"]

LISTS = ["ol","ul","dl"]


# ----------------------------------------------------------------------
class BuildHTML:

    def __init__(self,stream=None):
        """Instantiate a BuildHTML instance.

        `stream` should be either something that "looks like" a file
        instance (specifically, it has to have a "write" method), or
        else `None` if we want to default to sys.stdout
        """

        self.stream = stream or sys.stdout
        self.stack = []
        """A stack of tag names (e.g., ["html","body","h1","p"])
        """
        self.last = None
        """The last element we were told to add to our output.
        """
        self.fancy = 0

    def write_doctype(self):
        """Write out the DOCTYPE element at the start of the HTML file.

        For the moment, we don't provide any flexibility in this...
        """
        self.stream.write('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01'
                          ' Transitional//EN"'
                          ' "http://www.w3.org/TR/html4/loose.dtd">\n')

    def finish(self):
        """Call this to indicate we have finished.

        It will grumble if anything is left unclosed - i.e., if there
        is still stuff on the internal stack.
        """
        if len(self.stack) > 0:
            raise ValueError,"Items still outstanding on stack: %s"%\
                  self._stack_as_string()

    def _maybe_write_newline(self,tag,before=1):
        """Decide whether to write a newline before or after an element.
        """
        if before:
            if tag not in INLINE:
                self.stream.write("\n")
        else:
            if tag in NEWLINE_AFTER:
                self.stream.write("\n")

    def add(self,tag,*args,**keywords):
        """Write an HTML element at the current level.

        For instance::

            build.add("em","Some simple text.")

        If `tag` is "text" then it will automagically be converted
        to ordinary inline text (even though there is no such HTML
        element).

        Otherwise, this produces (for instance)::

            <em>Some simple text.</em>

        See `write` (which this uses) for more details of the arguments.
        """
        self._maybe_write_newline(tag)
        self.stream.write(self.element(tag,*args,**keywords))
        self.last = "/"+tag

    def start(self,tag,*args,**keywords):
        """Write out the start of an HTML element, and start a new level.

        `tag` should be the name of an HTML element (a tag), or "comment".

        For instance::

            build.start("li","some text")

        might cause::

            <li>some text

        to be written out. Note that the element's closing tag is *not*
        written out - see `end()` for that.

        If `args` are given, they are assumed to be (things that resolve
        to) more text elements (i.e., strings). For instance::

            build.start("li","some text",
                        build.element("strong","and emphasis"),
                        "and plain text again")

        See `write` (which this uses) for more details of the use of the
        `tag` and `keywords` arguments.
        """
        if tag in EMPTY:
            raise ValueError,"Cannot start an 'empty' element (%s)"%tag
        elif tag == "text":
            raise ValueError,"Cannot start 'text'"
        elif tag == "html" and len(self.stack) > 0:
            raise ValueError,\
                  "Cannot insert 'html' except at root of stack"

        self._maybe_write_newline(tag,before=1)
        self.stream.write(self.start_tag(tag,**keywords))
        content = self._content(tag,args)
        if content:
            self.stream.write(content)
        self._stack_add(tag)
        self.last = tag

    def end(self,tag,*args):
        """Write out the end of an HTML element, and finish the current level.

        `tag` should be the name of an HTML element (a tag), or "comment".

        For instance::

            build.end("ul")

        Otherwise, for the moment at least, the `tag` being ended
        must be the last tag that was begun (in the future, we *might*
        support automatic "unrolling" of the stack, but not at the
        moment).

        NB: if `args` are given, they will also be treated as closing
        tags, in order - thus, for example::

            build.end("td","tr","table")

        is exactly equivalent to::

            build.end("td")
            build.end("tr")
            build.end("table")

        (Hmm - I'm not sure if that last is a good idea. Still, I *do*
        use it for that specific instance, which is a relatively common
        thing to want to do, and it does save "wasting" two fairly
        uninteresting lines of code.)
        """
        if tag in EMPTY:
            raise ValueError,"Cannot start an 'empty' element (%s)"%tag

        if tag == "text":
            raise ValueError,"Cannot end 'text'"

        self._stack_remove(tag)
        self.stream.write(self.end_tag(tag))
        self._maybe_write_newline(tag,before=0)

        if args:
            for item in args:
                self.end(item)
            self.last = "/"+args[-1]
        else:
            self.last = "/"+tag

    def start_tag(self,tag,**keywords):
        """Construct and return a start tag.

        `tag` should be the name of an HTML element (a tag)

        `tag` may not be "text".

        `keywords` should be attributes for the tag.
        """
        if tag == "comment":
            return "<!-- "

        str = "<%s"%tag
        if keywords:
            keys = keywords.keys()
            keys.sort()
            for key in keys:
                value = self.escape(keywords[key])
                str += " %s='%s'"%(key,value)
        return str + ">"

    def end_tag(self,tag):
        """Construct and return an end tag.

        `tag` should be the name of an HTML element (a tag)

        `tag` may not be "text".
        """
        if tag == "comment":
            return " -->"
        else:
            return "</%s>"%tag

    def element(self,tag,*args,**keywords):
        """Construct and return a complete HTML element.

        `tag` should be the name of an HTML element (a tag), or "text".

        If `tag` is "text" then `keywords` is ignored, and the result
        of concatenating `args` is returned.

        Otherwise:

          - an opening tag is composed from `tag` and `keywords`
           (see `start_tag()`)
          - the result of concatentating `args` is appended to that
          - a closing tag (see `end_tag()`) is appended to that

        and the result is returned.

        Within `args`, non-strings are coerced to their representations.
        """
        content = self._content(tag,args)
        if tag == "text":
            return content
        else:
            return self.start_tag(tag,**keywords) + content + \
                   self.end_tag(tag)
        

    def _content(self,tag,args):
        """Return the *content* of an element.

        `tag` is not currently used, but *might* be useful later on?
        """
        content = ""
        if args:
            for item in args:
                if type(item) == type(""):
                    content += item
                else:
                    content += `item`
        return content

    def escape(self,text):
        """Return `text` as valid HTML

        (that is, with any "special" characters escaped)
        """
        # Hmm - paranoia, just in case
        if type(text) != type(""): return text

        text = string.replace(text, "&", "&amp;")
        text = string.replace(text, "<", "&lt;")
        text = string.replace(text, '"', "&quot;")
        text = string.replace(text, ">", "&gt;")

        if self.fancy:
            text = string.replace(text, " ", "&deg;")
            text = string.replace(text, "\n", "&para;\n")
        return text

    def last_tag(self):
        """Return the last element we were asked to add to our output.

        Note that if we just closed element "XX" (for instance), then
        we will return "/XX".
        """
        return self.last

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Stack queries

    def _in_list(self):
        """Are we *immediately* within a list

        (i.e., the first child element of a list)
        """
        return self.stack[-1] in LISTS


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Stack maintenance

    def _stack_ends(self,name):
        """Return true if the stack ends with the named entity.
        """
        return self.stack[-1] == name

    def _stack_add(self,name):
        """Add a new level to the stack.
        """
        self.stack.append(name)

    def _stack_remove(self,name):
        """Remove the last level from the stack

        (but only if it is of the right sort).
        """
        if len(self.stack) == 0:
            raise ValueError,"Cannot end %s - nothing outstanding to end"%\
                  (name)
        if name != self.stack[-1]:
            raise ValueError,"Cannot end %s - last thing begun was %s"%\
                  (name,self.stack[-1])
        del self.stack[-1]

    def _stack_as_string(self):
        names = []
        for name in self.stack:
            names.append(name)
        return string.join(names,",")


# ----------------------------------------------------------------------
if __name__ == "__main__":
    build = BuildHTML()

    print "Building a page"
    build.start("html")
    build.start("body")
    build.add("h1","Fred")
    build.start("p")
    build.add("text","This is some text.")
    build.add("strong","Really.")
    build.start("p","Another paragraph")
    build.end("body")
    build.end("html")
    build.finish()

    print
    print "Building a broken page"
    try:
        build.start("html")
        build.start("body")
        build.add("h1","Fred")
        build.start("p")
        build.finish()
    except ValueError,detail:
        print "ValueError:",detail
