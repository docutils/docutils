#! /usr/bin/env python
"""Extract information from a Python file (module) for the DOCUTILS

An example of using Jeremy Hylton's compiler module.

Originally based on an example *by* Jeremy Hylton. In the introduction
to that example, he said:

    Here's a trivial example program that extracts some information
    about methods and attributes from a class and its methods.
    I wasn't exhaustive here.  I'll get attributes assigned to by
    `self.x` in a method body and I'll get method definitions in
    the class body.  I don't deal with obvious things like attributes
    defined at the class level.

I'm expanding it, initially as a bigger example, and also so I get to
understand how things work!

Note: there should be an example Python file called test.py in the
same directory as this module - it's meant to be a suitable input
for testing/learning purposes.

:Author: Tibs <tibs@tibsnjoan.co.uk>
:Version: Whatever. Oh, 0.4, then
:Date: 2002-January-18

Aims for DOCUTILS
-----------------
1. Locate all modules, classes, functions, methods and their docstrings.
2. "Filter" these to leave the ones that DOCUTILS is interested in.
3. Annotate functions and methods with their arguments.
4. Locate all assignments which have docstrings.
5. Similarly "filter" these.
6. Locate all uses of `global` assignments.
7. Filter out those which are not obvious from the module level,
   so that the user can be warned about them.
8. Produce a tree structure suitable for DOCUTILS processing, containing
   the suitable results of the above.

(At least some of) 1, 3 and 4 have already been done.

Still to do
-----------
* *all* sorts of assignment should be visited, so that
  we can tie down the use of globals properly.
* the option of ignoring _name and __name things should be
  available (should this be the default?)
* Some consideration should be given to freeing up this mess of things
  when they are all finished with - there are so many interconnections
  that automatic deletion is unlikely to work well
* other things as they occur to me
* Finish off InstanceValue (its potential link to the class that defines it)

Names and fullnames
-------------------
A "name" is taken to be the name as presented in Python code. Note that
for class instance attribute references (e.g., "self.fred"), the name is
includes the class reference, dot and attribute name (e.g., "self.fred").

A "fullname" includes the full path to the name as well - that is, 
including all of the elements required to "reach" this item. Thus, for
instance, a method Fred in class Jim in module Bill would be::

    Bill.Jim.Fred

whilst a name A defined within Fred would be::

    Bill.Jim.Fred:A

Note that if Fred contains a reference to self.B, then that would (for the
moment at least) be written as:: 

    Bill.Jim.Fred:self.B

Within an eventual HTML document, such fullnames become anchor and link
names for nodes. For that purpose, they will be transformed so that they
do not contain "." or ":", but are still not accidentally confusable with
legitimate Python names. Thus the last example would be transformed to::

    Bill-Jim-Fred--self-B

Note that we also have a "special" label, artificially "generated" - that
is, there is a single label for all arguments to a function or method,
which is formed of the label for the function or method, followed by
*three* hyphens, and then the text "args" - so, for example:

    Bill-Jim---args

Special cases
-------------
Notes on special cases in name resolution:

- obviously I should be taking account of explicit roles.

- if it can't find the name directly via the "scope", it should be looking
  up in the module - so the ScopeMixin needs to provide support for that...

- the module name should be recognised as, in some sense, within the
  module's "parent" - even if there is no such beast.

- that generalises - in general, an object's own name should be visible
  to its docstring, I think - so, for instance::

      digit = 3
      '''We store the current digit of pi in `digit`'''

  (yes, I know that's poor documentation, but even so)

- <self>.name in a method (where <self> is the first argument to said
  method) should be regarded as being a name in the class containing
  the method, in some sense (but what sense?)
""" #"

import os
import sys
import string
import compiler

from docutils.parsers.rst import Parser

# Local modules
import utils
import visit

__docformat__ = "reST"


# ----------------------------------------------------------------------
def make_Docstring(text, settings):
    """A factory function to handle non-existant docstrings.
    """
    if text:
        return Docstring(text, settings)
    else:
        return None

class Docstring:
    """We use this to represent a docstring, parsed and unparsed.
    """

    def __init__(self, text, settings, debug=0):
        """Instantiate a new Docstring from its text.
        """
        self.text = self._trim(text)
        """The text of the docstring.
        """
        self.document = None
        """The result of parsing the docstring, if requested.
        (thus, only non-None if we *are* parsing docstrings in
        the containing module).
        """
        self.settings = settings
        """The docutils settings to apply to new document parsers.
        """

        self.debug = debug

    def _trim(self,docstring):
        """Trim leading space appropriately from the docstring text.

        When the docstring is read from the source file, it is likely
        to have all but the first line indented with respect to the
        first - for example, consider this docstring.

        Before it is *used* for anything (e.g., typing it out to be
        read by the user), it is necessary to adjust for this. The
        way to do this is well-defined (???where???).

        Returns the adjusted text.
        """

        # Remove any confusing tabs - follow the normal Python rules
        # - and split into convenient lines
        lines = string.split(string.expandtabs(docstring),"\n")

        # Find significant indentation
        firstline = lines[0]
        if firstline and firstline[0] == " ":
            oldlen = len(firstline)
            firstline = string.lstrip(firstline)
            indent = oldlen - len(firstline)
        else:
            indent = 0
            for line in lines[1:]:
                oldlen = len(line)
                lin = string.lstrip(line)
                if lin:
                    indent = oldlen - len(lin)
                    break

        # And remove it...
        newlines = [firstline]
        for line in lines[1:]:
            oldlen = len(line)
            lin = string.lstrip(line)
            if lin:
                extra = oldlen - len(lin) - indent
                if extra > 0:
                    lin = " "*extra + lin
            newlines.append(lin)

        # Don't introduce a trailing blank line if they did some
        # variant of the following layout::
        #         """Some text
        #         """
        if newlines and newlines[-1] == "":
            del newlines[-1]

        # And lastly, reassemble it back into text again
        return string.join(newlines,"\n")

    def show(self,stream,spaces=""):
        # Not optimised for efficiency...
        lines = string.split(self.text,"\n")

        # For the moment, don't try to format it very nicely at all...
        stream.write('%s"""%s\n'%(spaces,lines[0]))
        for line in lines[1:]:
            stream.write(spaces+line+"\n")
        stream.write('%s"""\n'%spaces)

    def parse(self,parser):
        """Parse our text with the given parser.
        """
        import docutils
        self.document = docutils.utils.new_document(self.text, self.settings)
        parser.parse(self.text,self.document)

    def add_to_DPS(self,dps):
        """Add this docstring content to a DOCUTILS subtree.

        Calls the appropriate things from `dps`, which is a
        builtree.BuildTree instance.
        """
        if self.document:
            for child in self.document.children:
                dps.addsubtree(child)
        else:
            dps.start("literal_block")
            dps.add("text",self.text)
            dps.end("literal_block")

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Experimental <interpreted> node code...
    # Copied from transform.py, and not adjusted yet (except superficially
    # and stupidly) for its position in this class.
    # - despite the comments in the docstring, it doesn't *do* anything yet...

    def transform_interpreted(self,thing):
        """Deal with <interpreted> nodes in this docstring.

        The intent is to find any <interpreted> nodes, try to locate the
        relevant Python value, and insert an appropriate cross-reference.

        `thing` is the `Module`, `Class`, `Function`, `Method` or
        `Name` instance to which this docstring is "attached". It
        is used to resolve the <interpreted> names.
        """
        if self.debug:
            print "\n*** Locating <interpreted> items for %s"%thing.fullname
            self.show(sys.stdout,"    ")
        self.locate_interpreted(self.document,thing)

    def locate_interpreted(self,element,thing):
        """Deal with finding "interpreted" nodes in a document subtree.
        """
        for node in element:
            if node.tagname == "#text":
                continue
            elif node.tagname == "interpreted":
                self.treat_interpreted(node,thing)
            else:
                self.locate_interpreted(node,thing)

    def treat_interpreted(self,element,thing):
        """Do something interesting with an "interpreted" node.
        """
        if element.has_key("role"):
            role = element["role"]
        else:
            role = None

        name = element.astext()

        #if role:
        #    print "...<interpreted> :%s: %s"%(role,name)
        #else:
        #    print "...<interpreted> %s"%name

        # Attempt to work out what this <interpreted> item refers to
        target = thing.scope_find_name(name,role=role,indent="...")
        if target:
            element["refname"] = target


# ----------------------------------------------------------------------
class ScopeMixin:
    """Learning about how to represent scope.

    The idea of this mixin class is to work out the best way of representing
    scope, so that I can use the results when working out exactly what an
    <interpreted> item in a docstring refers to.

    Note that I do not intend (at least, for the moment) to worry about Python
    before 2.2 - i.e., before embedded scope became non-optional. This is
    purely laziness on my part...
    """

    scope_debug = 0

    def scope_init(self):
        """Call at the end of `__init__` to initilise our experiments
        """

        self.scope_module = None
        """Which Module we are in.
        """

        self.scope_parent = None
        """The *immediate* parent entity.
        Note that for a method, this will be the class defining it.

        (Ultimately, it might be better if this were a weak reference.)
        """

        self.scope_encloser = None
        """The innermost enclosing block. Note that for a method,
        this should *not* be the class, but the innermost enclosing
        function/method/module/whatever (whatever?).
        """

        self.scope_defines = []
        """A list of the names *defined* by this entity
        (by assignment, definition or whatever).

        The list is ordered by last occurrence of the name.
        """

        self.scope_fullname = {}
        """The key is the name from `self.scope_defines`, the value is the
        "full" name for the defined value.
        """

        self.scope_children = {}
        """A dictionary of the entities defined by this entity.

        The keys are the entity name (so correspond to those in
        `self.scope_defines`), and the values are NULL if the entity
        is defined by simple assignment, otherwise the relevant
        "scope" instance (e.g., a Method or Class).
        """

        self.scope_globals = []
        """A list of names which are declared global in this scope.
        """

    def scope_define_parent(self,parent=None,module=None):
        """Define our parent and encloser.
        """
        self.scope_module = module
        self.scope_parent = parent
        self.scope_encloser = parent
        if isinstance(self,Method):
            while isinstance(self.scope_encloser,Class):
                self.scope_encloser = self.scope_encloser.scope_parent

    def scope_define_name(self,name,fullname,child=None):
        """Add a name that is defined in this scope.

        - `name` is the obvious name of the name being defined.
        - `fullname` is the "path" to it, starting at the module.
        - `child` is the class representing that name - e.g., a Class
           or Name instance.

        Note - remember that functions/methods "define" their arguments...

        HOWEVER we are not trying for a general solution to the problem of
        "find the item that is being referred to" - we are just trying to
        provide useful links between <interpreted> items in docstrings and
        those things that they might reasonably be expected to be referring to.

        Furthermore, we know that we will not show attributes, docstring or
        not, if they are not

        a. At module level (so a ModuleValue)
        b. Within a class (so a ClassValue)
        c. Within a method called __init__ or __new__.
        d. An argument (we always want to know about arguments).

        So we discard any attributes that do not match these criteria...
        """
        if self.scope_debug:
            if name in self.scope_defines:
                redef = "(re)"
            else:
                redef = "    "
            print "%-15s %-30s %sdefined in %s"%(child.__class__.__name__,
                                                 name,redef,self.name)

        # Should we ignore this name?
        if isinstance(child,Name):
            if self.__class__ not in [Package,Module,Class] and \
               not (self.__class__ == Method and
                    self.name in ["__init__","__new__"]) and \
               child.__class__ != Argument:
                return
        # Remember to ensure the name is always at the END of the list
        # (even if it was already there)
        if name in self.scope_defines:
            self.scope_defines.remove(name)
        self.scope_defines.append(name)
        self.scope_fullname[name] = fullname
        self.scope_children[name] = child

    def scope_define_global(self,name):
        """Indicate that a name is actually global.

        Note that it is entirely possible that the name is also present
        in `self.scope_defines` - being global should be remembered as taking
        precedence.
        """
        if self.scope_debug:
            if name in self.scope_defines:
                redef = "(re)"
            else:
                redef = "    "
            print "%-10s %-30s %sdefined in %s"%("[Global]",
                                                 name,redef,self.name)
        if name not in self.scope_globals:
            self.scope_globals.append(name)

    def scope_get_object(self,name):
        """Return the defined object corresponding to `name`, or None.
        """
        try:
            return self.scope_children[name]
        except:
            return None

    def scope_get_type(self,name):
        """Return the "type" of the defined object corresponding to `name`.
        """
        try:
            return self.scope_children[name].__class__.__name__
        except:
            return None

    def scope_name_in_self(self,name):
        """Return true if the `name` is defined by ourselves.
        """
        return name in self.scope_defines

    def scope_name_in_encloser(self,name):
        """Return true if the `name` is defined by our encloser.

        (If we don't *have* an encloser - e.g., we're a module - then
        it obviously returns false.)
        """
        if self.scope_encloser:
            return name in self.scope_encloser.scope_defines
        else:
            return 0

    def scope_name_in_module(self,name):
        """Return true if the `name` is defined by our module.
        """
        if self.scope_module:
            return name in self.scope_module.scope_defines
        else:
            return 0

    def scope_up_is_module(self):
        """Is our encloser the module?
        """
        return self.scope_module == self.scope_encloser

    # We need a dictionary to relate role names to the classes we use
    # A value of None against a key means that we are not attempting
    # to handle this yet.
    roles = {"package"           : "Package",
             "module"            : "Module",
             "class"             : "Class",
             "method"            : "Method",
             "function"          : "Function",
             "module_attribute"  : "ModuleValue",
             "class_attribute"   : "ClassValue",
             "instance_attribute": "InstanceValue",
             "variable"          : "Name",
             "parameter"         : "Argument",
             "type"              : None,
             "exception_class"   : None,
             "exception"         : None,
             "warning_class"     : None,
             "warning"           : None}

    def scope_obj_role_match(self,what,role):
        """Decide if the role (if any) makes sense with respect to `what`.

        `what` is the object we thing might be our item of interest,
        and `role` is the role we're looking for, or None if we're not
        particular.
        """
        if role:
            if what.__class__.__name__ == self.roles[role]:
                return 1
            else:
                return 0
        else:
            return 1

    def scope_class_role_match(self,class_name,role):
        """Decide if the role (if any) makes sense with respect to `what`.

        `what` is the class name of the object we thing might be our item
        of interest, and `role` is the role we're looking for, or None if
        we're not particular.
        """
        if role:
            if class_name == self.roles[role]:
                return 1
            else:
                return 0
        else:
            return 1

    def scope_callable_match(self,what,need_callable):
        """Does the callable state of the `what` match our needs? Do we care?
        """
        if need_callable:
            return what.is_callable()
        else:
            return 1

    def scope_find_name(self,name,role=None,indent="       "):
        """A first pass at looking up names in scope.

        - `name` is the text that the user has enclosed in backquotes,
          to make it interpreted
        - `role`, if given, specifies exactly what sort of name the
          user determines it to be.

        This makes no attempt to do anything beyond looking up a
        simple name - it doesn't try to deal with (for instance)
        a "()" at the end of the name, nor does it try to cope
        with dotted names in any clever way.

        Returns either the label for the referenced entity (which is
        a translation of its `fullname` into something legal as an
        XML name), or None.
        """
        if name.endswith("()"):
            name = name[:-2]
            need_callable = 1
        else:
            need_callable = 0

        if self.scope_debug:
            print indent,
            if role:
                print ":%s:"%role,

        # Is it a reference to ourself?
        if name == self.name and \
           self.scope_obj_role_match(self,role) and \
           self.scope_callable_match(self,need_callable):
            if self.scope_debug:
                print "%s '%s' IS %s %s"%\
                      (self.__class__.__name__,name,
                       self.__class__.__name__,self.name)
            return self.label()

        # Is it a reference to one of our children?
        if self.scope_name_in_self(name):
            what = self.scope_get_type(name)
            whom = self.scope_children[name]
            if self.scope_class_role_match(what,role) and \
               self.scope_callable_match(whom,need_callable):
                if self.scope_debug:
                    print "%s '%s' found HERE in %s %s"%\
                          (what,name,self.__class__.__name__,self.name)
                if whom.__class__ == Argument:
                    return self.args_label()
                else:
                    return whom.label()

        # Is it a reference to one of our encloser's children?
        if self.scope_name_in_encloser(name):
            what = self.scope_encloser.scope_get_type(name)
            whom = self.scope_encloser.scope_children[name]
            if self.scope_class_role_match(what,role) and \
               self.scope_callable_match(whom,need_callable):
                if self.scope_debug:
                    print "%s '%s' found UP in %s %s"%\
                          (what,name,self.scope_encloser.__class__.__name__,
                           self.scope_encloser.name)
                if whom.__class__ == Argument:
                    return self.scope_encloser.args_label()
                else:
                    return whom.label()

        # Is it a reference to our encloser?
        if self.scope_encloser and name == self.scope_encloser.name:
            # I'm not sure about this one - *should* we be able to "see"
            # the name of our encloser? Or is what I'm *really* after
            # the name of the *parent*?
            #
            # Specific instance - consider a method M in class C
            # - should `C` in `M`s docstring refer to class C,
            # even if class C is *not* defined at module level?
            if self.scope_obj_role_match(self.scope_encloser,role) and \
               self.scope_callable_match(self.scope_encloser,need_callable):
                if self.scope_debug:
                    print "%s '%s' IS %s %s"%\
                          (self.scope_encloser.__class__.__name__,name,
                           self.scope_encloser.__class__.__name__,
                           self.scope_encloser.name)
                return self.scope_encloser.label()

        # Is it a reference to a child of our module?
        if not self.scope_up_is_module() and self.scope_name_in_module(name):
            what = self.scope_module.scope_get_type(name)
            whom = self.scope_module.scope_children[name]
            if self.scope_class_role_match(what,role) and \
               self.scope_callable_match(whom,need_callable):
                if self.scope_debug:
                    print "%s '%s' found GLOBAL in %s %s"%\
                          (what,name,self.scope_module.__class__.__name__,
                           self.scope_module.name)
                # Modules don't have arguments...
                return whom.label()

        # Is it a reference to our module itself?
        if name == self.scope_module.name:
            if self.scope_obj_role_match(self.scope_module,role) and \
               not need_callable:
                if self.scope_debug:
                    print "%s '%s' IS %s %s"%\
                          (self.scope_module.__class__.__name__,name,
                           self.scope_module.__class__.__name__,
                           self.scope_module.name)
            return self.scope_module.label()

        if self.scope_debug:
            print "'%s' not in %s %s"%(name,self.__class__.__name__,self.name),
            if self.scope_encloser:
                print "or %s %s"%(self.scope_encloser.__class__.__name__,
                                  self.scope_encloser.name),
            if  self.scope_up_is_module():
                print
            else:
                print "or %s %s"%(self.scope_module.__class__.__name__,
                                  self.scope_module.name)
        return None

    def scope_show(self,indent=""):
        print "%s%s %s"%(indent,self.__class__.__name__,self.name),
        if self.scope_encloser and self.scope_encloser != self.scope_parent:
            print "<<upscope %s %s>>"%(self.scope_encloser.__class__.__name__,
                                       self.scope_encloser.name)
        else:
            print

        extra_indent = indent+"  "

        if self.scope_globals:
            print "%sglobals %s"%(extra_indent,self.scope_globals)

        for name in self.scope_defines:
            child = self.scope_children[name]
            if child:
                child.scope_show(extra_indent)


# ----------------------------------------------------------------------
class Base(ScopeMixin):
    """A simple base class for common functionality.
    """

    verbose = 0

    def __init__(self, settings, name, fullname, docstring=None):
        self.settings = settings
        """ The parser settings
        """

        self.name = name
        """The name of this scope entity.
        """

        self.fullname = fullname
        """The fully qualified name of this entity
        - for instance, for a method `fred` in a class `Jim`
        in a module `bob`, this would be "bob.Jim.fred"
        """

        self.docstring = make_Docstring(docstring, settings)
        """The docstring for this scope, or None.
        Note that, at least for the moment, we're only supporting
        one docstring for an entity. This may need to change for
        proper DOCUTILS support (but I [1]_, personally, hope not).

        .. [1] "I" being Tibs, at this point, and at this time [2]_.
        .. [2] "This time" being April 2002.
        """

        self.imports = {}
        """Import statements within this scope. The key is the
        name of each module being imported, the value is a list
        of the names "as" which the module is imported, with
        `None` meaning "as itself".
        """

        self.from_imports = {}
        """From <module> import <names> statements within this
        scope. The key is the <module>, the value a dictionary
        like that used for `self.imports`.
        """

        self.classes = {}
        """Classes defined within this scope. The key is the
        class name, the value is the Class instance for this ``class``.
        """

        self.functions = {}
        """Functions or methods defined within this scope. The key is the
        function name, the value is the Function or Method instance
        for this ``def``.
        """

        self.attributes = {}
        """Attributes assigned to within a scope. The key is the
        attribute name, the value is the Name instance for this ``name``.
        """

        self.children = []
        """A list of all of the items contained herein which might
        have, or have children which have, docstrings - i.e., packages,
        modules, classes, functions, methods.
        """

        self.class_list = []
        """A list of the class names defined in this scope, in order.
        """

        self.function_list = []
        """A list of the function/method names defined in this scope, in order.
        """

        self.attribute_list = []
        """A list of the attribute names defined in this scope, in order.
        """

        self.globals = {}
        """The names mentioned in global statements within a scope.
        The key is the name, the value doesn't matter.
        """

        self.parser = None
        """We may need a reStructuredText parser to handle our docstrings
        - if so, we'll generate one "on demand"."""

    def getParser(self):
        """Retrieve a reStructuredText parser, for docstring usage.
        """
        if not self.parser:
            self.parser = Parser()
        return self.parser

    def parse_docstrings(self,parser):
        """If we are meant to, parse our docstrings.
        """
        if self.docstring:
            self.docstring.parse(parser)
        for child in self.children:
            child.parse_docstrings(parser)

    def is_callable(self):
        """Does it make sense to call an object of this type?
        """
        return 0

    def label(self):
        """Return an XML-legal name for this object.

        Note that we implicitly assume that all objects have their
        output XML/HTML representation written to the same file
        - i.e., that we do not have to worry about references that
        are anything beyond simple names.
        """
        str = string.replace(self.fullname,".","-")
        str = string.replace(str,":","--")
        return str

    def addGlobal(self,name):
        self.globals[name] = name

    def addClass(self,klass):
        self.classes[klass.name] = klass
        self.class_list.append(klass.name)
        self.children.append(klass)

    def addFunction(self,func):
        self.functions[func.name] = func
        self.function_list.append(func.name)
        self.children.append(func)

    def addAttribute(self,name,instance):
        self.attributes[name] = instance
        self.attribute_list.append(name)
        self.children.append(instance)

    def addImport(self,names):
        """Remember an import statement.

        `names` is a list of ``(name,as)`` tuples, where ``name``
        is the name of the item being imported, and ``as`` is either
        None of else the name "as" which this item is being imported

        Note we aren't even *trying* to remember its exact locality.
        """
        for name,as in names:
            if not self.imports.has_key(name):
                self.imports[name] = [as]
            else:
                if as not in self.imports[name]:
                    self.imports[name].append(as)

    def addFromImport(self,modname,names):
        """Remember a from import statement.

        `modname` is the name of the module "from" which things are
        being imported.

        `names` is a list of ``(name,as)`` tuples, where ``name``
        is the name of the item being imported, and ``as`` is either
        None of else the name "as" which this item is being imported

        Note we aren't even *trying* to remember its exact locality.
        """
        if not self.from_imports.has_key(modname):
            self.from_imports[modname] = {}

        dict = self.from_imports[modname]
        for name,as in names:
            if not dict.has_key(name):
                dict[name] = [as]
            else:
                if as not in dict[name]:
                    dict[name].append(as)

    def isGlobal(self,name):
        return self.globals.has_key(name)

    def getClassNames(self):
        return self.class_list
        #return self.classes.keys()

    def getClass(self,name):
        return self.classes[name]

    def getFunctionNames(self):
        return self.function_list
        #return self.functions.keys()

    def getFunction(self,name):
        return self.functions[name]

    def getAttributeNames(self):
        return self.attribute_list
        #return self.attributes.keys()

    def getAttribute(self,name):
        return self.attributes[name]

    def getAttributeDocstring(self,name):
        """Return an attribute docstring, or None
        """
        return self.attributes[name]

    def getImports(self):
        """Return our "import"s, but not in any particular order.
        """
        return self.imports

    def getFromImports(self):
        """Return our "from ... import"s, but not in any particular order.
        """
        return self.from_imports

    def getFromImportNames(self,name):
        return self.from_imports[name]

    def getSelf(self):
        """Return the name used for "self". Doesn't do much elsewhere.

        (Actually, for a method *or* a function, it returns the name
        of the first argument.)
        """
        return None

    def _show_body(self,stream,indent=0):
        spaces = indent*" "

        if self.docstring:
            self.docstring.show(stream,spaces+"  ")

        if self.imports:
            names = self.imports.keys()
            stream.write("%s  Import %s\n"%(spaces,string.join(names,", ")))

        if self.from_imports:
            modnames = self.from_imports.keys()
            for mod in modnames:
                names = self.from_imports[mod]
                stream.write("%s  From %s import %s\n"%\
                             (spaces,mod,string.join(names,", ")))
        
        # Should we sort things alphabetically within each list?

        if self.verbose:
            globals = self.globals.keys()
            if globals:
                stream.write("%s  Global statements\n"%spaces)
                for name in globals:
                    stream.write("%s    %s\n"%(spaces,name))

        attributes = self.attributes.keys()
        if attributes:
            stream.write("%s  Attributes\n"%spaces)
            for name in attributes:
                self.attributes[name].show(stream,indent+4)

        class_names = self.classes.keys()
        if class_names:
            for name in class_names:
                self.classes[name].show(stream,indent+2)

        func_names = self.functions.keys()
        if func_names:
            for name in func_names:
                self.functions[name].show(stream,indent+2)

    def show(self,stream,indent=0):
        """Override as necessary."""
        stream.write("%s%s %s\n"%(" "*indent,self.__class__.__name__,
                                  self.name))
        self._show_body(stream,indent)

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Experimental <interpreted> node code...
    def find_docstrings(self):
        """Find each docstring in our subtree.
        """
        if self.docstring:
            self.docstring.transform_interpreted(self)
        for child in self.children:
            child.find_docstrings()
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# ----------------------------------------------------------------------
class Package(Base):
    """The representation of a Python package.

    Unlike the other children of `Base`, this class actually visits
    its members itself - that is, it is self-constructing.
    """

    def __init__(self,settings,directory,parent=None,debug=0):
        """Instantiate a new package.

        If this is a "sub" package, then `parent` should be a Package
        instance, within which we may be found...

        `settings` will give the parsing settings -- `verbose_parse` will
        cause the generation of a message for each stage of the production
        of our data, and `debug` will give a message for each "entity"
        in the AST that we visit.
        """

        self.settings = settings
        self.verbose = settings.verbose_parse
        self.debug   = debug

        path = os.path.expanduser(directory) # expand "~"
        path = os.path.expandvars(directory) # expand "$fred" or "%fred%"
        path = os.path.abspath(path)         # expand to a 'full' path
        path = os.path.normpath(path)        # sort out "/fred" versus "/fred/"
        self.directory = path
        """The `directory` is our best guess as to the actual
        path to the package."""

        # Take the package name to be the final component
        base,name = os.path.split(path)
        if parent:
            fullname = parent.fullname + "." + name
            Base.__init__(self,settings,name,fullname)
        else:
            # With no parent, our name and fullname are the same.
            Base.__init__(self,settings,name,name)

        self.modules = {}
        """Remember the modules (or Python files, really)
        in this package. The key is the module name, the
        value the Module instance for this module.
        """

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Experimental scope setup
        self.scope_init()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        self.parse_modules()

    def parse_modules(self):
        """Locate our constituent modules and parse them.
        """
        files = os.listdir(self.directory)

        if "__init__.py" not in files:
            raise ValueError,\
                  "Directory %s does not contain an __init__.py file"%\
                  self.directory

        # Parsing the __init__.py file may give us further information,
        # such as an __all__ value, which we may later want to use to
        # determine which files to read and parse, and which to ignore
        path = os.path.join(self.directory,"__init__.py")
        self.addModule(Module(self.settings,path,package=self, debug=self.debug))

        # But, for the moment, just parse all the Python files...
        for file in files:
            name,ext = os.path.splitext(file)
            if name == "__init__":
                continue
            elif ext in [".py",".pyw"]:
                path = os.path.join(self.directory,file)
                self.addModule(Module(self.settings,path,package=self,    
                    debug=self.debug))

    def addModule(self,mod):
        self.modules[mod.name] = mod
        self.children.append(mod)

    def getModuleNames(self):
        return self.modules.keys()

    def getModule(self,name):
        return self.modules[name]

    def getModules(self):
        """Return a list of Module instances.
        """
        return self.modules.values()

    def show(self,stream,indent=0):
        stream.write("%sPackage %s"%(" "*indent,self.name))
        if self.directory:
            stream.write("(in %s)\n"%self.directory)
        else:
            stream.write("\n")

        names = self.modules.keys()
        names.sort()
        for name in names:
            self.modules[name].show(stream)

    def show_ast(self,stream):
        """Print out a representation of our ASTs.
        """
        names = self.modules.keys()
        names.sort()
        for name in names:
            self.modules[name].show_ast(stream)
       

# ----------------------------------------------------------------------
class Module(Base):
    """The representation of a Python module.

    This class also knows how to *parse* a Python module.

    Still known to be missing:

    - visitLambda (I shudder)
    - should we also do visitExec?

    - visitAssign
    - visitAugAssign

    These last two are needed to allow us to link a Name with its value(s).

    If attribute docstrings are to be found, then the AST on which we
    act must first have been processed with `find_attr_docs()`.

    Note we also need to visit the *other* sorts of assignment, so
    we can update our Module with any globals used therein...
    """

    def __init__(self,settings,filename,docstring=None,package=None,debug=0):
        """Instantiate a new module.

        If this is a module within a package, then `package` should be
        a Package instance.

        `settings` will give the parsing settings -- `verbose_parse` will
        cause the generation of a message for each stage of the production
        of our data, and `debug` will give a message for each "entity"
        in the AST that we visit.
        """
        self.settings = settings
        self.package = package
        self.verbose = settings.verbose_parse
        self.debug   = debug

        self.docformat,self.language = utils.docformat("plaintext")

        filename = os.path.expanduser(filename) # expand "~"
        filename = os.path.expandvars(filename) # expand "$fred" or "%fred%"
        filename = os.path.abspath(filename)    # expand to a 'full' path
        filename = os.path.normpath(filename)   # and tidy up
        self.filename = filename
        """The `filename` is our best guess as to the actual
        path to the module."""

        dir,file = os.path.split(filename)
        name,ext = os.path.splitext(file)

        if package:
            fullname = package.fullname + "." + name
            Base.__init__(self,settings, name,fullname,docstring)
        else:
            # Without a package, our name and fullname are the same
            Base.__init__(self,settings, name,name,docstring)

        self.ast = None
        """The parse tree, as produced by compiler.
        """

        self.globals_used = {}
        """Remember the globals promulgated on this module
        (that is, occurrences of a global statement, associated
        with assignment *to* the named global, within the same
        scope).
        
        The key is the global name, the value a list of
        (scope,superscope) pairs.
        """

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Experimental scope setup
        self.scope_init()
        self.scope_define_parent(parent=package,module=self)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Now, parse ourselves!
        self.parse_module()

        # And then perform our "finishing off" operations
        self.determine_docformat()

        if self.verbose:
            print >>sys.stderr, \
                  "    >> docformat: %s, language %s"%(self.docformat,
                                                       self.language)

        if self.docformat == utils.RESTRUCTUREDTEXT:
            if self.verbose:
                print >>sys.stderr,"    .. parsing docstrings in module",\
                      self.name
            parser = self.getParser()
            self.parse_docstrings(parser)
            if self.verbose:
                print >>sys.stderr,"    .. finding <interpreted> nodes"
            self.find_docstrings()

    def parse_module(self):
        """Parse ourselves (!)
        """
        if self.verbose: print >>sys.stderr, "*** Parsing file",self.filename
        self.ast = compiler.parseFile(self.filename)

        # "Comb" the tree to sort out attribute docstrings
        if self.verbose: print >>sys.stderr, "    Locating attribute docstrings"
        utils.find_attr_docs(self.ast)

        # And to link names and their values in assignments
        if self.verbose: print >>sys.stderr, "    Locating attribute values"
        utils.find_attr_vals(self.ast)

        if self.verbose: print >>sys.stderr, "    Walking the tree"
        compiler.walk(self.ast,self)

    def determine_docformat(self):
        """Call this after reading in the module, before "using" it.

        This detects whether there is a module value called __docformat__, and
        if so, inspect its value.
        """
        if self.attributes.has_key("__docformat__"):
            name = self.attributes["__docformat__"]
            try:
                value = name.actualLastValue()
            except ValueError, detail:
                print >>sys.stderr, "Ignoring __docformat__"
                print >>sys.stderr, detail
                return
            self.docformat,self.language = utils.docformat(value)

    def addGlobalUsed(self,name,where):
        if self.globals_used.has_key(name):
            self.globals_used[name].append(where)
        else:
            self.globals_used[name] = [where]

    def getObscureGlobals(self):
        """Return a list of (name,[fullname]) tuples
        """
        globals = self.globals_used.keys()
        globals.sort()
        attributes = self.attributes.keys()
        obscure = []
        for name in globals:
            if name not in attributes:
                wherelist = self.globals_used[name]
                obscure.append( (name,wherelist) )
        return obscure

    # ----------------------------------------------------------------------
    # Extraction code

    def _scopes(self,scope,superscope):
        str = ""
        if scope:
            str = str + " in %-8s %-12s"%(scope.__class__.__name__,
                                          `scope.name`)
        if superscope:
            str = str + " in %-8s %-12s"%(superscope.__class__.__name__,
                                         `superscope.name`)
        return str

    def _report(self,what,name,scope,superscope):
        print >>sys.stderr,\
              "%-8s %-12s"%(what,name) + self._scopes(scope,superscope)

    def visitModule(self,node,scope=None,superscope=None):
        """Visit a Module node - heh, this must be us!

        We expect `scope` and `superscope` both to be None
        """
        if self.debug:
            self._report("Module",self.filename,scope,superscope)

        self.docstring = make_Docstring(node.doc, self.settings)

        # Visit our children with ourselves as their scope
        self.visit(node.node,self)

    def visitClass(self,node,scope=None,superscope=None):
        """Visit a Class node

        `scope` is the scope of this Class, and `superscope` is
        that scope's scope (if any).

        We don't yet cope well with things like::

            class Fred(docutils.nodes)

        - this will need fixing.
        """
        if self.debug:
            self._report("Class",node.name,scope,superscope)

        if scope:
            fullname = "%s.%s"%(scope.fullname,node.name)
        else:
            fullname = node.name

        # If we were nice, we'd resolve the
        #    Getattr(Getattr(Name('docutils'), 'nodes'), '_TextElement')
        # that we get for something like docutils.nodes into something
        # easier to deal with...
        cls = Class(self.settings,node.name,fullname,node.bases,node.doc)
        if scope:
            scope.addClass(cls)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Experimental scope work
        if scope:
            cls.scope_define_parent(scope,self)
            scope.scope_define_name(node.name,fullname,cls)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Visit our children with ourselves as their scope,
        # and our scope as their superscope
        self.visit(node.code,cls,scope)

    def visitFunction(self,node,scope=None,superscope=None):
        """Visit a Function node (nb: this means "function or method")

        `scope` is the scope of this Function, and `superscope` is
        that scope's scope (if any).
        """
        if self.debug:
            if isinstance(scope,Class):
                self._report("Method",node.name,scope,superscope)
            else:
                self._report("Function",node.name,scope,superscope)

        fullname = "%s.%s"%(scope.fullname,node.name)

        if isinstance(scope,Class):
            fun = Method(self.settings,node.name,fullname,
                         node.argnames,node.defaults,node.flags,node.doc)
        else:
            fun = Function(self.settings,node.name,fullname,
                           node.argnames,node.defaults,node.flags,node.doc)
        scope.addFunction(fun)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Experimental scope work
        fun.scope_define_parent(scope,self)
        scope.scope_define_name(node.name,fullname,fun)
        for argname in node.argnames:
            fullname = "%s.%s:%s"%(scope.fullname,node.name,argname)
            name = Argument(self.settings,argname,fullname)
            name.scope_define_parent(fun,self)
            fun.scope_define_name(argname,fullname,name)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Visit our children with this function as the scope, and
        # our scope as the superscope
        self.visit(node.code,fun,scope)

    def visitAssAttr(self,node,scope,superscope=None):
        """Visit an attribute assignment node.

        For example::

            self.a = 1

        Note that this is assignment to a *single* attribute only.

        `scope` is the scope of this assignment, and `superscope` is
        that scope's scope (if any).
        """
        if self.debug:
            self._report("attr",node.attrname,scope,superscope)

        # An AssAttr contains:
        #    .expr - the expression on the LH of the dot
        #    .attrname - the attribute name on the RH of the dot
        #    .flags
        # We're (to a first approximation) only interested in
        # assignments of the form <self>.<attr>, where "<self>"
        # is whatever name was passed down as the first argument
        # to a containing method.
        if isinstance(node.expr,compiler.ast.Name):
            fullname = "%s:%s.%s"%(scope.fullname,node.expr.name,node.attrname)
            selfname = "%s.%s"%(node.expr.name,node.attrname)
            if node.expr.name == scope.getSelf():
                what = InstanceValue
            else:
                what = Name # hmm, this feels wrong, but I do want to distinguish

            if hasattr(node,"docstring"):
                #name = what(node.attrname,fullname,node.expr.name,node.docstring)
                name = what(selfname,fullname,node.expr.name,node.docstring)
            else:
                #name = what(node.attrname,fullname,node.expr.name)
                name = what(selfname,fullname,node.expr.name)
            if hasattr(node,"assign_expr"):
                name.setValue(node.assign_expr)
            scope.addAttribute(selfname,name)

            if node.expr.name == scope.getSelf() and scope.__class__ == Class:
                name.set_class(scope)

            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # We should do *something* for our experimental scope
            # work at this point - but I guess it would be to add
            # this name to the class that contains the method in
            # which we are - and if we're actually in a *function*
            # which is to be assigned to a class at run-time (for
            # instance), that is a bit difficult (!). So ignore
            # this case for now...
            # OR maybe we should be adding in "fullname" and not
            #    "name" each time we add something to the scope.
            name.scope_define_parent(scope,self)
            scope.scope_define_name(selfname,fullname,name)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        else:
            # What to do here???
            # - it's presumably something like fred[3].attr, so I guess
            # we don't want to do anything very clever with it, at least
            # for the moment.

            print
            print ">>> Unhandled AST node <<<"
            print "    visitAssAttr: %s"%(node)
            print "            i.e.: %s"%(utils.stringify_expr(node))
            print "        location: Module %s, %s %s, %s %s"%\
                  (self.scope_module.name,
                   scope.__class__.__name__,scope.name,
                   superscope.__class__.__name__,superscope.name)
            print

            # Hmm - what scope and superscope does the
            # subexpression *really* want...
            # - is there any point (from a DPV point of view)
            #   in following down the innards of the expression?
            self.visit(node.expr,scope,superscope)

    def visitAssName(self,node,scope=None,superscope=None):
        """Visit a name assignment node.

        For example::

            a = 1

        Note that this is assignment to a *single* name only.

        `scope` is the scope of this assignment, and `superscope` is
        that scope's scope (if any).
        """
        if self.debug:
            self._report("name",node.name,scope,superscope)

        # For the moment, allow any assignments - later we may restrict
        # this (e.g., to Module, Class, __init__ method)
        if scope:
            fullname = "%s:%s"%(scope.fullname,node.name)
        else:
            fullname = node.name

        if scope.__class__ == Module:
            what = ModuleValue
        elif scope.__class__ == Class:
            what = ClassValue
        else:
            what = Name

        if hasattr(node,"docstring"):
            name = what(self.settings,node.name,fullname,
                docstring=node.docstring)
        else:
            name = what(self.settings,node.name,fullname)
        if hasattr(node,"assign_expr"):
            name.setValue(node.assign_expr)
        if scope.isGlobal(node.name):
            name.setGlobal()
        scope.addAttribute(node.name,name)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Experimental scope work
        if scope:
            name.scope_define_parent(scope,self)
            scope.scope_define_name(node.name,fullname,name)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Hmm - check whether the name is a global, and if so,
        # tell our Module about it...
        if scope.isGlobal(node.name):
            self.addGlobalUsed(node.name,scope.fullname)

    def visitAssTuple(self,node,scope=None,superscope=None):
        """Visit a tuple assignment statement.

        For example:

            a, b, c = 1, 2, 3

        Although we're not directly interested in these for docstring
        purposes, we *may* want to know that a variable has participated
        in one (why?).
        """
        for item in node.nodes:
            self.visit(item,scope,superscope)

    def visitAssList(self,node,scope=None,superscope=None):
        """Visit a list assignment statement.

        For example:

            a, b, c = [1, 2, 3]

        Although we're not directly interested in these for docstring
        purposes, we *may* want to know that a variable has participated
        in one (why?).
        """
        for item in node.nodes:
            self.visit(item,scope,superscope)

    def visitGlobal(self,node,scope=None,superscope=None):
        """Visit a global statement node

        `scope` is the scope of this global, and `superscope` is
        that scope's scope (if any).
        """
        if self.debug:
            for name in node.names:
                self._report("global",name,scope,superscope)

        for name in node.names:
            scope.addGlobal(name)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Experimental scope work
            scope.scope_define_global(name)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    def visitYield(self,node,scope=None,superscope=None):
        """Visit a yield statement.

        The presence of a yield statement within a function indicates
        that it is a generator (well, unless that statement is
        unreachable - but we shall not worry about that, at least
        for the moment!).

        `scope` is the scope of this statement, and `superscope` is
        that scope's scope (if any).
        """
        if self.debug:
            self._report("yield",node.value,scope,superscope)

        # So tell our scope that it is (probably) a generator
        scope.generator = 1

        # There seems little point in visiting our value, but on
        # the other hand, why not
        self.visit(node.value,scope,superscope)


    def visitImport(self,node,scope=None,superscope=None):
        """Visit an import statement node

        `scope` is the scope of this import statement, and
        `superscope` is that scope's scope (if any).
        """
        if self.debug:
            self._report("import","",scope,superscope)
            print >>sys.stderr, "%8s %12s %s"%(" "," ",node.names)

        if scope:
            scope.addImport(node.names)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Experimental scope work
            for item in node.names:
                fullname = scope.fullname + "." + item[0]
                name = ImportName(self.settings,item[0],fullname)
                name.scope_define_parent(scope,self)
                scope.scope_define_name(item[0],fullname,name)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    def visitFrom(self,node,scope=None,superscope=None):
        """Visit a from .. import statement node

        The rules for `scope` and `superscope` should be identical
        to those for `visitImport()`.
        """
        if self.debug:
            self._report("from",node.modname,scope,superscope)
            print >>sys.stderr, "%8s %12s %s"%(" "," ",node.names)

        if scope is not None:
            scope.addFromImport(node.modname,node.names)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Experimental scope work
            for item in node.names:
                fullname = scope.fullname + "." + item[0]
                name = ImportName(self.settings,item[0],fullname)
                name.scope_define_parent(scope,self)
                scope.scope_define_name(item[0],fullname,name)
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ----------------------------------------------------------------------

    def _prt(self,stream,indent,name,place):
        stream.write("%sGlobal %-10s     in %s\n"%(" "*indent,name,place))

    def _and(self,stream,indent,name,place):
        stream.write("%s       %-10s and in %s\n"%(" "*indent," "*len(name),
                                                    place))

    def _show_odd_globals(self,stream,indent=0):
        """Show which global names are defined and used below module level

        Obviously (!) we are only interested in things which are not
        already known to be module level attributes - we want to warn
        the user about things that are defined in non-obvious places.
        """
        obscure = self.getObscureGlobals()
        done_header = 0
        for name,wherelist in obscure:
            if not done_header:
                stream.write("Globals defined *and used* below"
                             " the top level:\n")
                done_header = 1
            self._prt(stream,indent+2,name,wherelist[0])
            for place in wherelist[1:]:
                self._and(stream,indent+2,name,place)

    def show(self,stream,indent=0):
        stream.write("%sModule %s"%(" "*indent,self.name))
        if self.filename:
            stream.write("(in file %s)\n"%self.filename)
        else:
            stream.write("\n")
        self._show_body(stream,indent)
        self._show_odd_globals(stream,indent)

    def show_ast(self,stream):
        """Print out a representation of our AST.
        """
        stream.write("AST for module %s\n"%self.name)
        utils.treeprint(stream,self.ast)

# ----------------------------------------------------------------------
class Class(Base):
    """The representation of a Python class."""

    def __init__(self,settings,name,fullname,bases,docstring):
        Base.__init__(self,settings,name,fullname,docstring)

        self.bases = bases or []
        """A list of the base classes for this class."""

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Experimental scope setup
        self.scope_init()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    def is_callable(self):
        """Does it make sense to call an object of this type?

        NB: it only makes sense to call this *after* the class has
        been parsed!
        """
        return self.functions.has_key("__call__")

    def show(self,stream,indent=0):
        basestr = ""
        if self.bases:
            names = self.getBaseNames()
            basestr = "(" + string.join(names,",") + ")"
        stream.write("%sClass %s%s\n"%(" "*indent,self.name,basestr))
        self._show_body(stream,indent)

    def getBaseNames(self):
        names = []
        for base in self.bases:
            # Unfortunately, we don't yet cope with base classes
            # like "docutils.nodes" in our visitor...
            # Make the best of it we can, for now
            try:
                names.append(base.name)
            except:
                names.append("???")
        return names

    # Is it worth doing this?
    addMethod = Base.addFunction
    getMethodNames = Base.getFunctionNames


# ----------------------------------------------------------------------
class Function(Base):
    """The representation of a Python function (and thus also of a method).

    But see also `Method`.
    """

    def __init__(self,settings,name,fullname,args,defaults,flags,docstring):
        Base.__init__(self,settings,name,fullname,docstring)

        self.args = args
        """The arguments for this function or method."""

        self.defaults = defaults
        """The defaults (if any) for the latter arguments.

        Note that if there are 4 arguments, and only the last
        2 have defaults, then this list will be 2 items long.
        """

        self.flags = flags
        """Used to indicate \*args and \**kwargs.
        """

        self.generator = 0
        """Set to true if this function appears to be a generator
        (i.e., it contains a "yield" statement). Obscure code that
        doesn't actually *reach* the yield statement - for instance::

            def fred():
                print "3"
                if 1: return
                yield "2"

        will also, incorrectly, be recognised as a generator - tough.
        """

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Experimental scope setup
        self.scope_init()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    def is_callable(self):
        """Does it make sense to call an object of this type?
        """
        return 1

    def args_label(self):
        """Return the specialised label we use for all of our arguments.
        """
        return self.label() + "---args"

    def getArgs(self):
        """Return a representation of our arguments as a list (of strings).

        This is then suitable for joining with commas and displaying.
        """
        args = utils.merge_args(self.args,self.defaults)
        if not args:
            return args

        # The file compiler/consts.py defines CO_VARARGS=1 and
        # CO_VARKEYWORDS=2.

        # The method Transformer.com_arglist() in file compiler/transformer.py
        # handles a function's argument list.

        # According to that, if there's a "*args" item, then "flags" gets the
        # CO_VARARGS bit set, and if there's a "**args" item, then "flags" gets
        # the CO_VARKEYWORDS bit set. In either case, we *know* they've got to
        # be the last arguments, so we can count backwards (as we do for
        # default arguments).

        if self.flags & compiler.consts.CO_VARKEYWORDS:
            # If it's there, this must always be the last one...
            args[-1] = "**" + args[-1]

        if self.flags & compiler.consts.CO_VARARGS:
            # But this one might be last OR last but one
            if self.flags & compiler.consts.CO_VARKEYWORDS:
                args[-2] = "*" + args[-2]
            else:
                args[-1] = "*" + args[-1]

        return args

    def show(self,stream,indent=0):
        stream.write("%s%s %s(%s)\n"%(" "*indent,self.__class__.__name__,
                                      self.name,
                                      string.join(self.getArgs(),", ")))
        self._show_body(stream,indent)

    def getSelf(self):
        """For a method, return the name used for "self".

        (Actually, for a method *or* a function, return the name of
        the first argument - we need it on functions as well in case
        someone is defining a function later to be assigned to a class
        and used as a method.)
        """
        if len(self.args) > 0:
            return self.args[0]
        else:
            return None


# ----------------------------------------------------------------------
class Method(Function):
    """Just to get the class name right, I'm afraid...
    """
    pass


# ----------------------------------------------------------------------
class Name(ScopeMixin):
    """Information about use of a name in assignment.

    STILL UNDER CONSTRUCTION

    Test using pysource.py's "--show" option...


    We are not interested in *all* names, nor in all information
    about them...

    (We *could* inherit from Base, but that really brings too much other
    stuff that we don't need with it - and it doesn't *quite* seem worth
    having a mixin class (although the more I work with this, the more
    that seems a wrong decision - maybe next refactor time will change
    things.)
    """

    def __init__(self,settings,name,fullname,selfname=None,docstring=None):
        """Instantiate a new Name.

        * `settings`  -- the settings for the parser
        * `name`     -- the name of this, erm, name
        * `fullname` -- the "fully qualified" name - this is the "path" from
          our top-level entity down to this name (e.g., module.class.name)
        * `selfname` -- if this is an instance attribute, the string used
          as the "self" equivalent
        * `docstring` -- the docstring for this name, if any.
        """

        self.name = name
        """The name of this, erm, name.
        """

        self.fullname = fullname
        """The fully qualified name of this name
        - for instance, for a name `fred` in a class `Jim`
        in a module `bob`, this would be "bob.Jim.fred"
        """

        self.docstring = make_Docstring(docstring, settings)
        """The docstring for this name, or None.
        Note that, at least for the moment, we're only supporting
        one docstring for an entity. See the equivalent comment
        in `Base`.
        """

        self.firstuse = None
        """Remember the first use of this name - i.e., what is
        assigned to it. The value stored is the AST expression
        assigned to our name.
        """

        self.lastuse = None
        """Also remember the last use of this name. The value
        stored is the AST expression, as for firstuse.
        """

        self.reused = 0
        """Assume it has not been assigned to more than once.
        If it has, then we can't *really* report on its 'content',
        and `self.firstuse` should be ignored.
        """

        self.selfname = selfname
        """If this name is referred to as something like 'self.name'
        then we want to remember the string used as 'self' (since the
        user *might* be using something else). This will be ``None``
        if there is no 'prefix'.
        """

        self.isglobal = 0
        """True if this name is global to its module.
        """

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Experimental scope setup
        self.scope_init()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    def parse_docstrings(self,parser):
        """If we are meant to, parse our docstrings.

        (The oddly plural name is for compatibility with those things
        that inherit from `Base` - another reason for thinking that a
        mixin class may be about due...)
        """
        if self.docstring:
            self.docstring.parse(parser)

    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Experimental <interpreted> node code...
    def find_docstrings(self):
        """Find each docstring in our subtree.

        (The oddly plural name is for compatibility with those things
        that inherit from `Base` - another reason for thinking that a
        mixin class may be about due...)
        """
        if self.docstring:
            self.docstring.transform_interpreted(self)
    # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    def is_callable(self):
        """Does it make sense to call an object of this type?
        """
        return 0

    def label(self):
        """Return an XML-legal name for this object.

        Note that we implicitly assume that all objects have their
        output XML/HTML representation written to the same file
        - i.e., that we do not have to worry about references that
        are anything beyond simple names.

        (another duplicate of a method in Base)
        """
        str = string.replace(self.fullname,".","-")
        str = string.replace(str,":","--")
        return str

    def setValue(self,expr):
        self.lastuse = expr
        if not self.firstuse:
            self.firstuse = expr
        else:
            self.reused = 1

    def setGlobal(self):
        self.isglobal = 1

    def isGlobal(self):
        return self.isglobal

    def getDocstring(self):
        return self.docstring

    def getSelfName(self):
        return self.selfname

    def getValueString(self):
        if self.reused or not self.firstuse:
            return None
        else:
            return utils.stringify_expr(self.firstuse)

    def getLastValueString(self):
        """Return the stringified value of this name, or None.
        """
        return utils.stringify_expr(self.lastuse)

    def actualLastValue(self):
        """Return the actual value assigned to this name.

        If the value assigned is a constant, then return it, otherwise
        raise a ValueError (we can't just return NULL because that *is*
        a sensible constant!).
        """
        thing = self.lastuse
        # See utils.stringify_expr() for information on what we think
        # we're doing here...
        if isinstance(thing,compiler.ast.Const):
            return thing.value
        else:
            raise ValueError,"Value assigned to %s is not a constant:\n" \
                  "(%s)"%(self.getSelfName(),self.getLastValueString())

    def getAsString(self):
        str = self.name
        val = self.getValueString()
        if val:
            str += " = " + val
        return str

    def show(self,stream,indent=0):
        """Override as necessary."""
        stream.write("%s%s %s"%(" "*indent,self.__class__.__name__,
                                self.getAsString()))
        if self.isglobal:
            stream.write("  (global)\n")
        else:
            stream.write("\n")
        if self.docstring:
            self.docstring.show(stream," "*(indent+2))


# ----------------------------------------------------------------------
class Argument(Name):
    """An argument to a method or function.

    This is a separate class just to get the class name right, I'm afraid...
    """
    pass


# ----------------------------------------------------------------------
class ModuleValue(Name):
    """A name defined by assignment at the top level of a module.

    This is a separate class just to get the class name right, I'm afraid...
    """
    pass


# ----------------------------------------------------------------------
class ClassValue(Name):
    """A name defined by assignment within a class definition.

    This is a separate class just to get the class name right, I'm afraid...
    """
    pass


# ----------------------------------------------------------------------
class InstanceValue(Name):
    """An instance defined by assignment within a method - e.g., self.thing

    TO DO: Hmm - I need to actually *implement* the use of `self.our_class`
    and `self.see_also`.
    """

    def __init__(self,*args,**kws):
        Name.__init__(self,*args,**kws)

        self.our_class = None
        """If we are self.thing in a method, then "self" refers to a class
        (the one enclosing the method!), and this will be a reference to it.
        """

        self.see_also = None
        """If the class already defined a `ClassInstance` of this name,
        then we'll ultimately want a reference to that class here, so
        that our output can say "see also class instance XXX".
        """

    def set_class(self,klass):
        """Indicate which Class we really belong to.
        """
        self.our_class = klass


# ----------------------------------------------------------------------
class ImportName(Name):
    """A name assigned by "import" or "from ... import"

    Should we distinguish these? ((yes))

    This is a separate class just to get the class name right, I'm afraid...

    NB: we don't yet deal with "import ... as ..."
    """

    def is_callable(self):
        """Does it make sense to call an object of this type?

        Unfortunately, if it's something imported with "from ... import"
        there is no simple way of knowing - so maybe err on the side of
        assuming so.

        ((Note that this is actually a good argument for separating out
        the two sorts of import, since we know *modules* are not callable,
        and "import" just makes modules visible.))
        """
        return 1


# ----------------------------------------------------------------------
def test_parse_module(filename):
    print "Reading file %s"%filename
    return Module(filename,debug=0)

def test_show_ast(thing):
    print
    print "AST"
    print "==="
    thing.show(sys.stdout)

def test_show_scopes(thing):
    print
    print "Scopes"
    print "======"
    thing.scope_show()

def test():
    print "Testing pysource/visit.py"
    if len(sys.argv) <= 1:
        print "Usage: pysource/visit.py <python-file>"
        return
    filename = sys.argv[1]
    thing = test_parse_module(filename)
    test_show_ast(thing)
    test_show_scopes(thing)

if __name__ == "__main__":
    test()
