"""Transfer the structure from visit.py into a DOCUTILS node tree.

TODO: ensure that the encoding of individual items makes sense.

This should probably be using David's new mechanisms from the docutils module
to build the DOCUTILS tree - but they were written after this was coded, and
I've still to investigate them.
"""

import string

import docutils.nodes
import docutils.utils

import visit
import utils
import buildtree

__docformat__ = "reST"


# ----------------------------------------------------------------------
class Process:
    """The infrastucture that knows how to produce our DOCUTILS node tree.
    """

    def __init__(self, with_groups=1, document=None):
        """Instantiate our transformation to a DOCUTILS tree.

        At the moment, whilst we are considering both my own old HTML
        output and also moving towards David Goodger's DOCUTILS HTML output,
        we have the ``with_groups`` argument - if this is false then
        buildtree will not output "group" elements...
        """
        self.current_module = None
        self.dps = buildtree.BuildTree(with_groups=with_groups, root=document)

    def __call__(self,thing):
        """Produce a DOCUTILS 'document' node and attach our information to it
        """
        self.dps.start("document")

        if thing.__class__ == visit.Package:
            self.add_package(thing)
        elif thing.__class__ == visit.Module:
            self.add_module(thing)
        else:
            raise ValueError,"Argument must be a Package or Module,"\
                  " not a %s"%thing.__class__.__name__

        self.dps.end("document")
        document = self.dps.finish()

        # David's HTML writer requires the document to have a "source"
        # attribute - so add a temporary one (something better should
        # be done later on - see what David does!)
        document["source"] = "pysource"
        return document

    def add_package(self,thing):
        """Process a package and produce DOCUTILS nodes for it.
        """

        self.dps.start("section",style="package")
        self.dps.add("target",name=thing.label())
        self.dps.add("title",self.title(thing))

        self.dps.start("group",style="details")
        self.dps.add("paragraph","Path: "+thing.directory)
        self.dps.end("group")

        for mod in thing.getModules():
            self.add_module(mod)

        self.dps.end("section")

    def add_module(self,thing):
        """Process a module and produce DOCUTILS nodes for it.
        """

        self.current_module = thing

        self.dps.add("target",name=thing.label())
        self.dps.start("section",style="module")
        self.dps.add("title",self.title(thing))

        self.dps.start("group",style="details")
        self.dps.start("paragraph","Full name: ")
        self.dps.add("emphasis",thing.fullname)
        self.dps.end("paragraph")
        self.dps.add("paragraph","Path: "+thing.filename)
        self.dps.end("group")
        
        self.add_generic(thing)

        obscure = thing.getObscureGlobals()
        if obscure:
            self.dps.start("group",style="obscure_globals")
            self.dps.start("paragraph")
            self.dps.add("strong","Globals in obscure places")
            self.dps.start("bullet_list")

            for name,places in thing.getObscureGlobals():
                self.dps.start("list_item")
                self.dps.start("paragraph")
                self.dps.add("literal",name)
                self.dps.add("text"," defined and used in")
                if len(places) == 1:
                    self.dps.add("text"," ")
                    self.dps.add("literal",places[0])
                else:
                    self.dps.add("text",":")
                    self.dps.start("bullet_list")
                    for place in places:
                        self.dps.add("list_item",
                                      self.dps.make("literal",place))
                    self.dps.end("bullet_list")
                self.dps.end("list_item")
            self.dps.end("bullet_list")
            self.dps.end("group")

        self.dps.end("section")

    def add_generic(self,thing):
        """Basic stuff, common to just about all things
        """

        # Side effects, all is side effects...
        self.add_docstring(thing)
        self.add_imports(thing)
        self.add_attributes(thing)
        self.add_classes(thing)
        self.add_functions(thing)

    def add_docstring(self,thing):
        is_function = (thing.__class__ in [visit.Method,visit.Function])

        if not thing.docstring and not is_function:
            return

        self.dps.start("group",style="docstring")

        if is_function:
            args = thing.getArgs()
            self.dps.start("paragraph")
            # Make the function/method name easily findable...
            self.dps.start("literal",style="funcall")
            self.dps.add("text",thing.name)
            self.dps.end("literal")
            # And then we have the arguments...
            self.dps.add("target",name=thing.args_label())
            self.dps.start("literal",style="funargs")
            self.dps.add("text","(%s)"%string.join(args,", "))
            self.dps.end("literal")
            self.dps.end("paragraph")

        if thing.docstring:
            # Insert the docstring into our tree structure
            thing.docstring.add_to_DPS(self.dps)

        self.dps.end("group")

    # #############################################################
    # Should we sort things alphabetically within each list, below?
    # - maybe yes for some, but no for others...
    # #############################################################

    def add_imports(self,thing):
        """Work out a description of any imports.

        `thing` is the visitor node that we are to use to construct
        DOCUTILS nodes.
        """

        imports = thing.getImports()
        froms   = thing.getFromImports()

        if not imports and not froms:
            return

        self.dps.add("target",name=thing.label())
        self.dps.start("group",style="imports")
        self.dps.start("paragraph")
        self.dps.add("strong","Imports:")
        self.dps.start("bullet_list")

        if imports:
            names = imports.keys()
            names.sort()
            for name in names:
                self.dps.start("list_item")
                self.dps.start("paragraph","import ")
                self._add_import(name,imports)
                self.dps.end("list_item")

        if froms:
            #print froms
            modnames = froms.keys()
            modnames.sort()
            for modname in modnames:
                self.dps.start("list_item")
                self.dps.start("paragraph","from ")
                self.dps.add("literal",modname)
                self.dps.add("text"," import ")
                dict = froms[modname]
                names = dict.keys()
                names.sort()
                uselist = len(names) > 1
                if uselist:
                    self.dps.start("bullet_list")
                for name in names:
                    if uselist:
                        self.dps.start("list_item")
                        self._add_import(name,dict)
                        self.dps.end("list_item")
                    else:
                        self._add_import(name,dict)
                if uselist:
                    self.dps.end("bullet_list")
                self.dps.end("list_item")

        self.dps.end("bullet_list")
        self.dps.end("group")

    def _add_import(self,name,import_dict):
        self.dps.add("literal",name)
        aslist = import_dict[name]
        if len(aslist) == 1 and aslist[0] == None:
            self.dps.add("text"," (as ");
            self.dps.add("emphasis","itself");
            self.dps.add("text",")");
        else:
            self._add_aslist(aslist)

    def _add_aslist(self,aslist):
        self.dps.add("text"," as ")
        another = 0
        for name in aslist:
            if another:
                self.dps.add("text",", ")
            else:
                another = 1
            if name is None:
                self.dps.add("emphasis","itself")
            else:
                self.dps.add("literal",name) 

    def add_attributes(self,thing):
        attributes = thing.getAttributeNames()
        if not attributes:
            return

        # We don't do attributes for functions, or for methods that are
        # not called "__init__" or "__new__" (the DOCUTILS spec doesn't require
        # retention of attributes in "__new__" methods, but it seems sensible
        # to me, and I'm not sure they were around when the DOCUTILS spec was
        # first written...)
        if thing.__class__ == visit.Function:
            return
        if thing.__class__ == visit.Method and \
           thing.name not in ("__init__","__new__"):
            return

        self.dps.start("group",style="attributes")
        self.dps.start("paragraph")
        self.dps.add("strong","Attributes:")
        self.dps.start("bullet_list")

        for name in attributes:
            self.dps.start("list_item")
            nameinst = thing.getAttribute(name)
            self.dps.start("paragraph")
            self.dps.add("target",name=nameinst.label())
            self.dps.add("literal",nameinst.getAsString())
            if nameinst.isGlobal():
                self.dps.add("text"," ")
                self.dps.add("emphasis","(global)")
            if nameinst.__class__ == visit.InstanceValue and \
               nameinst.see_also:
                self.dps.add("text"," - see also class instance ")
                self.dps.add("a",nameinst.our_class.name,
                             href="#"+nameinst.our_class.label())
            self.add_docstring(nameinst)
            self.dps.end("list_item")

        self.dps.end("bullet_list")
        self.dps.end("group")

    def add_classes(self,thing):
        class_names = thing.getClassNames()
        for name in class_names:
            class_instance = thing.getClass(name)
            self.add_class(class_instance)

    def add_class(self,thing):
        """Process a class and produce DOCUTILS nodes for it.
        """

        self.dps.add("target",name=thing.label())
        self.dps.start("section",style="class")
        self.dps.add("title",self.title(thing))

        self.dps.start("group",style="details")
        self.dps.start("paragraph","Full name: ")
        self.dps.add("emphasis",thing.fullname)

        names = thing.getBaseNames()
        if names:
            self.dps.start("paragraph","Inherits from: ")
            done_one = 0
            for name in names:
                if done_one:
                    self.dps.add("text",", ")
                else:
                    done_one = 1
                self.dps.add("literal",name)
        self.dps.end("group")

        self.add_generic(thing)
        self.dps.end("section")

    def add_functions(self,thing):
        """Add information about any functions or methods.
        """

        func_names = thing.getFunctionNames()
        for name in func_names:
            fun_instance = thing.getFunction(name)
            self.add_function(fun_instance)

    def add_function(self,thing):
        """Process a function or method and produce DOCUTILS nodes for it.
        """

        if thing.__class__ == visit.Method:
            style = "method"
        else:
            style = "function"
        self.dps.add("target",name=thing.label())
        self.dps.start("section",style=style)
        self.dps.add("title",self.title(thing))

        self.dps.start("group",style="details")
        self.dps.start("paragraph","Full name: ")
        self.dps.add("emphasis",thing.fullname)
        self.dps.end("group")

        if thing.generator:
            self.dps.start("group",style="generator")
            self.dps.add("paragraph","%s %s is actually a generator"%\
                         (thing.__class__.__name__,thing.name))
            self.dps.end("group")

        self.add_generic(thing)
        self.dps.end("section")

    def title(self,thing):
        return "%s %s"%(thing.__class__.__name__,thing.name)
