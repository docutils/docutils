"""Utilities for pysource
"""

import types
import string
import compiler

# We'd better have at least *one* module in this package that demonstrates
# *not* using reST for our docstrings...
__docformat__ = "none"


# ----------------------------------------------------------------------
PLAINTEXT = "plaintext"
RESTRUCTUREDTEXT = "restructuredtext"

canonical_format = { "plaintext" : PLAINTEXT,
                     "plain"     : PLAINTEXT,
                     "none"      : PLAINTEXT,
                     "rst"              : RESTRUCTUREDTEXT,
                     "rest"             : RESTRUCTUREDTEXT,
                     "rtxt"             : RESTRUCTUREDTEXT,
                     "restructuredtext" : RESTRUCTUREDTEXT,
                     }

def docformat(text):
    """Interpret a module's __docformat__ string.

    Returns a tuple of (format,language)
    """
    if text == None:
        return PLAINTEXT,"en"

    words = string.split(string.lower(text))

    #print words

    if len(words) == 0:
        return PLAINTEXT,"en"
    elif len(words) > 2:
        raise ValueError,"__docformat__ may be at most two 'words'"

    if len(words) == 2:
        language = string.lower(words[1])
    else:
        language = "en"

    try:
        format = canonical_format[string.lower(words[0])]
    except KeyError:
        legal = canonical_format.keys()
        legal.sort()
        raise ValueError,"__docformat__ should be one of %s"%legal

    return format,language


# ----------------------------------------------------------------------
def flatten(item):
    """Retrieve some simpler representation of our AST.

    (and it's not meant to be 'theoretically' wonderful, just something
    I can look at to work out how an AST works...)
    """
    if isinstance(item,compiler.ast.Node):
        things = [item.__class__.__name__]
        children = item.getChildren()
        for child in children:
            things.append(flatten(child))
        return things
    else:
        return [item]


# ----------------------------------------------------------------------
def treeprint(stream,item,indent=0):
    """Simple pretty printer for the AST."""
    if isinstance(item,compiler.ast.Node):
        stream.write("\n%s<%s>"%(" "*indent,item.__class__.__name__))

        children = item.getChildren()
        for child in children:
            treeprint(stream,child,indent+2)

        # Fake our docstring as a sub-node (it's *really* more an attribute)
        if hasattr(item,"docstring"):
            stream.write("\n%s  <docstring> %s"%(" "*indent,item.docstring))

        # And ditto for a remembered assignment expression
        if hasattr(item,"assign_expr"):
            stream.write("\n%s  <assign_expr>"%(" "*indent))
            treeprint(stream,item.assign_expr,indent+4)
    else:
        stream.write(" ")
        stream.write(`item`)


# ----------------------------------------------------------------------
def find_attr_docs(tree,verbose=0):
    """Find candidates for documented attributes

    Note that after this, it may be that the AST will not garbage collect
    its own nodes properly anymore, as we are adding in cross-linkages.
    """

    if not isinstance(tree,compiler.ast.Node):
        return

    children = tree.getChildren()

    # Might as well get our recursion done with first...
    for child in children:
        find_attr_docs(child,verbose)

    # I believe that only Stmt nodes can have Assign and Discard
    # nodes as children
    if not isinstance(tree,compiler.ast.Stmt):
        return

    if len(children) == 0:
        return

    pairs = []
    last  = children[0]
    for item in children[1:]:
        pairs.append((last,item))
        last = item

    for this,next in pairs:
        if isinstance(this,compiler.ast.Assign) and \
           isinstance(next,compiler.ast.Discard):
            if verbose:
                print
                print
                print "*** Attribute docstring candidate"
                treeprint(this,4)
                treeprint(next,4)
                print

            nextexpr = next.expr
            if isinstance(nextexpr,compiler.ast.Const):
                if type(nextexpr.value) == types.StringType:
                    docstring = nextexpr.value
                else:
                    if verbose:
                        print
                        print "... Discarded constant is not a string"
                    continue
            else:
                if verbose:
                    print
                    print "... Discarded expression is not a constant"
                continue

            # If there is more than one assignment attached to
            # the <Assign> node, we are not interested
            if len(this.nodes) > 1:
                if verbose:
                    print
                    print "... (but there are too many assignments in the <Assign>)"
                continue

            target = this.nodes[0]
            if isinstance(target,compiler.ast.AssName):
                # Let's be cheeky and glue the docstring on...
                target.docstring = docstring
            elif isinstance(target,compiler.ast.AssAttr):
                # Let's be cheeky and glue the docstring on...
                target.docstring = docstring
            else:
                if verbose:
                    print
                    print "... (but the assignment is to a tuple/list/etc.)"
                continue

            if verbose:
                print
                print "Becomes:"
                treeprint(this,4)


# ----------------------------------------------------------------------
def find_attr_vals(tree,verbose=0):
    """Find attributes whose values we're interested in.

    Clearly, when this is working, it could do with being "folded" into
    `find_attr_docs()`.

    Note that after this, it may be that the AST will not garbage collect
    its own nodes properly anymore, as we are adding in cross-linkages.
    """

    if not isinstance(tree,compiler.ast.Node):
        return

    children = tree.getChildren()

    # Might as well get our recursion done with first...
    for child in children:
        find_attr_vals(child,verbose)

    # I believe that only Stmt nodes can have Assign and Discard
    # nodes as children
    if not isinstance(tree,compiler.ast.Stmt):
        return

    for this in children:
        if isinstance(this,compiler.ast.Assign):
            if verbose:
                print
                print
                print "*** Assignment - name/value candidate"
                treeprint(this,4)
                print

            # If there is more than one assignment attached to
            # the <Assign> node, we are not interested
            if len(this.nodes) > 1:
                if verbose:
                    print
                    print "... (but there are too many assignments in the <Assign>)"
                continue

            target = this.nodes[0]
            if isinstance(target,compiler.ast.AssName) or \
               isinstance(target,compiler.ast.AssAttr):
                # Let's be cheeky and glue the associated expression on...
                target.assign_expr = this.expr
            else:
                if verbose:
                    print
                    print "... (but the assignment is to a tuple/list/etc.)"
                continue

            if verbose:
                print
                print "Becomes:"
                treeprint(this,4)


# ----------------------------------------------------------------------
def stringify_arg(thing):
    """Return a string representation of a function argument.

    This just works for tuples of (strings or tuples (of strings ...) ...)
    """
    if type(thing) == types.StringType:
        return thing
    elif type(thing) == types.TupleType:
        innards = []
        for item in thing:
            innards.append(stringify_arg(item))
        return "(" + string.join(innards,",") + ")"
    else:
        raise ValueError,"Tried to stringify type %s"%type(thing)


# ----------------------------------------------------------------------
def merge_args(args,defaults):
    """Merge together arguments and defaults from an argument list.

    Returns a list of argument strings.
    """
    if args == None:
        return []

    if defaults == None:
        defaults = []

    # This is horrible - do it nicely later on!
    argstrs = []
    for item in args:
        argstrs.append(stringify_arg(item))

    pos = len(args) - len(defaults)
    next = 0
    for index in range(pos,len(args)):
        thing = defaults[next]
        thing = stringify_expr(thing)
        argstrs[index] = "%s=%s"%(argstrs[index],thing)
        next = next + 1
    return argstrs


# ----------------------------------------------------------------------
def stringify_expr(thing):
    """Return a very simple string representation of an expression node

    Specifically, this function aims to support stringifying things
    which can be on the RHS of an assignment - is that *actually* the
    same as stringifying expression nodes?
    """

    # Humph - saving typing may be a good thing...
    strify = stringify_expr

    #print thing.__class__.__name__
    
    if thing == None:
        return 'None'
    elif isinstance(thing,compiler.ast.Add):
        return strify(thing.left) + " + " + strify(thing.right)
    elif isinstance(thing,compiler.ast.And):
        exprs = []
        for node in thing.nodes:
            exprs.append(strify(node))
        return string.join(exprs," && ")
    elif isinstance(thing,compiler.ast.AssAttr):
        # Attribute as target of assignment
        if thing.flags == compiler.consts.OP_ASSIGN:
            return strify(thing.expr) + "." + thing.attrname
        else:
            raise ValueError,"Unexpected flag %d in %s"%(thing.flags,`thing`)
    elif isinstance(thing,compiler.ast.AssName):
        # Name as target of assignment, but this also means name
        # as target of "in" assignment (e.g., "x in [1,2,3]"),
        # which is why *we're* interested in it (since this can
        # occur in list comprehensions, which can occur as the
        # RHS of assignments)
        if thing.flags == compiler.consts.OP_ASSIGN:
            return thing.name
        else:
            raise ValueError,"Unexpected flag %d in %s"%(thing.flags,`thing`)
    elif isinstance(thing,compiler.ast.Backquote):
        return "`" + strify(thing.expr) + "`"
    elif isinstance(thing,compiler.ast.Bitand):
        exprs = []
        for node in thing.nodes:
            exprs.append(strify(node))
        return string.join(exprs," & ")
    elif isinstance(thing,compiler.ast.Bitor):
        exprs = []
        for node in thing.nodes:
            exprs.append(strify(node))
        return string.join(exprs," | ")
    elif isinstance(thing,compiler.ast.Bitxor):
        exprs = []
        for node in thing.nodes:
            exprs.append(strify(node))
        return string.join(exprs," ^ ")
    elif isinstance(thing,compiler.ast.CallFunc):
        # Yuck - this is getting complicated!
        # (for an example, see method `hyperlink_target` in
        # restructuredtext/states.py)
        str = strify(thing.node) + "("
        arglist = []
        if thing.args:
            for arg in thing.args:
                arglist.append(strify(arg))
        if thing.star_args:
            arglist.append("*"+strify(thing.star_args))
        if thing.dstar_args:
            arglist.append("**"+strify(thing.dstar_args))
        if arglist:
            str += string.join(arglist,", ")
        return str+")"
    elif isinstance(thing,compiler.ast.Compare):
        str = strify(thing.expr) + " "
        for op,val in thing.ops:
            str += op + " " + strify(val)
        return str
    elif isinstance(thing,compiler.ast.Const):
        # Try not to let long strings take up too much room...
        value = thing.value
        if type(value) == type("") and len(value) > 50:
            value = value[:47] + "..."
        # Make Python decide for us if it needs quotes round it
        # (and, if so, what sort)
        return `value`
    elif isinstance(thing,compiler.ast.Dict):
        innards = []
        for key,val in thing.items:
            key = strify(key)
            val = strify(val)
            innards.append(key+":"+val)
        return "{" + string.join(innards,", ") + "}"
    elif isinstance(thing,compiler.ast.Div):
        return strify(thing.left) + " / " + strify(thing.right)
    elif isinstance(thing,compiler.ast.Ellipsis):
        return "..."
    elif isinstance(thing,compiler.ast.Getattr):
        return strify(thing.expr) + "." + thing.attrname
    elif isinstance(thing,compiler.ast.Invert):
        # Bitwise negation
        return "~" + strify(thing.expr)
    elif isinstance(thing,compiler.ast.Keyword):
        # An 'arg=value' within a function call
        return thing.name + "=" + strify(thing.expr)
    elif isinstance(thing,compiler.ast.Lambda):
        str = "lambda "
        if thing.flags != 0:
            str += " <flag %d> "%thing.flags
        str += string.join(merge_args(thing.argnames,thing.defaults),", ")
        str += ": "
        str += strify(thing.code)
        return str
    elif isinstance(thing,compiler.ast.LeftShift):
        return strify(thing.left) + " << " + strify(thing.right)
    elif isinstance(thing,compiler.ast.List):
        innards = []
        for item in thing.nodes:
            innards.append(strify(item))
        return "[" + string.join(innards,", ") + "]"
    elif isinstance(thing,compiler.ast.ListComp):
        str = "["+strify(thing.expr)
        for node in thing.quals:
            str += " "+strify(node)
        return str+"]"
    elif isinstance(thing,compiler.ast.ListCompFor):
        str = "for "+strify(thing.assign)
        str += " in "+strify(thing.list)
        if thing.ifs:
            for node in thing.ifs:
                str += " "+strify(node)
        return str
    elif isinstance(thing,compiler.ast.ListCompIf):
        return "if "+strify(thing.test)
    elif isinstance(thing,compiler.ast.Mod):
        return strify(thing.left) + "%" + strify(thing.right)
    elif isinstance(thing,compiler.ast.Mul):
        return strify(thing.left) + " * " + strify(thing.right)
    elif isinstance(thing,compiler.ast.Name):
        return thing.name
    elif isinstance(thing,compiler.ast.Not):
        return "not " + strify(thing.expr)
    elif isinstance(thing,compiler.ast.Or):
        exprs = []
        for node in thing.nodes:
            exprs.append(strify(node))
        return string.join(exprs," || ")
    elif isinstance(thing,compiler.ast.Power):
        return strify(thing.left) + " ** " + strify(thing.right)
    elif isinstance(thing,compiler.ast.RightShift):
        return strify(thing.left) + " >> " + strify(thing.right)
    elif isinstance(thing,compiler.ast.Slice):
        if thing.flags != compiler.consts.OP_APPLY:
            raise ValueError,"Unexpected flag %d in %s"%(thing.flags,`thing`)
        return strify(thing.expr) + "[" + \
               strify(thing.lower) + ":" + strify(thing.upper) + "]"
    elif isinstance(thing,compiler.ast.Sliceobj):
        slicelist = []
        for idx in thing.nodes:
            slicelist.append(strify(idx))
        return string.join(slicelist,":")
    elif isinstance(thing,compiler.ast.Sub):
        return strify(thing.left) + " - " + strify(thing.right)
    elif isinstance(thing,compiler.ast.Subscript):
        if thing.flags != compiler.consts.OP_APPLY:
            raise ValueError,"Unexpected flag %d in %s"%(thing.flags,`thing`)
        str = strify(thing.expr) + "["
        sublist = []
        for sub in thing.subs:
            sublist.append(strify(sub))
        return str + string.join(sublist,", ") + "]"
    elif isinstance(thing,compiler.ast.Tuple):
        innards = []
        for item in thing.nodes:
            innards.append(strify(item))
        return "(" + string.join(innards,", ") + ")"
    elif isinstance(thing,compiler.ast.UnaryAdd):
        return "+" + strify(thing.expr)
    elif isinstance(thing,compiler.ast.UnarySub):
        return "-" + strify(thing.expr)
    else:
        return _whatsthis(thing)

def _whatsthis(thing):
    # Wrong, but what else can we do?
    import sys
    print >>sys.stderr,"stringify_expr - don't recognise %s %s"%\
          (thing.__class__.__name__,thing)
    return `thing`


