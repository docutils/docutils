==========================================
reStructuredText Interpreted Role Registry
==========================================

This directory contains my implementation of an extensible interpreted
role registry, based largely on the existing directive registry.  It
contains two modules: `states.py` and `roles.py`.  Both are intended
to go in `docutils/docutils/parsers/rst/`.  (`roles.py` is a new
module; `states.py` is a replacement module).

Defining Roles
~~~~~~~~~~~~~~
Interpreted roles are implemented as functions with the following
signature::

    def role_fn(name, rawtext, text, lineno, inliner):
        """
        Given an interpreted text construct, return a tuple of two lists:
        document tree nodes, and system messages (may be empty).

        :Parameters:
          - `name`: The canonical name of the interpreted role.
          - `rawtext`: A string containing the enitre interpreted
            text input (including the role).  This should be
            included as the content of a system message if a
            problem is encountered.
          - `text`: The interpreted text content.
          - `lineno`: The line number where the interpreted text begins.
          - `inliner`: The inliner object that called role_fn.  This
            can be used for error reporting & nested parsing.
        """

See the module docstring in roles.py for more information.  

Registering Roles
~~~~~~~~~~~~~~~~~
To register a new role, use docutils.parsers.rst.roles.register_role().
Roles should be registered using canonial names; non-canonical names
can be specified via the language submodule.

Default Role
~~~~~~~~~~~~
The default interpreted role is defined by the DEFAULT_INTERPRETED_ROLE
variable in docutils.parsers.rst.roles.  It should contain a canonical
role name.

Differences from Directive System
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are some design differences between the roles system and the
directives system:

- All roles are registered via register_role(); there is no
  equivalant to _directive_registry, which contains a centralized
  list of directives to register.

- Instead, each directive is registered immediately following
  its definition.

- All standard roles are registered at import time; there is no
  mechanism for delaying imports until roles are actually used.

Changes to states.py
~~~~~~~~~~~~~~~~~~~~
A diff on the old states.py (revision 1.69) and my revised version will
show that I mostly removed code (the old role handling system) from
inside the Inliner class.  The only new code in states.py is the new
Inliner.interpreted(), and some simplifying changes to the part of
Inliner.interpreted_or_phrase_ref() that calls Inliner.interpreted().

Future Work
~~~~~~~~~~~
- It would be nice to add a directive to create new roles, based on
  existing ones.  Given that roles are implemented as functions,
  this would probably involve creating a wrapper function that:

  - calls the base role_fn function
  - modifies its output
  - returns the modified output

  It seems difficult to do this in a general way, though, since
  role functions are free to return any number of nodes, of any
  type.  So we might need to limit the extension mechanism to
  specific base role_fn's.
