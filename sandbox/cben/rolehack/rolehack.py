"""Preprocess reStructuredText roles to directives.

This is a stop-gap hack for prototyping new syntaxes for reST, mainly useful
when you find yourself using directives every 3 lines.  In such cases an
extension to docutils allowing some inline syntax is desired thing and such
extensions most probably will take the form of new interpretted text roles.

This module allows to easily prototype them by converting given interpretted
text roles to directives.  To make them inline, the uses of the roles are
replaced with substitutions and all substitution definitions are appeneded at
the end of the document (hint: use ``replace::`` if you don't want a
directive).

Since what's useful for inline syntaxes might also be useful outside of
paragraphs, preprocessing simple directives (only an argument, no options or
content) into other directives is also supported.

I was too lazy to implement an elaborate command-line interface, so this is
only a module.  You should import it from a python script and call this module
with specific role->template mappings to do the work.

BUGS
====

There are too many.  Most can't be fixed here, the right thing is to extend
the docutils parser...

- Backslashes are not interpretted in any way (except that backticks preceded
  by backslashes are won't be treated as start/end of interpretted text).
  This means backslashes are passed to the directive which probably won't stay
  this way if the role is accepted into docutils (only the double-backtick
  literal text syntax behaves this way).

  This bug is semi-intentional because it makes LaTeX generation easier...

- Any number of lines is consumed in search for the closing backtick,
  disregarding indentation.  The content is pasted into the directive as one
  line with normalized whitespace.

- The width of the substitution references is not equal to the original, so
  you can't use it in tables.

- Long parts of the document without empty lines might cause ``recursion limit
  exceeded`` errors.

- Directives not recognized if preceded by non-whitespace (e.g. in a table).

"""

import re

# Named groups are used to allow simultaneous replacement of all roles.

_re_options = re.IGNORECASE | re.MULTILINE | re.DOTALL | re.VERBOSE

def _role_re(group_name):
    return r'''
    # Start-string:
    (?:^|(?<=\s|[\'"([{<\-/:]))
    `
    (?=\S)
    # Content:
    (?P<%(group_name)s>
        (?:[^`]|\\.)+                           # skip escaped chars
    )
    # End-string:
    (?<=\S)
    `
    (?=$|\s|[#\'")\]}>\-/:.,;!?\\])
    ''' % locals()

_default_re = _role_re('_DEFAULT')

def _role2regexp(role):
    """Return regexp for approximate recognition of `role`."""
    prefix_re = _role_re('prefix_' + role)
    postfix_re = _role_re('postfix_' + role)
    return r'''
    :%(role)s:
    %(prefix_re)s
    |
    %(postfix_re)s
    :%(role)s:
    ''' % locals()

def _dir2regexp(dir):
    """Return regexp for approximate recognition of directive `dir`."""
    return r'''
    ^(?P<indent_%(dir)s> [ \t]* )       # record indentation
    \.\. \s+
    (?P<subst_%(dir)s>
    ##    (?:|[^|]*|)?                    # optional substitution
    )
    \s*
    %(dir)s \s* ::
    (?P<argument_%(dir)s>
        [^\n]*
        (?:
            \n
            (?P=indent_%(dir)s) [ \t]   # bigger indentation
            [^\n]+
        )*
    )
    ''' % locals()

def process(doc, roles={}, default_role=None, directives={}):
    """Process `doc` replacing given `roles`.

    `doc` should be a single string containing the whole document.  The
    `roles` dictionary maps from role names to replacement functions that
    should accept the role content and return the directive text, starting
    from the directive name, e.g.::

        def repl(text):
            return 'raw:: latex\n\n    %s\n' % (text,)

    See `template()` for an easy way to create such trivial functions.  The
    optional `default_role` argument specifies a replacement for the default
    role.

    The `directives` dictionary like `roles` but specifies directive names to
    handle.  The directive can have only an argument; substitution definitions
    with these directives are also recognized.  Indentation is adjusted
    properly for directives.

    """
    re_parts = []
    repls = {}
    if default_role:
        re_parts.append(_default_re)
        repls['_DEFAULT'] = default_role
    for role, repl in roles.items():
        re_parts.append(_role2regexp(role))
        repls['prefix_' + role] = repls['postfix_' + role] = repl
    for dir, repl in directives.items():
        re_parts.append(_dir2regexp(dir))
        repls['argument_' + dir] = repl
    full_re = '\n|'.join(re_parts)
    full_re = re.compile(full_re, _re_options)

    after_output = []
    def count(n=0):
        while True:
            yield n
            n += 1
    ids = count()
    def dispatch(match):
        groupname = match.lastgroup
        content = match.group(groupname)
        kind, name = groupname.split('_', 1)
        if kind == 'argument':          # substitution
            indent = match.group('indent_' + name)
            subst = match.group('subst_' + name)
            repl = '\n.. %s %s' % (subst, repls[groupname](content))
            return indent + repl.replace('\n', '\n' + indent)
        else:                           # role
            id = ids.next()
            subst = '|rolehack_%d|' % (id,)
            repl = '.. %s %s' % (subst, repls[groupname](content))
            after_output.append(repl)
            return subst

    # Hack: process by chunks separated by blank lines, trying to avoid
    # "recursion limit exceeded" errors.
    empty_line_re = re.compile(r'\n[ \t]*\n')
    output = [full_re.sub(dispatch, chunk)
              for chunk in empty_line_re.split(doc)]
    return '\n\n'.join(output + after_output)

def template(pre, post):
    """Make replacement function for wrapping content with two strings."""
    def repl(text):
        return ''.join((pre, text, post))
    return repl

def main(*args, **kw_args):
    """Simple command-line interface."""
    import sys
    def parse_args(input='-', output='-'):
        if input == '-':
            input = sys.stdin
        else:
            input = file(input)
        if output == '-':
            output = sys.stdout
        else:
            output = file(output, 'w')
        output.write(process(input.read(), *args, **kw_args))
    parse_args(*sys.argv[1:])

##main({'foo': template('foo::\n\n    ', '\n')},
##     template('default::\n\n   ', '\n'))
