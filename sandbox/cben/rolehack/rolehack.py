"""Preprocess reStructuredText roles to directives.

This is a stop-gap hack for \"adding\" inline syntaxes to reST, mainly useful
when you find yourself using directives every 3 lines.  In such cases an
extension to docutils allowing some inline syntax is desired thing and such
extensions most probably will take the form of new interpretted text roles.

This module allows to easily prototype them by converting given interpretted
text roles to directives.  To avoid indentation- and line-related headache,
the uses of the roles are replaced with substitutions and all substitution
definitions are appeneded at the end of the document (hint: use ``replace::``
if you don't want a directive).  @@@ This has the weakness that all directives
become inline.

I was too lazy to implement an elaborate command-line interface, so this is
only a module.  You should import it from a python script and call this module
with specific role->template mappings to do the work.

BUGS
====

- Backslashes are not interpretted in any way (except that backticks preceded
  by backslashes are won't be treated as start/end of interpretted text).
  This means backslashes are passed to the directive which probably won't stay
  this way if the role is accepted into docutils (only the double-backtick
  literal text syntax behaves this way).

  This bug is semi-intentional because it makes LaTeX generation easier...

- The default role is only recognized if preceded by any whitespace (including
  '\n') or at start of document.

- Any number of lines is consumed in search for the closing backtick,
  disregarding indentation.  The content is pasted into the directive as one
  line with normalized whitespace.

- Starting and ending contexts for inline markup recognition are not verified.

- The width of the substitution references is not equal to the original, so
  you can't use it in tables.

- Long parts of the document without empty lines might cause ``recursion limit
  exceeded`` errors.

"""

import re

# Named groups are used to allow simultaneous replacement of all roles.

_re_options = re.IGNORECASE | re.DOTALL | re.VERBOSE

_default_re = r'''(?<![\`_:])     # not escaped, literal, target or prefix role
                  `(?P<DEFAULT>         # store as ``DEFAULT``
                    (?:[^`]|\\.)+)`     # skip escaped chars
                  (?![_:])              # not reference or postfix role
                  '''

_empty_line_re = re.compile('\n[ \t]*\n') # NOT `re.VERBOSE`

def _role2regexp(role):
    """Return regexp for approximate recognition of `role`."""
    return r''':%(role)s:               # prefix role
               `(?P<prefix_%(role)s>    # group names must differ
                 (?:[^`]|\\.)+)`        # skip escaped chars
               |
               (?<![\`_:])       # not escaped, literal, target or prefix role
               `(?P<postfix_%(role)s>   # group names must differ
                 (?:[^`]|\\.)+)`          # skip escaped chars
               :%(role)s:               # postfix role role
               ''' % locals()

def process(doc, roles, default=None):
    """Process `doc` replacing given `roles`.

    `doc` should be a single string containing the whole document.  The
    `roles` dictionary maps from role names to replacement functions that
    should accept the role content and return the directive text, starting
    from the directive name, e.g.::

        def repl(text):
            return 'raw:: latex\n\n    %s\n' % (text,)

    See `template()` for an easy way to create such trivial functions.  The
    optional `default` argument specifies a replacement for the default role.

    """
    re_parts = []
    repls = {}
    if default:
        re_parts.append(_default_re)
        repls['DEFAULT'] = default
    for role, repl in roles.items():
        re_parts.append(_role2regexp(role))
        repls['prefix_' + role] = repls['postfix_' + role] = repl
    full_re = '\n|'.join(re_parts)
    full_re = re.compile(full_re, _re_options)

    after_output = []
    def count(n=0):
        while True:
            yield n
            n += 1
    ids = count()
    def dispatch(match):
        role = match.lastgroup
        content = ' '.join(match.group(role).split())
        id = ids.next()
        subst = '|rolehack_%d|' % (id,)
        after_output.append('.. %s %s' % (subst, repls[role](content)))
        return subst

    # Hack: process by chunks separated by blank lines, trying to avoid
    # "recursion limit exceeded" errors.
    output = [full_re.sub(dispatch, chunk)
              for chunk in _empty_line_re.split(doc)]
    return '\n\n'.join(output + after_output)

def template(pre, post):
    """Make replacement function for wrapping content with two strings."""
    def repl(text):
        return ''.join((pre, text, post))
    return repl

def main(roles, default=None):
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
        output.write(process(input.read(), roles, default))
    parse_args(*sys.argv[1:])

##main({'foo': template('foo::\n\n    ', '\n')},
##     template('default::\n\n   ', '\n'))
