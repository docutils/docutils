#!/usr/bin/python
# coding: utf-8

# :Author: Georg Brandl; Felix Wiemann; Günter Milde
# :Date: $Date$
# :Copyright: This module has been placed in the public domain.
#
# This is a merge of `Using Pygments in ReST documents`_ from the pygments_
# documentation, and a `proof of concept`_ by Felix Wiemann.
#
# .. class:: borderless
#
# ========== =============================================================
# 2007-06-01 Removed redundancy from class values.
# 2007-06-04 Merge of successive tokens of same type
#            (code taken from pygments.formatters.others).
# 2007-06-05 Separate docutils formatter script
#            Use pygments' CSS class names (like the html formatter)
#            allowing the use of pygments-produced style sheets.
# 2007-06-07 Merge in the formatting of the parsed tokens
#            (misnamed as docutils_formatter) as class DocutilsInterface
# 2007-06-08 Failsave implementation (fallback to a standard literal block
#            if pygments not found)
# 2010-11-27 Rename directive and class from "code-block" to "code".
#            Fix fallback if pygments not found.
#            Use class-based interface.
#            Add "number-lines" option.
# ========== =============================================================
#
# ::

"""Define and register a code directive using pygments"""

# Requirements
# ------------
# ::

from docutils import nodes
from docutils.parsers.rst import directives, Directive
try:
    import pygments
    from pygments.lexers import get_lexer_by_name
    from pygments.formatters.html import _get_ttype_class
    with_pygments = True
except ImportError:
    with_pygments = False


# Customisation
# -------------
#
# Do not insert inline nodes for the following tokens.
# (You could add e.g. Token.Punctuation like ``['', 'p']``.) ::

unstyled_tokens = ['']

# Tokenizer
# -----------------
#
# This interface class combines code from
# pygments.formatters.html and pygments.formatters.others.

class Tokenizer(object):
    """Parse `code` string and yield "classified" tokens.

    Arguments

      code     -- string of source code to parse
      language -- formal language the code is written in.

    Merge subsequent tokens of the same token-type.

    Yields the tokens as ``(ttype_class, value)`` tuples,
    where ttype_class is taken from pygments.token.STANDARD_TYPES and
    corresponds to the class argument used in pygments html output.
    """

    def __init__(self, code, language):
        self.code = code
        self.language = language

    def lex(self):
        # Get lexer for language (use text as fallback)
        try:
            lexer = get_lexer_by_name(self.language)
        except ValueError:
            # info: 'no pygments lexer for %s, using "text"' % self.language
            lexer = get_lexer_by_name('text')
        return pygments.lex(self.code, lexer)


    def join(self, tokens):
        """Join subsequent tokens of same token-type.

        Also, leave out the final '\n' (added by pygments).
        """
        tokens = iter(tokens)
        (lasttype, lastval) = tokens.next()
        for ttype, value in tokens:
            if ttype is lasttype:
                lastval += value
            else:
                yield(lasttype, lastval)
                (lasttype, lastval) = (ttype, value)
        if lastval != '\n':
            yield(lasttype, lastval)

    def __iter__(self):
        """parse code string and yield "classified" tokens
        """
        tokens = self.lex()
        for ttype, value in self.join(tokens):
            # yield (ttype, value)
            yield (_get_ttype_class(ttype), value)


class NumberLines(object):
    """Insert linenumber-tokens in front of every newline

    Nontrivial, as we need to weave these into the possibly
    multi-line tokens from pygments.
    """

    def __init__(self, tokens, startline, fmt_str):
        self.tokens = tokens
        self.lineno = startline
        self.fmt_str = fmt_str

    def __iter__(self):
        yield ('ln', self.fmt_str % self.lineno)
        for ttype, value in self.tokens:
            lines = value.split('\n')
            for line in lines[:-1]:
                yield (ttype, line + '\n')
                self.lineno += 1
                yield ('ln', self.fmt_str % self.lineno)
            yield (ttype, lines[-1])


# CodeBlock directive
# --------------------
# ::

class CodeBlock(Directive):
    """Parse and mark up content of a code block.
    """
    required_arguments = 1
    option_spec = {'class': directives.class_option,
                   'number-lines': directives.unchanged
                  }
    has_content = True

    def run(self):
        language = self.arguments[0]
        # Process number-lines with optional argument `startline`
        startline = self.options.get('number-lines', '1')
        try:
            startline = int(startline or 1) # default to 1 for empty str
        except ValueError:
            raise self.error(
                ':number-lines: option with non-integer start value')
        self.assert_has_content()

        # create a literal block element and set class argument
        code_block = nodes.literal_block(classes=['code', language]
                                        + self.options['class'])

        # iterator returning code tokens
        if with_pygments:
            tokens = Tokenizer(u'\n'.join(self.content), language)
        else:
            # TODO: warning or info?
            self.warning('Cannot highlight code, Pygments lexer not found.')
            tokens = [('', u'\n'.join(self.content))]

        if 'number-lines' in self.options:
            # pad linenumbers, e.g. endline == 100 -> fmt_str = '%3d '
            endline = startline + len(self.content)
            fmt_str = "%%%dd " % len(str(endline))
            # print startline, '...', endline, repr(fmt_str)
            tokens = NumberLines(tokens, startline, fmt_str)

        # parse content with pygments and add to code_block element
        for cls, value in tokens:
            if cls in unstyled_tokens:
                # insert as Text to decrease the verbosity of the output.
                code_block += nodes.Text(value, value)
            else:
                code_block += nodes.inline(value, value, classes=[cls])

        return [code_block]


# Register Directive
# ------------------
# ::

directives.register_directive('code', CodeBlock)

# .. _doctutils: http://docutils.sf.net/
# .. _pygments: http://pygments.org/
# .. _Using Pygments in ReST documents: http://pygments.org/docs/rstdirective/
# .. _proof of concept:
#      http://article.gmane.org/gmane.text.docutils.user/3689
#
# Test output
# -----------
#
# If called from the command line, call the docutils publisher to render the
# input::

if __name__ == '__main__':
    from docutils.core import publish_cmdline, default_description
    description = 'code-block directive test output' + default_description
    try:
        import locale
        locale.setlocale(locale.LC_ALL, '')
    except:
        pass
    # Uncomment the desired output format:
    # publish_cmdline(writer_name='pseudoxml', description=description)
    # publish_cmdline(writer_name='xml', description=description)
    publish_cmdline(writer_name='html', description=description)
    # publish_cmdline(writer_name='latex', description=description)
