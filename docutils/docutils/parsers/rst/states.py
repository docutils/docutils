"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This is the ``docutils.parsers.restructuredtext.states`` module, the core of
the reStructuredText parser. It defines the following:

:Classes:
    - `RSTStateMachine`: reStructuredText parser's entry point.
    - `NestedStateMachine`: recursive StateMachine.
    - `RSTState`: reStructuredText State superclass.
    - `Body`: Generic classifier of the first line of a block.
    - `BulletList`: Second and subsequent bullet_list list_items
    - `DefinitionList`: Second and subsequent definition_list_items.
    - `EnumeratedList`: Second and subsequent enumerated_list list_items.
    - `FieldList`: Second and subsequent fields.
    - `OptionList`: Second and subsequent option_list_items.
    - `Explicit`: Second and subsequent explicit markup constructs.
    - `SubstitutionDef`: For embedded directives in substitution definitions.
    - `Text`: Classifier of second line of a text block.
    - `Definition`: Second line of potential definition_list_item.
    - `Line`: Second line of overlined section title or transition marker.
    - `Stuff`: An auxilliary collection class.

:Exception classes:
    - `MarkupError`
    - `ParserError`
    - `TransformationError`

:Functions:
    - `escape2null()`: Return a string, escape-backslashes converted to nulls.
    - `unescape()`: Return a string, nulls removed or restored to backslashes.
    - `normname()`: Return a case- and whitespace-normalized name.

:Attributes:
    - `stateclasses`: set of State classes used with `RSTStateMachine`.

Parser Overview
===============

The reStructuredText parser is implemented as a state machine, examining its
input one line at a time. To understand how the parser works, please first
become familiar with the `docutils.statemachine` module. In the description
below, references are made to classes defined in this module; please see the
individual classes for details.

Parsing proceeds as follows:

1. The state machine examines each line of input, checking each of the
   transition patterns of the state `Body`, in order, looking for a match. The
   implicit transitions (blank lines and indentation) are checked before any
   others. The 'text' transition is a catch-all (matches anything).

2. The method associated with the matched transition pattern is called.

   A. Some transition methods are self-contained, appending elements to the
      document tree ('doctest' parses a doctest block). The parser's current
      line index is advanced to the end of the element, and parsing continues
      with step 1.

   B. Others trigger the creation of a nested state machine, whose job is to
      parse a compound construct ('indent' does a block quote, 'bullet' does a
      bullet list, 'overline' does a section [first checking for a valid
      section header]).

      - In the case of lists and explicit markup, a new state machine is
        created and run to parse the first item.

      - A new state machine is created and its initial state is set to the
        appropriate specialized state (`BulletList` in the case of the
        'bullet' transition). This state machine is run to parse the compound
        element (or series of explicit markup elements), and returns as soon
        as a non-member element is encountered. For example, the `BulletList`
        state machine aborts as soon as it encounters an element which is not
        a list item of that bullet list. The optional omission of
        inter-element blank lines is handled by the nested state machine.

      - The current line index is advanced to the end of the elements parsed,
        and parsing continues with step 1.

   C. The result of the 'text' transition depends on the next line of text.
      The current state is changed to `Text`, under which the second line is
      examined. If the second line is:

      - Indented: The element is a definition list item, and parsing proceeds
        similarly to step 2.B, using the `DefinitionList` state.

      - A line of uniform punctuation characters: The element is a section
        header; again, parsing proceeds as in step 2.B, and `Body` is still
        used.

      - Anything else: The element is a paragraph, which is examined for
        inline markup and appended to the parent element. Processing continues
        with step 1.
"""

__docformat__ = 'reStructuredText'


import sys, re, string
from docutils import nodes, statemachine, utils, roman, urischemes
from docutils.statemachine import StateMachineWS, StateWS
from docutils.utils import normname
import directives, languages
from tableparser import TableParser, TableMarkupError


class MarkupError(Exception): pass
class ParserError(Exception): pass


class Stuff:

    """Stores a bunch of stuff for dotted-attribute access."""

    def __init__(self, **keywordargs):
        self.__dict__.update(keywordargs)


class RSTStateMachine(StateMachineWS):

    """
    reStructuredText's master StateMachine.

    The entry point to reStructuredText parsing is the `run()` method.
    """

    def run(self, inputlines, docroot, inputoffset=0, matchtitles=1):
        """
        Parse `inputlines` and return a `docutils.nodes.document` instance.

        Extend `StateMachineWS.run()`: set up parse-global data, run the
        StateMachine, and return the resulting
        document.
        """
        self.language = languages.getlanguage(docroot.languagecode)
        self.matchtitles = matchtitles
        self.memo = Stuff(document=docroot,
                          reporter=docroot.reporter,
                          language=self.language,
                          titlestyles=[],
                          sectionlevel=0)
        self.node = docroot
        results = StateMachineWS.run(self, inputlines, inputoffset)
        assert results == [], 'RSTStateMachine.run() results should be empty.'
        self.node = self.memo = None    # remove unneeded references


class NestedStateMachine(StateMachineWS):

    """
    StateMachine run from within other StateMachine runs, to parse nested
    document structures.
    """

    def run(self, inputlines, inputoffset, memo, node, matchtitles=1):
        """
        Parse `inputlines` and populate a `docutils.nodes.document` instance.

        Extend `StateMachineWS.run()`: set up document-wide data.
        """
        self.matchtitles = matchtitles
        self.memo = memo
        self.node = node
        results = StateMachineWS.run(self, inputlines, inputoffset)
        assert results == [], 'NestedStateMachine.run() results should be empty'
        return results


class RSTState(StateWS):

    """
    reStructuredText State superclass.

    Contains methods used by all State subclasses.
    """

    nestedSM = NestedStateMachine

    def __init__(self, statemachine, debug=0):
        self.nestedSMkwargs = {'stateclasses': stateclasses,
                               'initialstate': 'Body'}
        StateWS.__init__(self, statemachine, debug)

    def gotoline(self, abslineoffset):
        """Jump to input line `abslineoffset`, ignoring jumps past the end."""
        try:
            self.statemachine.gotoline(abslineoffset)
        except IndexError:
            pass

    def bof(self, context):
        """Called at beginning of file."""
        return [], []

    def nestedparse(self, block, inputoffset, node, matchtitles=0,
                      statemachineclass=None, statemachinekwargs=None):
        """
        Create a new StateMachine rooted at `node` and run it over the input
        `block`.
        """
        if statemachineclass is None:
            statemachineclass = self.nestedSM
        if statemachinekwargs is None:
            statemachinekwargs = self.nestedSMkwargs
        statemachine = statemachineclass(debug=self.debug, **statemachinekwargs)
        statemachine.run(block, inputoffset, memo=self.statemachine.memo,
                         node=node, matchtitles=matchtitles)
        statemachine.unlink()
        return statemachine.abslineoffset()

    def nestedlistparse(self, block, inputoffset, node, initialstate,
                        blankfinish, blankfinishstate=None, extrasettings={},
                        matchtitles=0, statemachineclass=None,
                        statemachinekwargs=None):
        """
        Create a new StateMachine rooted at `node` and run it over the input
        `block`. Also keep track of optional intermdediate blank lines and the
        required final one.
        """
        if statemachineclass is None:
            statemachineclass = self.nestedSM
        if statemachinekwargs is None:
            statemachinekwargs = self.nestedSMkwargs.copy()
        statemachinekwargs['initialstate'] = initialstate
        statemachine = statemachineclass(debug=self.debug, **statemachinekwargs)
        if blankfinishstate is None:
            blankfinishstate = initialstate
        statemachine.states[blankfinishstate].blankfinish = blankfinish
        for key, value in extrasettings.items():
            setattr(statemachine.states[initialstate], key, value)
        statemachine.run(block, inputoffset, memo=self.statemachine.memo,
                         node=node, matchtitles=matchtitles)
        blankfinish = statemachine.states[blankfinishstate].blankfinish
        statemachine.unlink()
        return statemachine.abslineoffset(), blankfinish

    def section(self, title, source, style, lineno):
        """
        When a new section is reached that isn't a subsection of the current
        section, back up the line count (use previousline(-x)), then raise
        EOFError. The current StateMachine will finish, then the calling
        StateMachine can re-examine the title. This will work its way back up
        the calling chain until the correct section level isreached.

        Alternative: Evaluate the title, store the title info & level, and
        back up the chain until that level is reached. Store in memo? Or
        return in results?
        """
        if self.checksubsection(source, style, lineno):
            self.newsubsection(title, lineno)

    def checksubsection(self, source, style, lineno):
        """
        Check for a valid subsection header. Return 1 (true) or None (false).

        :Exception: `EOFError` when a sibling or supersection encountered.
        """
        memo = self.statemachine.memo
        titlestyles = memo.titlestyles
        mylevel = memo.sectionlevel
        try:                            # check for existing title style
            level = titlestyles.index(style) + 1
        except ValueError:              # new title style
            if len(titlestyles) == memo.sectionlevel: # new subsection
                titlestyles.append(style)
                return 1
            else:                       # not at lowest level
                self.statemachine.node += self.titleinconsistent(source, lineno)
                return None
        if level <= mylevel:            # sibling or supersection
            memo.sectionlevel = level   # bubble up to parent section
            # back up 2 lines for underline title, 3 for overline title
            self.statemachine.previousline(len(style) + 1)
            raise EOFError              # let parent section re-evaluate
        if level == mylevel + 1:        # immediate subsection
            return 1
        else:                           # invalid subsection
            self.statemachine.node += self.titleinconsistent(source, lineno)
            return None

    def titleinconsistent(self, sourcetext, lineno):
        literalblock = nodes.literal_block('', sourcetext)
        error = self.statemachine.memo.reporter.severe(
              'Title level inconsistent at line %s:' % lineno, '', literalblock)
        return error

    def newsubsection(self, title, lineno):
        """Append new subsection to document tree. On return, check level."""
        memo = self.statemachine.memo
        mylevel = memo.sectionlevel
        memo.sectionlevel += 1
        sectionnode = nodes.section()
        self.statemachine.node += sectionnode
        textnodes, messages = self.inline_text(title, lineno)
        titlenode = nodes.title(title, '', *textnodes)
        name = normname(titlenode.astext())
        sectionnode['name'] = name
        sectionnode += titlenode
        sectionnode += messages
        memo.document.note_implicit_target(sectionnode, sectionnode)
        offset = self.statemachine.lineoffset + 1
        absoffset = self.statemachine.abslineoffset() + 1
        newabsoffset = self.nestedparse(
              self.statemachine.inputlines[offset:], inputoffset=absoffset,
              node=sectionnode, matchtitles=1)
        self.gotoline(newabsoffset)
        if memo.sectionlevel <= mylevel: # can't handle next section?
            raise EOFError              # bubble up to supersection
        # reset sectionlevel; next pass will detect it properly
        memo.sectionlevel = mylevel

    def paragraph(self, lines, lineno):
        """
        Return a list (paragraph & messages) and a boolean: literal_block next?
        """
        data = '\n'.join(lines).rstrip()
        if data[-2:] == '::':
            if len(data) == 2:
                return [], 1
            elif data[-3] == ' ':
                text = data[:-3].rstrip()
            else:
                text = data[:-1]
            literalnext = 1
        else:
            text = data
            literalnext = 0
        textnodes, messages = self.inline_text(text, lineno)
        p = nodes.paragraph(data, '', *textnodes)
        return [p] + messages, literalnext

    inline = Stuff()
    """Patterns and constants used for inline markup recognition."""

    inline.openers = '\'"([{<'
    inline.closers = '\'")]}>'
    inline.start_string_prefix = (r'(?:(?<=^)|(?<=[ \n%s]))'
                                  % re.escape(inline.openers))
    inline.end_string_suffix = (r'(?:(?=$)|(?=[- \n.,:;!?%s]))'
                                % re.escape(inline.closers))
    inline.non_whitespace_before = r'(?<![ \n])'
    inline.non_whitespace_escape_before = r'(?<![ \n\x00])'
    inline.non_whitespace_after = r'(?![ \n])'
    inline.simplename = r'[a-zA-Z0-9](?:[-_.a-zA-Z0-9]*[a-zA-Z0-9])?'
    inline.uric = r"""[-_.!~*'()[\];/:@&=+$,%a-zA-Z0-9]"""
    inline.urilast = r"""[_~/\]a-zA-Z0-9]"""
    inline.emailc = r"""[-_!~*'{|}/#?^`&=+$%a-zA-Z0-9]"""
    inline.identity = string.maketrans('', '')
    inline.null2backslash = string.maketrans('\x00', '\\')
    inline.patterns = Stuff(
          initial=re.compile(r"""
                             %s             # start-string prefix
                             (
                               (              # start-strings only (group 2):
                                   \*\*         # strong
                                 |
                                   \*           # emphasis
                                   (?!\*)         # but not strong
                                 |
                                   ``           # literal
                                 |
                                   _`           # inline hyperlink target
                                 |
                                   \|           # substitution_reference start
                               )
                               %s             # no whitespace after
                             |              # *OR*
                               (              # whole constructs (group 3):
                                   (%s)         # reference name (4)
                                   (__?)        # end-string (5)
                                 |
                                   \[           # footnote_reference or
                                                # citation_reference start
                                   (            # label (group 6):
                                       [0-9]+     # manually numbered
                                     |          # *OR*
                                       \#(?:%s)?  # auto-numbered (w/ label?)
                                     |          # *OR*
                                       \*         # auto-symbol
                                     |          # *OR*
                                       (%s)       # citation reference (group 7)
                                   )
                                   (\]_)        # end-string (group 8)
                               )
                               %s             # end-string suffix
                             |              # *OR*
                               ((?::%s:)?)    # optional role (group 9)
                               (              # start-string (group 10)
                                 `              # interpreted text
                                                # or phrase reference
                                 (?!`)          # but not literal
                               )
                               %s             # no whitespace after
                             )
                             """ % (inline.start_string_prefix,
                                    inline.non_whitespace_after,
                                    inline.simplename,
                                    inline.simplename,
                                    inline.simplename,
                                    inline.end_string_suffix,
                                    inline.simplename,
                                    inline.non_whitespace_after,),
                             re.VERBOSE),
          emphasis=re.compile(inline.non_whitespace_escape_before
                              + r'(\*)' + inline.end_string_suffix),
          strong=re.compile(inline.non_whitespace_escape_before
                            + r'(\*\*)' + inline.end_string_suffix),
          interpreted_or_phrase_ref=re.compile(
                '%s(`(:%s:|__?)?)%s' % (inline.non_whitespace_escape_before,
                                        inline.simplename,
                                        inline.end_string_suffix)),
          literal=re.compile(inline.non_whitespace_before + '(``)'
                             + inline.end_string_suffix),
          target=re.compile(inline.non_whitespace_escape_before
                            + r'(`)' + inline.end_string_suffix),
          substitution_ref=re.compile(inline.non_whitespace_escape_before
                                      + r'(\|_{0,2})'
                                      + inline.end_string_suffix),
          uri=re.compile(
                r"""
                %s                          # start-string prefix
                (
                  (                           # absolute URI (group 2)
                    (                           # scheme (http, ftp, mailto)
                      [a-zA-Z][a-zA-Z0-9.+-]*     # (group 3)
                    )
                    :
                    (?:
                      (?:                         # either:
                        (?://?)?                    # hierarchical URI
                        %s*                         # URI characters
                        %s                          # final URI char
                      )
                      (?:                         # optional query
                        \?%s*                       # URI characters
                        %s                          # final URI char
                      )?
                      (?:                         # optional fragment
                        \#%s*                       # URI characters
                        %s                          # final URI char
                      )?
                    )
                  )
                |                           # *OR*
                  (                           # email address (group 4)
                    %s+(?:\.%s+)*               # name
                    @                           # at
                    %s+(?:\.%s*)*               # host
                    %s                          # final URI char
                  )
                )
                %s                          # end-string suffix
                """ % (inline.start_string_prefix,
                       inline.uric, inline.urilast,
                       inline.uric, inline.urilast,
                       inline.uric, inline.urilast,
                       inline.emailc, inline.emailc,
                       inline.emailc, inline.emailc,
                       inline.urilast,
                       inline.end_string_suffix,),
                re.VERBOSE))
    inline.groups = Stuff(initial=Stuff(start=2, whole=3, refname=4, refend=5,
                                        footnotelabel=6, citationlabel=7,
                                        fnend=8, role=9, backquote=10),
                          interpreted_or_phrase_ref=Stuff(suffix=2),
                          uri=Stuff(whole=1, absolute=2, scheme=3, email=4))

    def quotedstart(self, match):
        """Return 1 if inline markup start-string is 'quoted', 0 if not."""
        string = match.string
        start = match.start()
        end = match.end()
        if start == 0:                  # start-string at beginning of text
            return 0
        prestart = string[start - 1]
        try:
            poststart = string[end]
            if self.inline.openers.index(prestart) \
                  == self.inline.closers.index(poststart):   # quoted
                return 1
        except IndexError:              # start-string at end of text
            return 1
        except ValueError:              # not quoted
            pass
        return 0

    def inlineobj(self, match, lineno, pattern, nodeclass,
                  restorebackslashes=0):
        string = match.string
        matchstart = match.start(self.inline.groups.initial.start)
        matchend = match.end(self.inline.groups.initial.start)
        if self.quotedstart(match):
            return (string[:matchend], [], string[matchend:], [], '')
        endmatch = pattern.search(string[matchend:])
        if endmatch and endmatch.start(1):  # 1 or more chars
            text = unescape(endmatch.string[:endmatch.start(1)],
                            restorebackslashes)
            rawsource = unescape(string[matchstart:matchend+endmatch.end(1)], 1)
            return (string[:matchstart], [nodeclass(rawsource, text)],
                    string[matchend:][endmatch.end(1):], [], endmatch.group(1))
        msg = self.statemachine.memo.reporter.warning(
              'Inline %s start-string without end-string '
              'at line %s.' % (nodeclass.__name__, lineno))
        text = unescape(string[matchstart:matchend], 1)
        rawsource = unescape(string[matchstart:matchend], 1)
        prb = self.problematic(text, rawsource, msg)
        return string[:matchstart], [prb], string[matchend:], [msg], ''

    def problematic(self, text, rawsource, message):
        msgid = self.statemachine.memo.document.set_id(message,
                                                       self.statemachine.node)
        problematic = nodes.problematic(rawsource, text, refid=msgid)
        prbid = self.statemachine.memo.document.set_id(problematic)
        message.add_backref(prbid)
        return problematic

    def emphasis(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inlineobj(
              match, lineno, self.inline.patterns.emphasis, nodes.emphasis)
        return before, inlines, remaining, sysmessages

    def strong(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inlineobj(
              match, lineno, self.inline.patterns.strong, nodes.strong)
        return before, inlines, remaining, sysmessages

    def interpreted_or_phrase_ref(self, match, lineno):
        pattern = self.inline.patterns.interpreted_or_phrase_ref
        rolegroup = self.inline.groups.initial.role
        backquote = self.inline.groups.initial.backquote
        string = match.string
        matchstart = match.start(backquote)
        matchend = match.end(backquote)
        rolestart = match.start(rolegroup)
        role = match.group(rolegroup)
        position = ''
        if role:
            role = role[1:-1]
            position = 'prefix'
        elif self.quotedstart(match):
            return (string[:matchend], [], string[matchend:], [])
        endmatch = pattern.search(string[matchend:])
        if endmatch and endmatch.start(1):  # 1 or more chars
            escaped = endmatch.string[:endmatch.start(1)]
            text = unescape(escaped, 0)
            rawsource = unescape(
                  string[match.start():matchend+endmatch.end()], 1)
            if rawsource[-1:] == '_':
                if role:
                    msg = self.statemachine.memo.reporter.warning(
                          'Mismatch: inline interpreted text start-string and '
                          'role with phrase-reference end-string at line %s.'
                          % lineno)
                    text = unescape(string[matchstart:matchend], 1)
                    rawsource = unescape(string[matchstart:matchend], 1)
                    prb = self.problematic(text, rawsource, msg)
                    return (string[:matchstart], [prb], string[matchend:],
                            [msg])
                return self.phrase_ref(
                      string[:matchstart], string[matchend:][endmatch.end():],
                      text, rawsource)
            else:
                return self.interpreted(
                      string[:rolestart], string[matchend:][endmatch.end():],
                      endmatch, role, position, lineno,
                      escaped, rawsource, text)
        msg = self.statemachine.memo.reporter.warning(
              'Inline interpreted text or phrase reference start-string '
              'without end-string at line %s.' % lineno)
        text = unescape(string[matchstart:matchend], 1)
        rawsource = unescape(string[matchstart:matchend], 1)
        prb = self.problematic(text, rawsource, msg)
        return string[:matchstart], [prb], string[matchend:], [msg]

    def phrase_ref(self, before, after, text, rawsource):
        refname = normname(text)
        reference = nodes.reference(rawsource, text)
        if rawsource[-2:] == '__':
            reference['anonymous'] = 1
            self.statemachine.memo.document.note_anonymous_ref(reference)
        else:
            reference['refname'] = refname
            self.statemachine.memo.document.note_refname(reference)
        return before, [reference], after, []

    def interpreted(self, before, after, endmatch, role, position, lineno,
                    escaped, rawsource, text):
        suffix = self.inline.groups.interpreted_or_phrase_ref.suffix
        if endmatch.group(suffix):
            if role:
                msg = self.statemachine.memo.reporter.warning(
                      'Multiple roles in interpreted text at line %s.'
                      % lineno)
                return (before + rawsource, [], after, [msg])
            role = endmatch.group(suffix)[1:-1]
            position = 'suffix'
        if role:
            atts = {'role': role, 'position': position}
        else:
            atts = {}
        return before, [nodes.interpreted(rawsource, text, **atts)], after, []

    def literal(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inlineobj(
              match, lineno, self.inline.patterns.literal, nodes.literal,
              restorebackslashes=1)
        return before, inlines, remaining, sysmessages

    def inline_target(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inlineobj(
              match, lineno, self.inline.patterns.target, nodes.target)
        if inlines and isinstance(inlines[0], nodes.target):
            assert len(inlines) == 1
            target = inlines[0]
            name = normname(target.astext())
            target['name'] = name
            self.statemachine.memo.document.note_explicit_target(
                  target, self.statemachine.node)
        return before, inlines, remaining, sysmessages

    def substitution_reference(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inlineobj(
              match, lineno, self.inline.patterns.substitution_ref,
              nodes.substitution_reference)
        if inlines:
            assert len(inlines) == 1
            subrefnode = inlines[0]
            assert isinstance(subrefnode, nodes.substitution_reference)
            subreftext = subrefnode.astext()
            refname = normname(subreftext)
            subrefnode['refname'] = refname
            self.statemachine.memo.document.note_substitution_ref(subrefnode)
            if endstring[-1:] == '_':
                referencenode = nodes.reference('|%s%s' % (subreftext, endstring), '')
                if endstring[-2:] == '__':
                    referencenode['anonymous'] = 1
                    self.statemachine.memo.document.note_anonymous_ref(referencenode)
                else:
                    referencenode['refname'] = refname
                    self.statemachine.memo.document.note_refname(referencenode)
                referencenode += subrefnode
                inlines = [referencenode]
        return before, inlines, remaining, sysmessages

    def footnote_reference(self, match, lineno):
        """
        Handles `nodes.footnote_reference` and `nodes.citation_reference`
        elements.
        """
        label = match.group(self.inline.groups.initial.footnotelabel)
        refname = normname(label)
        if match.group(self.inline.groups.initial.citationlabel):
            refnode = nodes.citation_reference('[%s]_' % label, refname=refname)
            refnode += nodes.Text(label)
            self.statemachine.memo.document.note_citation_ref(refnode)
        else:
            refnode = nodes.footnote_reference('[%s]_' % label)
            if refname[0] == '#':
                refname = refname[1:]
                refnode['auto'] = 1
                self.statemachine.memo.document.note_autofootnote_ref(refnode)
            elif refname == '*':
                refname = ''
                refnode['auto'] = '*'
                self.statemachine.memo.document.note_symbol_footnote_ref(
                      refnode)
            else:
                refnode += nodes.Text(label)
            if refname:
                refnode['refname'] = refname
                self.statemachine.memo.document.note_footnote_ref(refnode)
        string = match.string
        matchstart = match.start(self.inline.groups.initial.whole)
        matchend = match.end(self.inline.groups.initial.whole)
        return (string[:matchstart], [refnode], string[matchend:], [])

    def reference(self, match, lineno, anonymous=None):
        referencename = match.group(self.inline.groups.initial.refname)
        refname = normname(referencename)
        referencenode = nodes.reference(
              referencename + match.group(self.inline.groups.initial.refend),
              referencename)
        if anonymous:
            referencenode['anonymous'] = 1
            self.statemachine.memo.document.note_anonymous_ref(referencenode)
        else:
            referencenode['refname'] = refname
            self.statemachine.memo.document.note_refname(referencenode)
        string = match.string
        matchstart = match.start(self.inline.groups.initial.whole)
        matchend = match.end(self.inline.groups.initial.whole)
        return (string[:matchstart], [referencenode], string[matchend:], [])

    def anonymous_reference(self, match, lineno):
        return self.reference(match, lineno, anonymous=1)

    def standalone_uri(self, text, lineno):
        pattern = self.inline.patterns.uri
        whole = self.inline.groups.uri.whole
        scheme = self.inline.groups.uri.scheme
        email = self.inline.groups.uri.email
        remainder = text
        textnodes = []
        start = 0
        while 1:
            match = pattern.search(remainder, start)
            if match:
                if not match.group(scheme) or \
                      urischemes.schemes.has_key(match.group(scheme).lower()):
                    if match.start(whole) > 0:
                        textnodes.append(nodes.Text(unescape(
                              remainder[:match.start(whole)])))
                    if match.group(email):
                        addscheme = 'mailto:'
                    else:
                        addscheme = ''
                    text = match.group(whole)
                    unescaped = unescape(text, 0)
                    textnodes.append(
                          nodes.reference(unescape(text, 1), unescaped,
                                          refuri=addscheme + unescaped))
                    remainder = remainder[match.end(whole):]
                    start = 0
                else:                   # not a valid scheme
                    start = match.end(whole)
            else:
                if remainder:
                    textnodes.append(nodes.Text(unescape(remainder)))
                break
        return textnodes

    inline.dispatch = {'*': emphasis,
                       '**': strong,
                       '`': interpreted_or_phrase_ref,
                       '``': literal,
                       '_`': inline_target,
                       ']_': footnote_reference,
                       '|': substitution_reference,
                       '_': reference,
                       '__': anonymous_reference}

    def inline_text(self, text, lineno):
        """
        Return 2 lists: nodes (text and inline elements), and system_messages.

        Using a `pattern` matching start-strings (for emphasis, strong,
        interpreted, phrase reference, literal, substitution reference, and
        inline target) or complete constructs (simple reference, footnote
        reference) we search for a candidate. When one is found, we check for
        validity (e.g., not a quoted '*' character). If valid, search for the
        corresponding end string if applicable, and check for validity. If not
        found or invalid, generate a warning and ignore the start-string.
        Standalone hyperlinks are found last.
        """
        pattern = self.inline.patterns.initial
        dispatch = self.inline.dispatch
        start = self.inline.groups.initial.start - 1
        backquote = self.inline.groups.initial.backquote - 1
        refend = self.inline.groups.initial.refend - 1
        fnend = self.inline.groups.initial.fnend - 1
        remaining = escape2null(text)
        processed = []
        unprocessed = []
        messages = []
        while remaining:
            match = pattern.search(remaining)
            if match:
                groups = match.groups()
                before, inlines, remaining, sysmessages = \
                      dispatch[groups[start] or groups[backquote]
                               or groups[refend]
                               or groups[fnend]](self, match, lineno)
                unprocessed.append(before)
                messages += sysmessages
                if inlines:
                    processed += self.standalone_uri(''.join(unprocessed),
                                                     lineno)
                    processed += inlines
                    unprocessed = []
            else:
                break
        remaining = ''.join(unprocessed) + remaining
        if remaining:
            processed += self.standalone_uri(remaining, lineno)
        return processed, messages

    def unindentwarning(self):
        return self.statemachine.memo.reporter.warning(
              ('Unindent without blank line at line %s.'
                  % (self.statemachine.abslineno() + 1)))


class Body(RSTState):

    """
    Generic classifier of the first line of a block.
    """

    enum = Stuff()
    """Enumerated list parsing information."""

    enum.formatinfo = {
          'parens': Stuff(prefix='(', suffix=')', start=1, end=-1),
          'rparen': Stuff(prefix='', suffix=')', start=0, end=-1),
          'period': Stuff(prefix='', suffix='.', start=0, end=-1)}
    enum.formats = enum.formatinfo.keys()
    enum.sequences = ['arabic', 'loweralpha', 'upperalpha',
                      'lowerroman', 'upperroman'] # ORDERED!
    enum.sequencepats = {'arabic': '[0-9]+',
                         'loweralpha': '[a-z]',
                         'upperalpha': '[A-Z]',
                         'lowerroman': '[ivxlcdm]+',
                         'upperroman': '[IVXLCDM]+',}
    enum.converters = {'arabic': int,
                       'loweralpha':
                       lambda s, zero=(ord('a')-1): ord(s) - zero,
                       'upperalpha':
                       lambda s, zero=(ord('A')-1): ord(s) - zero,
                       'lowerroman':
                       lambda s: roman.fromRoman(s.upper()),
                       'upperroman': roman.fromRoman}

    enum.sequenceregexps = {}
    for sequence in enum.sequences:
        enum.sequenceregexps[sequence] = re.compile(enum.sequencepats[sequence]
                                                + '$')

    tabletoppat = re.compile(r'\+-[-+]+-\+ *$')
    """Matches the top (& bottom) of a table)."""

    tableparser = TableParser()

    pats = {}
    """Fragments of patterns used by transitions."""

    pats['nonalphanum7bit'] = '[!-/:-@[-`{-~]'
    pats['alpha'] = '[a-zA-Z]'
    pats['alphanum'] = '[a-zA-Z0-9]'
    pats['alphanumplus'] = '[a-zA-Z0-9_-]'
    pats['enum'] = ('(%(arabic)s|%(loweralpha)s|%(upperalpha)s|%(lowerroman)s'
                    '|%(upperroman)s)' % enum.sequencepats)
    pats['optname'] = '%(alphanum)s%(alphanumplus)s*' % pats
    pats['optarg'] = '%(alpha)s%(alphanumplus)s*' % pats
    pats['option'] = r'(--?|\+|/)%(optname)s([ =]%(optarg)s)?' % pats

    for format in enum.formats:
        pats[format] = '(?P<%s>%s%s%s)' % (
              format, re.escape(enum.formatinfo[format].prefix),
              pats['enum'], re.escape(enum.formatinfo[format].suffix))

    patterns = {'bullet': r'[-+*]( +|$)',
                'enumerator': r'(%(parens)s|%(rparen)s|%(period)s)( +|$)'
                % pats,
                'field_marker': r':[^: ]([^:]*[^: ])?:( +|$)',
                'option_marker': r'%(option)s(, %(option)s)*(  +| ?$)' % pats,
                'doctest': r'>>>( +|$)',
                'tabletop': tabletoppat,
                'explicit_markup': r'\.\.( +|$)',
                'anonymous': r'__( +|$)',
                'line': r'(%(nonalphanum7bit)s)\1\1\1+ *$' % pats,
                #'rfc822': r'[!-9;-~]+:( +|$)',
                'text': r''}
    initialtransitions = ['bullet',
                          'enumerator',
                          'field_marker',
                          'option_marker',
                          'doctest',
                          'tabletop',
                          'explicit_markup',
                          'anonymous',
                          'line',
                          'text']

    def indent(self, match, context, nextstate):
        """Block quote."""
        indented, indent, lineoffset, blankfinish = \
              self.statemachine.getindented()
        blockquote = self.block_quote(indented, lineoffset)
        self.statemachine.node += blockquote
        if not blankfinish:
            self.statemachine.node += self.unindentwarning()
        return context, nextstate, []

    def block_quote(self, indented, lineoffset):
        blockquote = nodes.block_quote()
        self.nestedparse(indented, lineoffset, blockquote)
        return blockquote

    def bullet(self, match, context, nextstate):
        """Bullet list item."""
        bulletlist = nodes.bullet_list()
        self.statemachine.node += bulletlist
        bulletlist['bullet'] = match.string[0]
        i, blankfinish = self.list_item(match.end())
        bulletlist += i
        offset = self.statemachine.lineoffset + 1   # next line
        newlineoffset, blankfinish = self.nestedlistparse(
              self.statemachine.inputlines[offset:],
              inputoffset=self.statemachine.abslineoffset() + 1,
              node=bulletlist, initialstate='BulletList',
              blankfinish=blankfinish)
        if not blankfinish:
            self.statemachine.node += self.unindentwarning()
        self.gotoline(newlineoffset)
        return [], nextstate, []

    def list_item(self, indent):
        indented, lineoffset, blankfinish = \
              self.statemachine.getknownindented(indent)
        listitem = nodes.list_item('\n'.join(indented))
        if indented:
            self.nestedparse(indented, inputoffset=lineoffset, node=listitem)
        return listitem, blankfinish

    def enumerator(self, match, context, nextstate):
        """Enumerated List Item"""
        format, sequence, text, ordinal = self.parse_enumerator(match)
        if ordinal is None:
            msg = self.statemachine.memo.reporter.error(
                  ('Enumerated list start value invalid at line %s: '
                   '%r (sequence %r)' % (self.statemachine.abslineno(),
                                         text, sequence)))
            self.statemachine.node += msg
            indented, lineoffset, blankfinish = \
                  self.statemachine.getknownindented(match.end())
            bq = self.block_quote(indented, lineoffset)
            self.statemachine.node += bq
            if not blankfinish:
                self.statemachine.node += self.unindentwarning()
            return [], nextstate, []
        if ordinal != 1:
            msg = self.statemachine.memo.reporter.info(
                  ('Enumerated list start value not ordinal-1 at line %s: '
                      '%r (ordinal %s)' % (self.statemachine.abslineno(),
                                           text, ordinal)))
            self.statemachine.node += msg
        enumlist = nodes.enumerated_list()
        self.statemachine.node += enumlist
        enumlist['enumtype'] = sequence
        if ordinal != 1:
            enumlist['start'] = ordinal
        enumlist['prefix'] = self.enum.formatinfo[format].prefix
        enumlist['suffix'] = self.enum.formatinfo[format].suffix
        listitem, blankfinish = self.list_item(match.end())
        enumlist += listitem
        offset = self.statemachine.lineoffset + 1   # next line
        newlineoffset, blankfinish = self.nestedlistparse(
              self.statemachine.inputlines[offset:],
              inputoffset=self.statemachine.abslineoffset() + 1,
              node=enumlist, initialstate='EnumeratedList',
              blankfinish=blankfinish,
              extrasettings={'lastordinal': ordinal, 'format': format})
        if not blankfinish:
            self.statemachine.node += self.unindentwarning()
        self.gotoline(newlineoffset)
        return [], nextstate, []

    def parse_enumerator(self, match, expectedsequence=None):
        """
        Analyze an enumerator and return the results.

        :Return:
            - the enumerator format ('period', 'parens', or 'rparen'),
            - the sequence used ('arabic', 'loweralpha', 'upperroman', etc.),
            - the text of the enumerator, stripped of formatting, and
            - the ordinal value of the enumerator ('a' -> 1, 'ii' -> 2, etc.;
              ``None`` is returned for invalid enumerator text).

        The enumerator format has already been determined by the regular
        expression match. If `expectedsequence` is given, that sequence is
        tried first. If not, we check for Roman numeral 1. This way,
        single-character Roman numerals (which are also alphabetical) can be
        matched. If no sequence has been matched, all sequences are checked in
        order.
        """
        groupdict = match.groupdict()
        sequence = ''
        for format in self.enum.formats:
            if groupdict[format]:       # was this the format matched?
                break                   # yes; keep `format`
        else:                           # shouldn't happen
            raise ParserError, 'enumerator format not matched'
        text = groupdict[format][self.enum.formatinfo[format].start
                                 :self.enum.formatinfo[format].end]
        if expectedsequence:
            try:
                if self.enum.sequenceregexps[expectedsequence].match(text):
                    sequence = expectedsequence
            except KeyError:            # shouldn't happen
                raise ParserError, 'unknown sequence: %s' % sequence
        else:
            if text == 'i':
                sequence = 'lowerroman'
            elif text == 'I':
                sequence = 'upperroman'
        if not sequence:
            for sequence in self.enum.sequences:
                if self.enum.sequenceregexps[sequence].match(text):
                    break
            else:                       # shouldn't happen
                raise ParserError, 'enumerator sequence not matched'
        try:
            ordinal = self.enum.converters[sequence](text)
        except roman.InvalidRomanNumeralError:
            ordinal = None
        return format, sequence, text, ordinal

    def field_marker(self, match, context, nextstate):
        """Field list item."""
        fieldlist = nodes.field_list()
        self.statemachine.node += fieldlist
        field, blankfinish = self.field(match)
        fieldlist += field
        offset = self.statemachine.lineoffset + 1   # next line
        newlineoffset, blankfinish = self.nestedlistparse(
              self.statemachine.inputlines[offset:],
              inputoffset=self.statemachine.abslineoffset() + 1,
              node=fieldlist, initialstate='FieldList',
              blankfinish=blankfinish)
        if not blankfinish:
            self.statemachine.node += self.unindentwarning()
        self.gotoline(newlineoffset)
        return [], nextstate, []

    def field(self, match):
        name, args = self.parse_field_marker(match)
        indented, indent, lineoffset, blankfinish = \
              self.statemachine.getfirstknownindented(match.end())
        fieldnode = nodes.field()
        fieldnode += nodes.field_name(name, name)
        for arg in args:
            fieldnode += nodes.field_argument(arg, arg)
        fieldbody = nodes.field_body('\n'.join(indented))
        fieldnode += fieldbody
        if indented:
            self.nestedparse(indented, inputoffset=lineoffset, node=fieldbody)
        return fieldnode, blankfinish

    def parse_field_marker(self, match):
        """Extract & return name & argument list from a field marker match."""
        field = match.string[1:]        # strip off leading ':'
        field = field[:field.find(':')] # strip off trailing ':' etc.
        tokens = field.split()
        return tokens[0], tokens[1:]    # first == name, others == args

    def option_marker(self, match, context, nextstate):
        """Option list item."""
        optionlist = nodes.option_list()
        try:
            listitem, blankfinish = self.option_list_item(match)
        except MarkupError, detail:     # shouldn't happen; won't match pattern
            msg = self.statemachine.memo.reporter.error(
                  ('Invalid option list marker at line %s: %s'
                      % (self.statemachine.abslineno(), detail)))
            self.statemachine.node += msg
            indented, indent, lineoffset, blankfinish = \
                  self.statemachine.getfirstknownindented(match.end())
            blockquote = self.block_quote(indented, lineoffset)
            self.statemachine.node += blockquote
            if not blankfinish:
                self.statemachine.node += self.unindentwarning()
            return [], nextstate, []
        self.statemachine.node += optionlist
        optionlist += listitem
        offset = self.statemachine.lineoffset + 1   # next line
        newlineoffset, blankfinish = self.nestedlistparse(
              self.statemachine.inputlines[offset:],
              inputoffset=self.statemachine.abslineoffset() + 1,
              node=optionlist, initialstate='OptionList',
              blankfinish=blankfinish)
        if not blankfinish:
            self.statemachine.node += self.unindentwarning()
        self.gotoline(newlineoffset)
        return [], nextstate, []

    def option_list_item(self, match):
        options = self.parse_option_marker(match)
        indented, indent, lineoffset, blankfinish = \
              self.statemachine.getfirstknownindented(match.end())
        if not indented:                # not an option list item
            raise statemachine.TransitionCorrection('text')
        option_group = nodes.option_group('', *options)
        description = nodes.description('\n'.join(indented))
        option_list_item = nodes.option_list_item('', option_group, description)
        if indented:
            self.nestedparse(indented, inputoffset=lineoffset, node=description)
        return option_list_item, blankfinish

    def parse_option_marker(self, match):
        """
        Return a list of `node.option` and `node.option_argument` objects,
        parsed from an option marker match.

        :Exception: `MarkupError` for invalid option markers.
        """
        optlist = []
        optionstrings = match.group().rstrip().split(', ')
        for optionstring in optionstrings:
            tokens = optionstring.split()
            delimiter = ' '
            firstopt = tokens[0].split('=')
            if len(firstopt) > 1:
                tokens[:1] = firstopt
                delimiter = '='
            if 0 < len(tokens) <= 2:
                option = nodes.option(optionstring)
                option += nodes.option_string(tokens[0], tokens[0])
                if len(tokens) > 1:
                    option += nodes.option_argument(tokens[1], tokens[1],
                                                    delimiter=delimiter)
                optlist.append(option)
            else:
                raise MarkupError('wrong numer of option tokens (=%s), '
                                  'should be 1 or 2: %r' % (len(tokens),
                                                            optionstring))
        return optlist

    def doctest(self, match, context, nextstate):
        data = '\n'.join(self.statemachine.gettextblock())
        self.statemachine.node += nodes.doctest_block(data, data)
        return [], nextstate, []

    def tabletop(self, match, context, nextstate):
        """Top border of a table."""
        nodelist, blankfinish = self.table()
        self.statemachine.node += nodelist
        if not blankfinish:
            msg = self.statemachine.memo.reporter.warning(
                  'Blank line required after table at line %s.'
                  % (self.statemachine.abslineno() + 1))
            self.statemachine.node += msg
        return [], nextstate, []

    def table(self):
        """Parse a table."""
        block, messages, blankfinish = self.isolatetable()
        if block:
            try:
                tabledata = self.tableparser.parse(block)
                tableline = self.statemachine.abslineno() - len(block) + 1
                table = self.buildtable(tabledata, tableline)
                nodelist = [table] + messages
            except TableMarkupError, detail:
                nodelist = self.malformedtable(block, str(detail)) + messages
        else:
            nodelist = messages
        return nodelist, blankfinish

    def isolatetable(self):
        messages = []
        blankfinish = 1
        try:
            block = self.statemachine.getunindented()
        except statemachine.UnexpectedIndentationError, instance:
            block, lineno = instance.args
            messages.append(self.statemachine.memo.reporter.error(
                  'Unexpected indentation at line %s.' % lineno))
            blankfinish = 0
        width = len(block[0].strip())
        for i in range(len(block)):
            block[i] = block[i].strip()
            if block[i][0] not in '+|': # check left edge
                blankfinish = 0
                self.statemachine.previousline(len(block) - i)
                del block[i:]
                break
        if not self.tabletoppat.match(block[-1]): # find bottom
            blankfinish = 0
            # from second-last to third line of table:
            for i in range(len(block) - 2, 1, -1):
                if self.tabletoppat.match(block[i]):
                    self.statemachine.previousline(len(block) - i + 1)
                    del block[i+1:]
                    break
            else:
                messages.extend(self.malformedtable(block))
                return [], messages, blankfinish
        for i in range(len(block)):     # check right edge
            if len(block[i]) != width or block[i][-1] not in '+|':
                messages.extend(self.malformedtable(block))
                return [], messages, blankfinish
        return block, messages, blankfinish

    def malformedtable(self, block, detail=''):
        data = '\n'.join(block)
        message = 'Malformed table at line %s; formatting as a ' \
                  'literal block.' % (self.statemachine.abslineno()
                                      - len(block) + 1)
        if detail:
            message += '\n' + detail
        nodelist = [self.statemachine.memo.reporter.error(message),
                    nodes.literal_block(data, data)]
        return nodelist

    def buildtable(self, tabledata, tableline):
        colspecs, headrows, bodyrows = tabledata
        table = nodes.table()
        tgroup = nodes.tgroup(cols=len(colspecs))
        table += tgroup
        for colspec in colspecs:
            tgroup += nodes.colspec(colwidth=colspec)
        if headrows:
            thead = nodes.thead()
            tgroup += thead
            for row in headrows:
                thead += self.buildtablerow(row, tableline)
        tbody = nodes.tbody()
        tgroup += tbody
        for row in bodyrows:
            tbody += self.buildtablerow(row, tableline)
        return table

    def buildtablerow(self, rowdata, tableline):
        row = nodes.row()
        for cell in rowdata:
            if cell is None:
                continue
            morerows, morecols, offset, cellblock = cell
            attributes = {}
            if morerows:
                attributes['morerows'] = morerows
            if morecols:
                attributes['morecols'] = morecols
            entry = nodes.entry(**attributes)
            row += entry
            if ''.join(cellblock):
                self.nestedparse(cellblock, inputoffset=tableline+offset,
                                 node=entry)
        return row


    explicit = Stuff()
    """Patterns and constants used for explicit markup recognition."""

    explicit.patterns = Stuff(
          target=re.compile(r"""
                            (?:
                              _           # anonymous target
                            |           # *OR*
                              (`?)        # optional open quote
                              (?![ `])    # first char. not space or backquote
                              (           # reference name
                                .+?
                              )
                              %s          # not whitespace or escape
                              \1          # close quote if open quote used
                            )
                            %s          # not whitespace or escape
                            :           # end of reference name
                            (?:[ ]+|$)  # followed by whitespace
                            """
                            % (RSTState.inline.non_whitespace_escape_before,
                               RSTState.inline.non_whitespace_escape_before),
                            re.VERBOSE),
          reference=re.compile(r"""
                               (?:
                                 (%s)_       # simple reference name
                               |           # *OR*
                                 `           # open backquote
                                 (?![ ])     # not space
                                 (.+?)       # hyperlink phrase
                                 %s          # not whitespace or escape
                                 `_          # close backquote & reference mark
                               )
                               $           # end of string
                               """ %
                               (RSTState.inline.simplename,
                                RSTState.inline.non_whitespace_escape_before,),
                               re.VERBOSE),
          substitution=re.compile(r"""
                                  (?:
                                    (?![ ])     # first char. not space
                                    (.+?)       # substitution text
                                    %s          # not whitespace or escape
                                    \|          # close delimiter
                                  )
                                  (?:[ ]+|$)    # followed by whitespace
                                  """ %
                                  RSTState.inline.non_whitespace_escape_before,
                                  re.VERBOSE),)
    explicit.groups = Stuff(
          target=Stuff(quote=1, name=2),
          reference=Stuff(simple=1, phrase=2),
          substitution=Stuff(name=1))

    def footnote(self, match):
        indented, indent, offset, blankfinish = \
              self.statemachine.getfirstknownindented(match.end())
        label = match.group(1)
        name = normname(label)
        footnote = nodes.footnote('\n'.join(indented))
        if name[0] == '#':              # auto-numbered
            name = name[1:]             # autonumber label
            footnote['auto'] = 1
            if name:
                footnote['name'] = name
            self.statemachine.memo.document.note_autofootnote(footnote)
        elif name == '*':               # auto-symbol
            name = ''
            footnote['auto'] = '*'
            self.statemachine.memo.document.note_symbol_footnote(footnote)
        else:                           # manually numbered
            footnote += nodes.label('', label)
            footnote['name'] = name
            self.statemachine.memo.document.note_footnote(footnote)
        if name:
            self.statemachine.memo.document.note_explicit_target(footnote,
                                                                 footnote)
        if indented:
            self.nestedparse(indented, inputoffset=offset, node=footnote)
        return [footnote], blankfinish

    def citation(self, match):
        indented, indent, offset, blankfinish = \
              self.statemachine.getfirstknownindented(match.end())
        label = match.group(1)
        name = normname(label)
        citation = nodes.citation('\n'.join(indented))
        citation += nodes.label('', label)
        citation['name'] = name
        self.statemachine.memo.document.note_citation(citation)
        self.statemachine.memo.document.note_explicit_target(citation, citation)
        if indented:
            self.nestedparse(indented, inputoffset=offset, node=citation)
        return [citation], blankfinish

    def hyperlink_target(self, match):
        pattern = self.explicit.patterns.target
        namegroup = self.explicit.groups.target.name
        lineno = self.statemachine.abslineno()
        block, indent, offset, blankfinish = \
              self.statemachine.getfirstknownindented(match.end(), uptoblank=1,
                                                      stripindent=0)
        blocktext = match.string[:match.end()] + '\n'.join(block)
        block = [escape2null(line) for line in block]
        escaped = block[0]
        blockindex = 0
        while 1:
            targetmatch = pattern.match(escaped)
            if targetmatch:
                break
            blockindex += 1
            try:
                escaped += block[blockindex]
            except (IndexError, MarkupError):
                raise MarkupError('malformed hyperlink target at line %s.'
                                  % lineno)
        del block[:blockindex]
        block[0] = (block[0] + ' ')[targetmatch.end()-len(escaped)-1:].strip()
        if block and block[-1].strip()[-1:] == '_': # possible indirect target
            reference = ' '.join([line.strip() for line in block])
            refname = self.isreference(reference)
            if refname:
                target = nodes.target(blocktext, '', refname=refname)
                self.addtarget(targetmatch.group(namegroup), '', target)
                self.statemachine.memo.document.note_indirect_target(target)
                return [target], blankfinish
        nodelist = []
        reference = ''.join([line.strip() for line in block])
        if reference.find(' ') != -1:
            warning = self.statemachine.memo.reporter.warning(
                  'Hyperlink target at line %s contains whitespace. '
                  'Perhaps a footnote was intended?'
                  % (self.statemachine.abslineno() - len(block) + 1), '',
                  nodes.literal_block(blocktext, blocktext))
            nodelist.append(warning)
        else:
            unescaped = unescape(reference)
            target = nodes.target(blocktext, '')
            self.addtarget(targetmatch.group(namegroup), unescaped, target)
            nodelist.append(target)
        return nodelist, blankfinish

    def isreference(self, reference):
        match = self.explicit.patterns.reference.match(normname(reference))
        if not match:
            return None
        return unescape(match.group(self.explicit.groups.reference.simple)
                        or match.group(self.explicit.groups.reference.phrase))

    def addtarget(self, targetname, refuri, target):
        if targetname:
            name = normname(unescape(targetname))
            target['name'] = name
            if refuri:
                target['refuri'] = refuri
                self.statemachine.memo.document.note_external_target(target)
            else:
                self.statemachine.memo.document.note_internal_target(target)
            self.statemachine.memo.document.note_explicit_target(
                  target, self.statemachine.node)
        else:                       # anonymous target
            if refuri:
                target['refuri'] = refuri
            target['anonymous'] = 1
            self.statemachine.memo.document.note_anonymous_target(target)

    def substitutiondef(self, match):
        pattern = self.explicit.patterns.substitution
        lineno = self.statemachine.abslineno()
        block, indent, offset, blankfinish = \
              self.statemachine.getfirstknownindented(match.end(),
                                                      stripindent=0)
        blocktext = (match.string[:match.end()] + '\n'.join(block))
        block = [escape2null(line) for line in block]
        escaped = block[0].rstrip()
        blockindex = 0
        while 1:
            subdefmatch = pattern.match(escaped)
            if subdefmatch:
                break
            blockindex += 1
            try:
                escaped = escaped + ' ' + block[blockindex].strip()
            except (IndexError, MarkupError):
                raise MarkupError('malformed substitution definition '
                                  'at line %s.' % lineno)
        del block[:blockindex]          # strip out the substitution marker
        block[0] = (block[0] + ' ')[subdefmatch.end()-len(escaped)-1:].strip()
        if not block[0]:
            del block[0]
            offset += 1
        subname = subdefmatch.group(self.explicit.groups.substitution.name)
        name = normname(subname)
        substitutionnode = nodes.substitution_definition(
              blocktext, name=name, alt=subname)
        if block:
            block[0] = block[0].strip()
            newabsoffset, blankfinish = self.nestedlistparse(
                  block, inputoffset=offset, node=substitutionnode,
                  initialstate='SubstitutionDef', blankfinish=blankfinish)
            self.statemachine.previousline(
                  len(block) + offset - newabsoffset - 1)
            i = 0
            for node in substitutionnode[:]:
                if not (isinstance(node, nodes.Inline) or
                        isinstance(node, nodes.Text)):
                    self.statemachine.node += substitutionnode[i]
                    del substitutionnode[i]
                else:
                    i += 1
            if len(substitutionnode) == 0:
                msg = self.statemachine.memo.reporter.warning(
                      'Substitution definition "%s" empty or invalid at line '
                      '%s.' % (subname, self.statemachine.abslineno()), '',
                      nodes.literal_block(blocktext, blocktext))
                self.statemachine.node += msg
            else:
                del substitutionnode['alt']
                self.statemachine.memo.document.note_substitution_def(
                      substitutionnode, self.statemachine.node)
                return [substitutionnode], blankfinish
        else:
            msg = self.statemachine.memo.reporter.warning(
                  'Substitution definition "%s" missing contents at line %s.'
                  % (subname, self.statemachine.abslineno()), '',
                  nodes.literal_block(blocktext, blocktext))
            self.statemachine.node += msg
        return [], blankfinish

    def directive(self, match, **attributes):
        typename = match.group(1)
        directivefunction = directives.directive(
              typename, self.statemachine.memo.language)
        data = match.string[match.end():].strip()
        if directivefunction:
            return directivefunction(match, typename, data, self,
                                     self.statemachine, attributes)
        else:
            return self.unknowndirective(typename, data)

    def unknowndirective(self, typename, data):
        lineno = self.statemachine.abslineno()
        indented, indent, offset, blankfinish = \
              self.statemachine.getfirstknownindented(0, stripindent=0)
        text = '\n'.join(indented)
        error = self.statemachine.memo.reporter.error(
              'Unknown directive type "%s" at line %s.' % (typename, lineno),
              '', nodes.literal_block(text, text))
        return [error], blankfinish

    def parse_extension_attributes(self, attribute_spec, datalines, blankfinish):
        """
        Parse `datalines` for a field list containing extension attributes
        matching `attribute_spec`.

        :Parameters:
            - `attribute_spec`: a mapping of attribute name to conversion
              function, which should raise an exception on bad input.
            - `datalines`: a list of input strings.
            - `blankfinish`:

        :Return:
            - Success value, 1 or 0.
            - An attribute dictionary on success, an error string on failure.
            - Updated `blankfinish` flag.
        """
        node = nodes.field_list()
        newlineoffset, blankfinish = self.nestedlistparse(
              datalines, 0, node, initialstate='FieldList',
              blankfinish=blankfinish)
        if newlineoffset != len(datalines): # incomplete parse of block
            return 0, 'invalid attribute block', blankfinish
        try:
            attributes = utils.extract_extension_attributes(node, attribute_spec)
        except KeyError, detail:
            return 0, ('unknown attribute: "%s"' % detail), blankfinish
        except (ValueError, TypeError), detail:
            return 0, ('invalid attribute value:\n%s' % detail), blankfinish
        except utils.ExtensionAttributeError, detail:
            return 0, ('invalid attribute data: %s' % detail), blankfinish
        return 1, attributes, blankfinish

    def comment(self, match):
        if not match.string[match.end():].strip() \
              and self.statemachine.nextlineblank(): # an empty comment?
            return [nodes.comment()], 1 # "A tiny but practical wart."
        indented, indent, offset, blankfinish = \
              self.statemachine.getfirstknownindented(match.end())
        text = '\n'.join(indented)
        return [nodes.comment(text, text)], blankfinish

    explicit.constructs = [
          (footnote,
           re.compile(r"""
                      \.\.[ ]+          # explicit markup start
                      \[
                      (                 # footnote label:
                          [0-9]+          # manually numbered footnote
                        |               # *OR*
                          \#              # anonymous auto-numbered footnote
                        |               # *OR*
                          \#%s            # auto-number ed?) footnote label
                        |               # *OR*
                          \*              # auto-symbol footnote
                      )
                      \]
                      (?:[ ]+|$)        # whitespace or end of line
                      """ % RSTState.inline.simplename, re.VERBOSE)),
          (citation,
           re.compile(r"""
                      \.\.[ ]+          # explicit markup start
                      \[(%s)\]          # citation label
                      (?:[ ]+|$)        # whitespace or end of line
                      """ % RSTState.inline.simplename, re.VERBOSE)),
          (hyperlink_target,
           re.compile(r"""
                      \.\.[ ]+          # explicit markup start
                      _                 # target indicator
                      (?![ ])           # first char. not space
                      """, re.VERBOSE)),
          (substitutiondef,
           re.compile(r"""
                      \.\.[ ]+          # explicit markup start
                      \|                # substitution indicator
                      (?![ ])           # first char. not space
                      """, re.VERBOSE)),
          (directive,
           re.compile(r"""
                      \.\.[ ]+          # explicit markup start
                      (%s)              # directive name
                      ::                # directive delimiter
                      (?:[ ]+|$)        # whitespace or end of line
                      """ % RSTState.inline.simplename, re.VERBOSE))]

    def explicit_markup(self, match, context, nextstate):
        """Footnotes, hyperlink targets, directives, comments."""
        nodelist, blankfinish = self.explicit_construct(match)
        self.statemachine.node += nodelist
        self.explicitlist(blankfinish)
        return [], nextstate, []

    def explicit_construct(self, match):
        """Determine which explicit construct this is, parse & return it."""
        errors = []
        for method, pattern in self.explicit.constructs:
            expmatch = pattern.match(match.string)
            if expmatch:
                try:
                    return method(self, expmatch)
                except MarkupError, detail: # never reached?
                    errors.append(
                          self.statemachine.memo.reporter.warning('%s: %s'
                          % (detail.__class__.__name__, detail)))
                    break
        nodelist, blankfinish = self.comment(match)
        return nodelist + errors, blankfinish

    def explicitlist(self, blankfinish):
        """
        Create a nested state machine for a series of explicit markup constructs
        (including anonymous hyperlink targets).
        """
        offset = self.statemachine.lineoffset + 1   # next line
        newlineoffset, blankfinish = self.nestedlistparse(
              self.statemachine.inputlines[offset:],
              inputoffset=self.statemachine.abslineoffset() + 1,
              node=self.statemachine.node, initialstate='Explicit',
              blankfinish=blankfinish)
        self.gotoline(newlineoffset)
        if not blankfinish:
            self.statemachine.node += self.unindentwarning()

    def anonymous(self, match, context, nextstate):
        """Anonymous hyperlink targets."""
        nodelist, blankfinish = self.anonymous_target(match)
        self.statemachine.node += nodelist
        self.explicitlist(blankfinish)
        return [], nextstate, []

    def anonymous_target(self, match):
        block, indent, offset, blankfinish \
              = self.statemachine.getfirstknownindented(match.end(),
                                                        uptoblank=1)
        blocktext = match.string[:match.end()] + '\n'.join(block)
        if block and block[-1].strip()[-1:] == '_': # possible indirect target
            reference = escape2null(' '.join([line.strip() for line in block]))
            refname = self.isreference(reference)
            if refname:
                target = nodes.target(blocktext, '', refname=refname,
                                      anonymous=1)
                self.statemachine.memo.document.note_anonymous_target(target)
                self.statemachine.memo.document.note_indirect_target(target)
                return [target], blankfinish
        nodelist = []
        reference = escape2null(''.join([line.strip() for line in block]))
        if reference.find(' ') != -1:
            warning = self.statemachine.memo.reporter.warning(
                  'Anonymous hyperlink target at line %s contains whitespace. '
                  'Perhaps a footnote was intended?'
                  % (self.statemachine.abslineno() - len(block) + 1), '',
                  nodes.literal_block(blocktext, blocktext))
            nodelist.append(warning)
        else:
            target = nodes.target(blocktext, '', anonymous=1)
            if reference:
                unescaped = unescape(reference)
                target['refuri'] = unescaped
            self.statemachine.memo.document.note_anonymous_target(target)
            nodelist.append(target)
        return nodelist, blankfinish

    def line(self, match, context, nextstate):
        """Section title overline or transition marker."""
        if self.statemachine.matchtitles:
            return [match.string], 'Line', []
        else:
            blocktext = self.statemachine.line
            msg = self.statemachine.memo.reporter.severe(
                  'Unexpected section title or transition at line %s.'
                  % self.statemachine.abslineno(), '',
                  nodes.literal_block(blocktext, blocktext))
            self.statemachine.node += msg
            return [], nextstate, []

    def text(self, match, context, nextstate):
        """Titles, definition lists, paragraphs."""
        return [match.string], 'Text', []


class SpecializedBody(Body):

    """
    Superclass for second and subsequent compound element members.

    All transition methods are disabled. Override individual methods in
    subclasses to re-enable.
    """

    def invalid_input(self, match=None, context=None, nextstate=None):
        """Not a compound element member. Abort this state machine."""
        self.statemachine.previousline()  # back up so parent SM can reassess
        raise EOFError

    indent = invalid_input
    bullet = invalid_input
    enumerator = invalid_input
    field_marker = invalid_input
    option_marker = invalid_input
    doctest = invalid_input
    tabletop = invalid_input
    explicit_markup = invalid_input
    anonymous = invalid_input
    line = invalid_input
    text = invalid_input


class BulletList(SpecializedBody):

    """Second and subsequent bullet_list list_items."""

    def bullet(self, match, context, nextstate):
        """Bullet list item."""
        if match.string[0] != self.statemachine.node['bullet']:
            # different bullet: new list
            self.invalid_input()
        listitem, blankfinish = self.list_item(match.end())
        self.statemachine.node += listitem
        self.blankfinish = blankfinish
        return [], 'BulletList', []


class DefinitionList(SpecializedBody):

    """Second and subsequent definition_list_items."""

    def text(self, match, context, nextstate):
        """Definition lists."""
        return [match.string], 'Definition', []


class EnumeratedList(SpecializedBody):

    """Second and subsequent enumerated_list list_items."""

    def enumerator(self, match, context, nextstate):
        """Enumerated list item."""
        format, sequence, text, ordinal = self.parse_enumerator(
              match, self.statemachine.node['enumtype'])
        if (sequence != self.statemachine.node['enumtype'] or
            format != self.format or
            ordinal != self.lastordinal + 1):
            # different enumeration: new list
            self.invalid_input()
        listitem, blankfinish = self.list_item(match.end())
        self.statemachine.node += listitem
        self.blankfinish = blankfinish
        self.lastordinal = ordinal
        return [], 'EnumeratedList', []


class FieldList(SpecializedBody):

    """Second and subsequent field_list fields."""

    def field_marker(self, match, context, nextstate):
        """Field list field."""
        field, blankfinish = self.field(match)
        self.statemachine.node += field
        self.blankfinish = blankfinish
        return [], 'FieldList', []


class OptionList(SpecializedBody):

    """Second and subsequent option_list option_list_items."""

    def option_marker(self, match, context, nextstate):
        """Option list item."""
        try:
            option_list_item, blankfinish = self.option_list_item(match)
        except MarkupError, detail:
            self.invalid_input()
        self.statemachine.node += option_list_item
        self.blankfinish = blankfinish
        return [], 'OptionList', []


class RFC822List(SpecializedBody):

    """Second and subsequent RFC822 field_list fields."""

    pass


class Explicit(SpecializedBody):

    """Second and subsequent explicit markup construct."""

    def explicit_markup(self, match, context, nextstate):
        """Footnotes, hyperlink targets, directives, comments."""
        nodelist, blankfinish = self.explicit_construct(match)
        self.statemachine.node += nodelist
        self.blankfinish = blankfinish
        return [], nextstate, []

    def anonymous(self, match, context, nextstate):
        """Anonymous hyperlink targets."""
        nodelist, blankfinish = self.anonymous_target(match)
        self.statemachine.node += nodelist
        self.blankfinish = blankfinish
        return [], nextstate, []


class SubstitutionDef(Body):

    """
    Parser for the contents of a substitution_definition element.
    """

    patterns = {
          'embedded_directive': r'(%s)::( +|$)' % RSTState.inline.simplename,
          'text': r''}
    initialtransitions = ['embedded_directive', 'text']

    def embedded_directive(self, match, context, nextstate):
        if self.statemachine.node.has_key('alt'):
            attributes = {'alt': self.statemachine.node['alt']}
        else:
            attributes = {}
        nodelist, blankfinish = self.directive(match, **attributes)
        self.statemachine.node += nodelist
        if not self.statemachine.ateof():
            self.blankfinish = blankfinish
        raise EOFError

    def text(self, match, context, nextstate):
        if not self.statemachine.ateof():
            self.blankfinish = self.statemachine.nextlineblank()
        raise EOFError


class Text(RSTState):

    """
    Classifier of second line of a text block.

    Could be a paragraph, a definition list item, or a title.
    """

    patterns = {'underline': Body.patterns['line'],
                'text': r''}
    initialtransitions = [('underline', 'Body'), ('text', 'Body')]

    def blank(self, match, context, nextstate):
        """End of paragraph."""
        paragraph, literalnext = self.paragraph(
              context, self.statemachine.abslineno() - 1)
        self.statemachine.node += paragraph
        if literalnext:
            self.statemachine.node += self.literal_block()
        return [], 'Body', []

    def eof(self, context):
        if context:
            paragraph, literalnext = self.paragraph(
                  context, self.statemachine.abslineno() - 1)
            self.statemachine.node += paragraph
            if literalnext:
                self.statemachine.node += self.literal_block()
        return []

    def indent(self, match, context, nextstate):
        """Definition list item."""
        definitionlist = nodes.definition_list()
        definitionlistitem, blankfinish = self.definition_list_item(context)
        definitionlist += definitionlistitem
        self.statemachine.node += definitionlist
        offset = self.statemachine.lineoffset + 1   # next line
        newlineoffset, blankfinish = self.nestedlistparse(
              self.statemachine.inputlines[offset:],
              inputoffset=self.statemachine.abslineoffset() + 1,
              node=definitionlist, initialstate='DefinitionList',
              blankfinish=blankfinish, blankfinishstate='Definition')
        if not blankfinish:
            self.statemachine.node += self.unindentwarning()
        self.gotoline(newlineoffset)
        return [], 'Body', []

    def underline(self, match, context, nextstate):
        """Section title."""
        lineno = self.statemachine.abslineno()
        if not self.statemachine.matchtitles:
            blocktext = context[0] + '\n' + self.statemachine.line
            msg = self.statemachine.memo.reporter.severe(
                  'Unexpected section title at line %s.' % lineno, '',
                  nodes.literal_block(blocktext, blocktext))
            self.statemachine.node += msg
            return [], nextstate, []
        title = context[0].rstrip()
        underline = match.string.rstrip()
        source = title + '\n' + underline
        if len(title) > len(underline):
            blocktext = context[0] + '\n' + self.statemachine.line
            msg = self.statemachine.memo.reporter.info(
                  'Title underline too short at line %s.' % lineno, '',
                  nodes.literal_block(blocktext, blocktext))
            self.statemachine.node += msg
        style = underline[0]
        context[:] = []
        self.section(title, source, style, lineno - 1)
        return [], nextstate, []

    def text(self, match, context, nextstate):
        """Paragraph."""
        startline = self.statemachine.abslineno() - 1
        msg = None
        try:
            block = self.statemachine.getunindented()
        except statemachine.UnexpectedIndentationError, instance:
            block, lineno = instance.args
            msg = self.statemachine.memo.reporter.error(
                  'Unexpected indentation at line %s.' % lineno)
        lines = context + block
        paragraph, literalnext = self.paragraph(lines, startline)
        self.statemachine.node += paragraph
        self.statemachine.node += msg
        if literalnext:
            try:
                self.statemachine.nextline()
            except IndexError:
                pass
            self.statemachine.node += self.literal_block()
        return [], nextstate, []

    def literal_block(self):
        """Return a list of nodes."""
        indented, indent, offset, blankfinish = \
              self.statemachine.getindented()
        nodelist = []
        while indented and not indented[-1].strip():
            indented.pop()
        if indented:
            data = '\n'.join(indented)
            nodelist.append(nodes.literal_block(data, data))
            if not blankfinish:
                nodelist.append(self.unindentwarning())
        else:
            nodelist.append(self.statemachine.memo.reporter.warning(
                  'Literal block expected at line %s; none found.'
                  % self.statemachine.abslineno()))
        return nodelist

    def definition_list_item(self, termline):
        indented, indent, lineoffset, blankfinish = \
              self.statemachine.getindented()
        definitionlistitem = nodes.definition_list_item('\n'.join(termline
                                                                  + indented))
        termlist, messages = self.term(termline,
                                       self.statemachine.abslineno() - 1)
        definitionlistitem += termlist
        definition = nodes.definition('', *messages)
        definitionlistitem += definition
        if termline[0][-2:] == '::':
            definition += self.statemachine.memo.reporter.info(
                  'Blank line missing before literal block? Interpreted as a '
                  'definition list item. At line %s.' % (lineoffset + 1))
        self.nestedparse(indented, inputoffset=lineoffset, node=definition)
        return definitionlistitem, blankfinish

    def term(self, lines, lineno):
        """Return a definition_list's term and optional classifier."""
        assert len(lines) == 1
        nodelist = []
        parts = lines[0].split(' : ', 1)  # split into 1 or 2 parts
        termpart = parts[0].rstrip()
        textnodes, messages = self.inline_text(termpart, lineno)
        nodelist = [nodes.term(termpart, '', *textnodes)]
        if len(parts) == 2:
            classifierpart = parts[1].lstrip()
            textnodes, cpmessages = self.inline_text(classifierpart, lineno)
            nodelist.append(nodes.classifier(classifierpart, '', *textnodes))
            messages += cpmessages
        return nodelist, messages


class SpecializedText(Text):

    """
    Superclass for second and subsequent lines of Text-variants.

    All transition methods are disabled. Override individual methods in
    subclasses to re-enable.
    """

    def eof(self, context):
        """Incomplete construct."""
        return []

    def invalid_input(self, match=None, context=None, nextstate=None):
        """Not a compound element member. Abort this state machine."""
        raise EOFError

    blank = invalid_input
    indent = invalid_input
    underline = invalid_input
    text = invalid_input


class Definition(SpecializedText):

    """Second line of potential definition_list_item."""

    def eof(self, context):
        """Not a definition."""
        self.statemachine.previousline(2) # back up so parent SM can reassess
        return []

    def indent(self, match, context, nextstate):
        """Definition list item."""
        definitionlistitem, blankfinish = self.definition_list_item(context)
        self.statemachine.node += definitionlistitem
        self.blankfinish = blankfinish
        return [], 'DefinitionList', []


class Line(SpecializedText):

    """Second line of over- & underlined section title or transition marker."""

    eofcheck = 1                        # @@@ ???
    """Set to 0 while parsing sections, so that we don't catch the EOF."""

    def eof(self, context):
        """Transition marker at end of section or document."""
        if self.eofcheck:               # ignore EOFError with sections
            transition = nodes.transition(context[0])
            self.statemachine.node += transition
            msg = self.statemachine.memo.reporter.error(
                  'Document or section may not end with a transition '
                  '(line %s).' % (self.statemachine.abslineno() - 1))
            self.statemachine.node += msg
        self.eofcheck = 1
        return []

    def blank(self, match, context, nextstate):
        """Transition marker."""
        transition = nodes.transition(context[0])
        if len(self.statemachine.node) == 0:
            msg = self.statemachine.memo.reporter.error(
                  'Document or section may not begin with a transition '
                  '(line %s).' % (self.statemachine.abslineno() - 1))
            self.statemachine.node += msg
        elif isinstance(self.statemachine.node[-1], nodes.transition):
            msg = self.statemachine.memo.reporter.error(
                  'At least one body element must separate transitions; '
                  'adjacent transitions at line %s.'
                  % (self.statemachine.abslineno() - 1))
            self.statemachine.node += msg
        self.statemachine.node += transition
        return [], 'Body', []

    def text(self, match, context, nextstate):
        """Potential over- & underlined title."""
        lineno = self.statemachine.abslineno() - 1
        overline = context[0]
        title = match.string
        underline = ''
        try:
            underline = self.statemachine.nextline()
        except IndexError:
            blocktext = overline + '\n' + title
            msg = self.statemachine.memo.reporter.severe(
                  'Incomplete section title at line %s.' % lineno, '',
                  nodes.literal_block(blocktext, blocktext))
            self.statemachine.node += msg
            return [], 'Body', []
        source = '%s\n%s\n%s' % (overline, title, underline)
        overline = overline.rstrip()
        underline = underline.rstrip()
        if not self.transitions['underline'][0].match(underline):
            msg = self.statemachine.memo.reporter.severe(
                  'Missing underline for overline at line %s.' % lineno, '',
                  nodes.literal_block(source, source))
            self.statemachine.node += msg
            return [], 'Body', []
        elif overline != underline:
            msg = self.statemachine.memo.reporter.severe(
                  'Title overline & underline mismatch at ' 'line %s.' % lineno,
                  '', nodes.literal_block(source, source))
            self.statemachine.node += msg
            return [], 'Body', []
        title = title.rstrip()
        if len(title) > len(overline):
            msg = self.statemachine.memo.reporter.info(
                  'Title overline too short at line %s.'% lineno, '',
                  nodes.literal_block(source, source))
            self.statemachine.node += msg
        style = (overline[0], underline[0])
        self.eofcheck = 0               # @@@ not sure this is correct
        self.section(title.lstrip(), source, style, lineno + 1)
        self.eofcheck = 1
        return [], 'Body', []

    indent = text                       # indented title

    def underline(self, match=None, context=None, nextstate=None):
        blocktext = context[0] + '\n' + self.statemachine.line
        msg = self.statemachine.memo.reporter.error(
              'Invalid section title or transition marker at line %s.'
              % (self.statemachine.abslineno() - 1), '',
              nodes.literal_block(blocktext, blocktext))
        self.statemachine.node += msg
        return [], 'Body', []


stateclasses = [Body, BulletList, DefinitionList, EnumeratedList, FieldList,
                OptionList, RFC822List, Explicit, Text, Definition, Line,
                SubstitutionDef]
"""Standard set of State classes used to start `RSTStateMachine`."""


def escape2null(text):
    """Return a string with escape-backslashes converted to nulls."""
    parts = []
    start = 0
    while 1:
        found = text.find('\\', start)
        if found == -1:
            parts.append(text[start:])
            return ''.join(parts)
        parts.append(text[start:found])
        parts.append('\x00' + text[found+1:found+2])
        start = found + 2               # skip character after escape

def unescape(text, restorebackslashes=0):
    """Return a string with nulls removed or restored to backslashes."""
    if restorebackslashes:
        return text.translate(RSTState.inline.null2backslash)
    else:
        return text.translate(RSTState.inline.identity, '\x00')
