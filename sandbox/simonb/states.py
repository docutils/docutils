"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This is the ``docutils.parsers.restructuredtext.states`` module, the core of
the reStructuredText parser.  It defines the following:

:Classes:
    - `RSTStateMachine`: reStructuredText parser's entry point.
    - `NestedStateMachine`: recursive StateMachine.
    - `RSTState`: reStructuredText State superclass.
    - `Inliner`: For parsing inline markup.
    - `Body`: Generic classifier of the first line of a block.
    - `SpecializedBody`: Superclass for compound element members.
    - `BulletList`: Second and subsequent bullet_list list_items
    - `DefinitionList`: Second+ definition_list_items.
    - `EnumeratedList`: Second+ enumerated_list list_items.
    - `FieldList`: Second+ fields.
    - `OptionList`: Second+ option_list_items.
    - `RFC2822List`: Second+ RFC2822-style fields.
    - `Explicit`: Second+ explicit markup constructs.
    - `SubstitutionDef`: For embedded directives in substitution definitions.
    - `Text`: Classifier of second line of a text block.
    - `SpecializedText`: Superclass for continuation lines of Text-variants.
    - `Definition`: Second line of potential definition_list_item.
    - `Line`: Second line of overlined section title or transition marker.
    - `Stuff`: An auxilliary collection class.

:Exception classes:
    - `MarkupError`
    - `ParserError`
    - `MarkupMismatch`

:Functions:
    - `escape2null()`: Return a string, escape-backslashes converted to nulls.
    - `unescape()`: Return a string, nulls removed or restored to backslashes.

:Attributes:
    - `state_classes`: set of State classes used with `RSTStateMachine`.

Parser Overview
===============

The reStructuredText parser is implemented as a state machine, examining its
input one line at a time.  To understand how the parser works, please first
become familiar with the `docutils.statemachine` module.  In the description
below, references are made to classes defined in this module; please see the
individual classes for details.

Parsing proceeds as follows:

1. The state machine examines each line of input, checking each of the
   transition patterns of the state `Body`, in order, looking for a match.
   The implicit transitions (blank lines and indentation) are checked before
   any others.  The 'text' transition is a catch-all (matches anything).

2. The method associated with the matched transition pattern is called.

   A. Some transition methods are self-contained, appending elements to the
      document tree ('doctest' parses a doctest block).  The parser's current
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
        'bullet' transition).  This state machine is run to parse the compound
        element (or series of explicit markup elements), and returns as soon
        as a non-member element is encountered.  For example, the `BulletList`
        state machine aborts as soon as it encounters an element which is not
        a list item of that bullet list.  The optional omission of
        inter-element blank lines is handled by the nested state machine.

      - The current line index is advanced to the end of the elements parsed,
        and parsing continues with step 1.

   C. The result of the 'text' transition depends on the next line of text.
      The current state is changed to `Text`, under which the second line is
      examined.  If the second line is:

      - Indented: The element is a definition list item, and parsing proceeds
        similarly to step 2.B, using the `DefinitionList` state.

      - A line of uniform punctuation characters: The element is a section
        header; again, parsing proceeds as in step 2.B, and `Body` is still
        used.

      - Anything else: The element is a paragraph, which is examined for
        inline markup and appended to the parent element.  Processing
        continues with step 1.
"""

__docformat__ = 'reStructuredText'


import sys
import re
import string
from docutils import nodes, statemachine, utils, roman, urischemes
from docutils import ApplicationError, DataError
from docutils.statemachine import StateMachineWS, StateWS
from docutils.utils import normalize_name
from docutils.parsers.rst import directives, languages
from docutils.parsers.rst.tableparser import TableParser, TableMarkupError


class MarkupError(DataError): pass
class ParserError(ApplicationError): pass
class MarkupMismatch(Exception): pass


class Stuff:

    """Stores a bunch of stuff for dotted-attribute access."""

    def __init__(self, **keywordargs):
        self.__dict__.update(keywordargs)


class RSTStateMachine(StateMachineWS):

    """
    reStructuredText's master StateMachine.

    The entry point to reStructuredText parsing is the `run()` method.
    """

    def run(self, input_lines, document, input_offset=0, match_titles=1,
            inliner=None):
        """
        Parse `input_lines` and return a `docutils.nodes.document` instance.

        Extend `StateMachineWS.run()`: set up parse-global data, run the
        StateMachine, and return the resulting
        document.
        """
        self.language = languages.get_language(document.options.language_code)
        self.match_titles = match_titles
        if inliner is None:
            inliner = Inliner()
        self.memo = Stuff(document=document,
                          reporter=document.reporter,
                          language=self.language,
                          title_styles=[],
                          section_level=0,
                          inliner=inliner)
        self.document = self.memo.document
        self.reporter = self.memo.reporter
        self.node = document
        results = StateMachineWS.run(self, input_lines, input_offset)
        assert results == [], 'RSTStateMachine.run() results should be empty!'
        self.node = self.memo = None    # remove unneeded references


class NestedStateMachine(StateMachineWS):

    """
    StateMachine run from within other StateMachine runs, to parse nested
    document structures.
    """

    def run(self, input_lines, input_offset, memo, node, match_titles=1):
        """
        Parse `input_lines` and populate a `docutils.nodes.document` instance.

        Extend `StateMachineWS.run()`: set up document-wide data.
        """
        self.match_titles = match_titles
        self.memo = memo
        self.document = memo.document
        self.reporter = memo.reporter
        self.node = node
        results = StateMachineWS.run(self, input_lines, input_offset)
        assert results == [], ('NestedStateMachine.run() results should be '
                               'empty!')
        return results


class RSTState(StateWS):

    """
    reStructuredText State superclass.

    Contains methods used by all State subclasses.
    """

    nested_sm = NestedStateMachine

    def __init__(self, state_machine, debug=0):
        self.nested_sm_kwargs = {'state_classes': state_classes,
                                 'initial_state': 'Body'}
        StateWS.__init__(self, state_machine, debug)

    def runtime_init(self):
        StateWS.runtime_init(self)
        memo = self.state_machine.memo
        self.memo = memo
        self.reporter = memo.reporter
        self.inliner = memo.inliner
        self.document = memo.document
        self.parent = self.state_machine.node

    def goto_line(self, abs_line_offset):
        """
        Jump to input line `abs_line_offset`, ignoring jumps past the end.
        """
        try:
            self.state_machine.goto_line(abs_line_offset)
        except IndexError:
            pass

    def bof(self, context):
        """Called at beginning of file."""
        return [], []

    def nested_parse(self, block, input_offset, node, match_titles=0,
                     state_machine_class=None, state_machine_kwargs=None):
        """
        Create a new StateMachine rooted at `node` and run it over the input
        `block`.
        """
        if state_machine_class is None:
            state_machine_class = self.nested_sm
        if state_machine_kwargs is None:
            state_machine_kwargs = self.nested_sm_kwargs
        state_machine = state_machine_class(debug=self.debug,
                                            **state_machine_kwargs)
        state_machine.run(block, input_offset, memo=self.memo,
                          node=node, match_titles=match_titles)
        state_machine.unlink()
        return state_machine.abs_line_offset()

    def nested_list_parse(self, block, input_offset, node, initial_state,
                          blank_finish,
                          blank_finish_state=None,
                          extra_settings={},
                          match_titles=0,
                          state_machine_class=None,
                          state_machine_kwargs=None):
        """
        Create a new StateMachine rooted at `node` and run it over the input
        `block`. Also keep track of optional intermdediate blank lines and the
        required final one.
        """
        if state_machine_class is None:
            state_machine_class = self.nested_sm
        if state_machine_kwargs is None:
            state_machine_kwargs = self.nested_sm_kwargs.copy()
        state_machine_kwargs['initial_state'] = initial_state
        state_machine = state_machine_class(debug=self.debug,
                                            **state_machine_kwargs)
        if blank_finish_state is None:
            blank_finish_state = initial_state
        state_machine.states[blank_finish_state].blank_finish = blank_finish
        for key, value in extra_settings.items():
            setattr(state_machine.states[initial_state], key, value)
        state_machine.run(block, input_offset, memo=self.memo,
                          node=node, match_titles=match_titles)
        blank_finish = state_machine.states[blank_finish_state].blank_finish
        state_machine.unlink()
        return state_machine.abs_line_offset(), blank_finish

    def section(self, title, source, style, lineno):
        """Check for a valid subsection and create one if it checks out."""
        if self.check_subsection(source, style, lineno):
            self.new_subsection(title, lineno)

    def check_subsection(self, source, style, lineno):
        """
        Check for a valid subsection header.  Return 1 (true) or None (false).

        When a new section is reached that isn't a subsection of the current
        section, back up the line count (use ``previous_line(-x)``), then
        ``raise EOFError``.  The current StateMachine will finish, then the
        calling StateMachine can re-examine the title.  This will work its way
        back up the calling chain until the correct section level isreached.

        @@@ Alternative: Evaluate the title, store the title info & level, and
        back up the chain until that level is reached.  Store in memo? Or
        return in results?

        :Exception: `EOFError` when a sibling or supersection encountered.
        """
        memo = self.memo
        title_styles = memo.title_styles
        mylevel = memo.section_level
        try:                            # check for existing title style
            level = title_styles.index(style) + 1
        except ValueError:              # new title style
            if len(title_styles) == memo.section_level: # new subsection
                title_styles.append(style)
                return 1
            else:                       # not at lowest level
                self.parent += self.title_inconsistent(source, lineno)
                return None
        if level <= mylevel:            # sibling or supersection
            memo.section_level = level   # bubble up to parent section
            # back up 2 lines for underline title, 3 for overline title
            self.state_machine.previous_line(len(style) + 1)
            raise EOFError              # let parent section re-evaluate
        if level == mylevel + 1:        # immediate subsection
            return 1
        else:                           # invalid subsection
            self.parent += self.title_inconsistent(source, lineno)
            return None

    def title_inconsistent(self, sourcetext, lineno):
        literalblock = nodes.literal_block('', sourcetext)
        error = self.reporter.severe('Title level inconsistent at line %s:'
                                     % lineno, '', literalblock)
        return error

    def new_subsection(self, title, lineno):
        """Append new subsection to document tree. On return, check level."""
        memo = self.memo
        mylevel = memo.section_level
        memo.section_level += 1
        sectionnode = nodes.section()
        self.parent += sectionnode
        textnodes, messages = self.inline_text(title, lineno)
        titlenode = nodes.title(title, '', *textnodes)
        name = normalize_name(titlenode.astext())
        sectionnode['name'] = name
        sectionnode += titlenode
        sectionnode += messages
        self.document.note_implicit_target(sectionnode, sectionnode)
        offset = self.state_machine.line_offset + 1
        absoffset = self.state_machine.abs_line_offset() + 1
        newabsoffset = self.nested_parse(
              self.state_machine.input_lines[offset:], input_offset=absoffset,
              node=sectionnode, match_titles=1)
        self.goto_line(newabsoffset)
        if memo.section_level <= mylevel: # can't handle next section?
            raise EOFError              # bubble up to supersection
        # reset section_level; next pass will detect it properly
        memo.section_level = mylevel

    def paragraph(self, lines, lineno):
        """
        Return a list (paragraph & messages) & a boolean: literal_block next?
        """
        data = '\n'.join(lines).rstrip()
        if data[-2:] == '::':
            if len(data) == 2:
                return [], 1
            elif data[-3] in ' \n':
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

    def inline_text(self, text, lineno):
        """
        Return 2 lists: nodes (text and inline elements), and system_messages.
        """
        return self.inliner.parse(text, lineno, self.memo, self.parent)

    def unindent_warning(self, node_name):
        return self.reporter.warning(
              ('%s ends without a blank line; unexpected unindent at line %s.'
               % (node_name, self.state_machine.abs_line_number() + 1)))


class Inliner:

    """
    Parse inline markup; call the `parse()` method.
    """

    def parse(self, text, lineno, memo, parent):
        """
        Return 2 lists: nodes (text and inline elements), and system_messages.

        Using `self.patterns.initial`, a pattern which matches start-strings
        (emphasis, strong, interpreted, phrase reference, literal,
        substitution reference, and inline target) and complete constructs
        (simple reference, footnote reference), search for a candidate.  When
        one is found, check for validity (e.g., not a quoted '*' character).
        If valid, search for the corresponding end string if applicable, and
        check it for validity.  If not found or invalid, generate a warning
        and ignore the start-string.  Implicit inline markup (e.g. standalone
        URIs) is found last.
        """
        self.reporter = memo.reporter
        self.document = memo.document
        self.parent = parent
        pattern_search = self.patterns.initial.search
        dispatch = self.dispatch
        remaining = escape2null(text)
        processed = []
        unprocessed = []
        messages = []
        while remaining:
            match = pattern_search(remaining)
            if match:
                groups = match.groupdict()
                method = dispatch[groups['start'] or groups['backquote']
                                  or groups['refend'] or groups['fnend']]
                before, inlines, remaining, sysmessages = method(self, match,
                                                                 lineno)
                unprocessed.append(before)
                messages += sysmessages
                if inlines:
                    processed += self.implicit_inline(''.join(unprocessed),
                                                      lineno)
                    processed += inlines
                    unprocessed = []
            else:
                break
        remaining = ''.join(unprocessed) + remaining
        if remaining:
            processed += self.implicit_inline(remaining, lineno)
        return processed, messages

    openers = '\'"([{<'
    closers = '\'")]}>'
    start_string_prefix = (r'((?<=^)|(?<=[-/: \n%s]))' % re.escape(openers))
    end_string_suffix = (r'((?=$)|(?=[-/:.,;!? \n%s]))' % re.escape(closers))
    non_whitespace_before = r'(?<![ \n])'
    non_whitespace_escape_before = r'(?<![ \n\x00])'
    non_whitespace_after = r'(?![ \n])'
    simplename = r'[a-zA-Z0-9]([-_.a-zA-Z0-9]*[a-zA-Z0-9])?'
    uric = r"""[-_.!~*'()[\];/:@&=+$,%a-zA-Z0-9]"""
    urilast = r"""[_~/\]a-zA-Z0-9]"""   # no punctuation
    emailc = r"""[-_!~*'{|}/#?^`&=+$%a-zA-Z0-9]"""
    patterns = Stuff(
          initial=re.compile(
                r"""
                %(start_string_prefix)s
                (
                  (?P<start>              # start-strings only:
                      \*\*                  # strong
                    |
                      \*                    # emphasis
                      (?!\*)                  # but not strong
                    |
                      ``                    # literal
                    |
                      _`                    # inline hyperlink target
                    |
                      \|                    # substitution_reference start
                  )
                  %(non_whitespace_after)s
                |                       # *OR*
                  (?P<whole>              # whole constructs:
                      (?P<refname>%(simplename)s)  # reference name
                      (?P<refend>__?)              # end-string
                      (\s+(?P=refend)<(?P<inline_target>[^>]*)>)?
                    |
                      \[                    # footnote_reference or
                                            # citation_reference start
                      (?P<footnotelabel>    # label:
                          [0-9]+              # manually numbered
                        |                   # *OR*
                          \#(%(simplename)s)?  # auto-numbered (w/ label?)
                        |                   # *OR*
                          \*                  # auto-symbol
                        |                   # *OR*
                          (?P<citationlabel>
                           %(simplename)s)    # citation reference
                      )
                      (?P<fnend>\]_)        # end-string
                  )
                  %(end_string_suffix)s
                |                       # *OR*
                  (?P<role>(:%(simplename)s:)?)  # optional role
                  (?P<backquote>          # start-string
                    `                       # interpreted text
                                            # or phrase reference
                    (?!`)                   # but not literal
                  )
                  %(non_whitespace_after)s  # no whitespace after
                )
                """ % locals(), re.VERBOSE),
          emphasis=re.compile(non_whitespace_escape_before
                              + r'(\*)' + end_string_suffix),
          strong=re.compile(non_whitespace_escape_before
                            + r'(\*\*)' + end_string_suffix),
          interpreted_or_phrase_ref=re.compile(
              r"""
              %(non_whitespace_escape_before)s
              (
                `
                (?P<suffix>
                  (?P<role>:%(simplename)s:)?
                  (?P<refend>__?)?
                  (\s+(?P=refend)<(?P<inline_target>[^>]*)>)?
                )
              )
              %(end_string_suffix)s
              """ % locals(), re.VERBOSE),
          literal=re.compile(non_whitespace_before + '(``)'
                             + end_string_suffix),
          target=re.compile(non_whitespace_escape_before
                            + r'(`)' + end_string_suffix),
          substitution_ref=re.compile(non_whitespace_escape_before
                                      + r'(\|_{0,2})'
                                      + end_string_suffix),
          uri=re.compile(
                r"""
                %(start_string_prefix)s
                (?P<whole>
                  (?P<absolute>           # absolute URI
                    (?P<scheme>             # scheme (http, ftp, mailto)
                      [a-zA-Z][a-zA-Z0-9.+-]*
                    )
                    :
                    (
                      (                       # either:
                        (//?)?                  # hierarchical URI
                        %(uric)s*               # URI characters
                        %(urilast)s             # final URI char
                      )
                      (                       # optional query
                        \?%(uric)s*
                        %(urilast)s
                      )?
                      (                       # optional fragment
                        \#%(uric)s*
                        %(urilast)s
                      )?
                    )
                  )
                |                       # *OR*
                  (?P<email>              # email address
                    %(emailc)s+(\.%(emailc)s+)*  # name
                    @                            # at
                    %(emailc)s+(\.%(emailc)s*)*  # host
                    %(urilast)s                  # final URI char
                  )
                )
                %(end_string_suffix)s
                """ % locals(), re.VERBOSE))

    def quoted_start(self, match):
        """Return 1 if inline markup start-string is 'quoted', 0 if not."""
        string = match.string
        start = match.start()
        end = match.end()
        if start == 0:                  # start-string at beginning of text
            return 0
        prestart = string[start - 1]
        try:
            poststart = string[end]
            if self.openers.index(prestart) \
                  == self.closers.index(poststart):   # quoted
                return 1
        except IndexError:              # start-string at end of text
            return 1
        except ValueError:              # not quoted
            pass
        return 0

    def inline_obj(self, match, lineno, end_pattern, nodeclass,
                   restore_backslashes=0):
        string = match.string
        matchstart = match.start('start')
        matchend = match.end('start')
        if self.quoted_start(match):
            return (string[:matchend], [], string[matchend:], [], '')
        endmatch = end_pattern.search(string[matchend:])
        if endmatch and endmatch.start(1):  # 1 or more chars
            text = unescape(endmatch.string[:endmatch.start(1)],
                            restore_backslashes)
            textend = matchend + endmatch.end(1)
            rawsource = unescape(string[matchstart:textend], 1)
            return (string[:matchstart], [nodeclass(rawsource, text)],
                    string[textend:], [], endmatch.group(1))
        msg = self.reporter.warning(
              'Inline %s start-string without end-string '
              'at line %s.' % (nodeclass.__name__, lineno))
        text = unescape(string[matchstart:matchend], 1)
        rawsource = unescape(string[matchstart:matchend], 1)
        prb = self.problematic(text, rawsource, msg)
        return string[:matchstart], [prb], string[matchend:], [msg], ''

    def problematic(self, text, rawsource, message):
        msgid = self.document.set_id(message, self.parent)
        problematic = nodes.problematic(rawsource, text, refid=msgid)
        prbid = self.document.set_id(problematic)
        message.add_backref(prbid)
        return problematic

    def emphasis(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inline_obj(
              match, lineno, self.patterns.emphasis, nodes.emphasis)
        return before, inlines, remaining, sysmessages

    def strong(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inline_obj(
              match, lineno, self.patterns.strong, nodes.strong)
        return before, inlines, remaining, sysmessages

    def interpreted_or_phrase_ref(self, match, lineno):
        end_pattern = self.patterns.interpreted_or_phrase_ref
        string = match.string
        matchstart = match.start('backquote')
        matchend = match.end('backquote')
        rolestart = match.start('role')
        role = match.group('role')
        position = ''
        if role:
            role = role[1:-1]
            position = 'prefix'
        elif self.quoted_start(match):
            return (string[:matchend], [], string[matchend:], [])
        endmatch = end_pattern.search(string[matchend:])
        if endmatch and endmatch.start(1):  # 1 or more chars
            textend = matchend + endmatch.end()
            if endmatch.group('role'):
                if role:
                    msg = self.reporter.warning(
                        'Multiple roles in interpreted text at line %s (both '
                        'prefix and suffix present; only one allowed).'
                        % lineno)
                    text = unescape(string[rolestart:textend], 1)
                    prb = self.problematic(text, text, msg)
                    return string[:rolestart], [prb], string[textend:], [msg]
                role = endmatch.group('suffix')[1:-1]
                position = 'suffix'
            escaped = endmatch.string[:endmatch.start(1)]
            text = unescape(escaped, 0)
            rawsource = unescape(string[matchstart:textend], 1)
            if endmatch.group ('refend'):
                if role:
                    msg = self.reporter.warning(
                          'Mismatch: both interpreted text role %s and '
                          'reference suffix at line %s.' % (position, lineno))
                    text = unescape(string[rolestart:textend], 1)
                    prb = self.problematic(text, text, msg)
                    return string[:rolestart], [prb], string[textend:], [msg]
                return self.phrase_ref(string[:matchstart], string[textend:],
                                       rawsource, text,
                                       endmatch.group ('refend') == '__',
                                       endmatch.group('inline_target'))
            else:
                return self.interpreted(string[:rolestart], string[textend:],
                                        rawsource, text, role, position)
        msg = self.reporter.warning(
              'Inline interpreted text or phrase reference start-string '
              'without end-string at line %s.' % lineno)
        text = unescape(string[matchstart:matchend], 1)
        prb = self.problematic(text, text, msg)
        return string[:matchstart], [prb], string[matchend:], [msg]

    def phrase_ref (self, before, after, rawsource,
                    text, anonymous, inline_target):
        refname = normalize_name(text)
        reference = nodes.reference(rawsource, text)
        if inline_target:
            inline_target = ''.join (inline_target.split ('\n'))
            if inline_target:   # ugly...
                reference['refuri'] = inline_target

        if anonymous:
            if not inline_target:
                reference['anonymous'] = 1
                self.document.note_anonymous_ref(reference)
        else:
            reference['refname'] = refname
            self.document.note_refname(reference)
            if inline_target:
                target = nodes.target (rawsource, inline_target)
                target['name'] = refname
                target['refuri'] = inline_target
                self.document.note_external_target(target)
                self.document.note_explicit_target(target, self.parent)
        return before, [reference], after, []

    def interpreted(self, before, after, rawsource, text, role, position):
        if role:
            atts = {'role': role, 'position': position}
        else:
            atts = {}
        return before, [nodes.interpreted(rawsource, text, **atts)], after, []

    def literal(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inline_obj(
              match, lineno, self.patterns.literal, nodes.literal,
              restore_backslashes=1)
        return before, inlines, remaining, sysmessages

    def inline_target(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inline_obj(
              match, lineno, self.patterns.target, nodes.target)
        if inlines and isinstance(inlines[0], nodes.target):
            assert len(inlines) == 1
            target = inlines[0]
            name = normalize_name(target.astext())
            target['name'] = name
            self.document.note_explicit_target(target, self.parent)
        return before, inlines, remaining, sysmessages

    def substitution_reference(self, match, lineno):
        before, inlines, remaining, sysmessages, endstring = self.inline_obj(
              match, lineno, self.patterns.substitution_ref,
              nodes.substitution_reference)
        if len(inlines) == 1:
            subrefnode = inlines[0]
            if isinstance(subrefnode, nodes.substitution_reference):
                subreftext = subrefnode.astext()
                refname = normalize_name(subreftext)
                subrefnode['refname'] = refname
                self.document.note_substitution_ref(
                      subrefnode)
                if endstring[-1:] == '_':
                    referencenode = nodes.reference(
                          '|%s%s' % (subreftext, endstring), '')
                    if endstring[-2:] == '__':
                        referencenode['anonymous'] = 1
                        self.document.note_anonymous_ref(
                              referencenode)
                    else:
                        referencenode['refname'] = refname
                        self.document.note_refname(
                              referencenode)
                    referencenode += subrefnode
                    inlines = [referencenode]
        return before, inlines, remaining, sysmessages

    def footnote_reference(self, match, lineno):
        """
        Handles `nodes.footnote_reference` and `nodes.citation_reference`
        elements.
        """
        label = match.group('footnotelabel')
        refname = normalize_name(label)
        if match.group('citationlabel'):
            refnode = nodes.citation_reference('[%s]_' % label,
                                               refname=refname)
            refnode += nodes.Text(label)
            self.document.note_citation_ref(refnode)
        else:
            refnode = nodes.footnote_reference('[%s]_' % label)
            if refname[0] == '#':
                refname = refname[1:]
                refnode['auto'] = 1
                self.document.note_autofootnote_ref(refnode)
            elif refname == '*':
                refname = ''
                refnode['auto'] = '*'
                self.document.note_symbol_footnote_ref(
                      refnode)
            else:
                refnode += nodes.Text(label)
            if refname:
                refnode['refname'] = refname
                self.document.note_footnote_ref(refnode)
        string = match.string
        matchstart = match.start('whole')
        matchend = match.end('whole')
        return (string[:matchstart], [refnode], string[matchend:], [])

    def reference(self, match, lineno, anonymous=None):
        referencename = match.group('refname')
        refname = normalize_name(referencename)
        referencenode = nodes.reference(match.group('whole'),
                                        referencename)
        inline_target = match.group ('inline_target')

        if inline_target:
            inline_target = ''.join (inline_target.split ('\n'))
            if inline_target:  # ugly...
                referencenode['refuri']=inline_target

        if anonymous:
            if not inline_target:
                referencenode['anonymous'] = 1
                self.document.note_anonymous_ref(referencenode)
        else:
            referencenode['refname'] = refname
            self.document.note_refname(referencenode)
            if inline_target:
                target = nodes.target (match.group('whole'), inline_target)
                target['name'] = refname
                target['refuri'] = inline_target
                self.document.note_external_target(target)
                self.document.note_explicit_target(target, self.parent)
            
        string = match.string
        matchstart = match.start('whole')
        matchend = match.end('whole')
        return (string[:matchstart], [referencenode], string[matchend:], [])

    def anonymous_reference(self, match, lineno):
        return self.reference(match, lineno, anonymous=1)

    def standalone_uri(self, match, lineno):
        if not match.group('scheme') or urischemes.schemes.has_key(
              match.group('scheme').lower()):
            if match.group('email'):
                addscheme = 'mailto:'
            else:
                addscheme = ''
            text = match.group('whole')
            unescaped = unescape(text, 0)
            return [nodes.reference(unescape(text, 1), unescaped,
                                    refuri=addscheme + unescaped)]
        else:                   # not a valid scheme
            raise MarkupMismatch

    implicit = ((patterns.uri, standalone_uri),)
    """List of (pattern, dispatch method) pairs."""

    def implicit_inline(self, text, lineno):
        """
        Check each of the patterns in `self.implicit` for a match, and
        dispatch to the stored method for the pattern.  Recursively check the
        text before and after the match.  Return a list of `nodes.Text` and
        inline element nodes.
        """
        if not text:
            return []
        for pattern, dispatch in self.implicit:
            match = pattern.search(text)
            if match:
                try:
                    return (self.text(text[:match.start()])
                            + dispatch(self, match, lineno) +
                            self.implicit_inline(text[match.end():], lineno))
                except MarkupMismatch:
                    pass
        return [nodes.Text(unescape(text))]

    def text(self, text):
        """Return a list containing one `nodes.Text` node or nothing."""
        if not text:
            return []
        return [nodes.Text(unescape(text))]
        

    dispatch = {'*': emphasis,
                '**': strong,
                '`': interpreted_or_phrase_ref,
                '``': literal,
                '_`': inline_target,
                ']_': footnote_reference,
                '|': substitution_reference,
                '_': reference,
                '__': anonymous_reference}


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
        enum.sequenceregexps[sequence] = re.compile(
              enum.sequencepats[sequence] + '$')

    table_top_pat = re.compile(r'\+-[-+]+-\+ *$')
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

    patterns = {
          'bullet': r'[-+*]( +|$)',
          'enumerator': r'(%(parens)s|%(rparen)s|%(period)s)( +|$)' % pats,
          'field_marker': r':[^: ]([^:]*[^: ])?:( +|$)',
          'option_marker': r'%(option)s(, %(option)s)*(  +| ?$)' % pats,
          'doctest': r'>>>( +|$)',
          'table_top': table_top_pat,
          'explicit_markup': r'\.\.( +|$)',
          'anonymous': r'__( +|$)',
          'line': r'(%(nonalphanum7bit)s)\1\1\1+ *$' % pats,
          'text': r''}
    initial_transitions = (
          'bullet',
          'enumerator',
          'field_marker',
          'option_marker',
          'doctest',
          'table_top',
          'explicit_markup',
          'anonymous',
          'line',
          'text')

    def indent(self, match, context, next_state):
        """Block quote."""
        indented, indent, line_offset, blank_finish = \
              self.state_machine.get_indented()
        blockquote = self.block_quote(indented, line_offset)
        self.parent += blockquote
        if not blank_finish:
            self.parent += self.unindent_warning('Block quote')
        return context, next_state, []

    def block_quote(self, indented, line_offset):
        blockquote = nodes.block_quote()
        self.nested_parse(indented, line_offset, blockquote)
        return blockquote

    def bullet(self, match, context, next_state):
        """Bullet list item."""
        bulletlist = nodes.bullet_list()
        self.parent += bulletlist
        bulletlist['bullet'] = match.string[0]
        i, blank_finish = self.list_item(match.end())
        bulletlist += i
        offset = self.state_machine.line_offset + 1   # next line
        newline_offset, blank_finish = self.nested_list_parse(
              self.state_machine.input_lines[offset:],
              input_offset=self.state_machine.abs_line_offset() + 1,
              node=bulletlist, initial_state='BulletList',
              blank_finish=blank_finish)
        self.goto_line(newline_offset)
        if not blank_finish:
            self.parent += self.unindent_warning('Bullet list')
        return [], next_state, []

    def list_item(self, indent):
        indented, line_offset, blank_finish = \
              self.state_machine.get_known_indented(indent)
        listitem = nodes.list_item('\n'.join(indented))
        if indented:
            self.nested_parse(indented, input_offset=line_offset,
                              node=listitem)
        return listitem, blank_finish

    def enumerator(self, match, context, next_state):
        """Enumerated List Item"""
        format, sequence, text, ordinal = self.parse_enumerator(match)
        if ordinal is None:
            msg = self.reporter.error(
                  ('Enumerated list start value invalid at line %s: '
                   '"%s" (sequence %r)'
                   % (self.state_machine.abs_line_number(), text, sequence)))
            self.parent += msg
            indented, line_offset, blank_finish = \
                  self.state_machine.get_known_indented(match.end())
            bq = self.block_quote(indented, line_offset)
            self.parent += bq
            if not blank_finish:
                self.parent += self.unindent_warning(
                      'Enumerated list')
            return [], next_state, []
        if ordinal != 1:
            msg = self.reporter.info(
                  ('Enumerated list start value not ordinal-1 at line %s: '
                   '"%s" (ordinal %s)'
                   % (self.state_machine.abs_line_number(), text, ordinal)))
            self.parent += msg
        enumlist = nodes.enumerated_list()
        self.parent += enumlist
        enumlist['enumtype'] = sequence
        if ordinal != 1:
            enumlist['start'] = ordinal
        enumlist['prefix'] = self.enum.formatinfo[format].prefix
        enumlist['suffix'] = self.enum.formatinfo[format].suffix
        listitem, blank_finish = self.list_item(match.end())
        enumlist += listitem
        offset = self.state_machine.line_offset + 1   # next line
        newline_offset, blank_finish = self.nested_list_parse(
              self.state_machine.input_lines[offset:],
              input_offset=self.state_machine.abs_line_offset() + 1,
              node=enumlist, initial_state='EnumeratedList',
              blank_finish=blank_finish,
              extra_settings={'lastordinal': ordinal, 'format': format})
        self.goto_line(newline_offset)
        if not blank_finish:
            self.parent += self.unindent_warning('Enumerated list')
        return [], next_state, []

    def parse_enumerator(self, match, expected_sequence=None):
        """
        Analyze an enumerator and return the results.

        :Return:
            - the enumerator format ('period', 'parens', or 'rparen'),
            - the sequence used ('arabic', 'loweralpha', 'upperroman', etc.),
            - the text of the enumerator, stripped of formatting, and
            - the ordinal value of the enumerator ('a' -> 1, 'ii' -> 2, etc.;
              ``None`` is returned for invalid enumerator text).

        The enumerator format has already been determined by the regular
        expression match. If `expected_sequence` is given, that sequence is
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
        if expected_sequence:
            try:
                if self.enum.sequenceregexps[expected_sequence].match(text):
                    sequence = expected_sequence
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

    def field_marker(self, match, context, next_state):
        """Field list item."""
        fieldlist = nodes.field_list()
        self.parent += fieldlist
        field, blank_finish = self.field(match)
        fieldlist += field
        offset = self.state_machine.line_offset + 1   # next line
        newline_offset, blank_finish = self.nested_list_parse(
              self.state_machine.input_lines[offset:],
              input_offset=self.state_machine.abs_line_offset() + 1,
              node=fieldlist, initial_state='FieldList',
              blank_finish=blank_finish)
        self.goto_line(newline_offset)
        if not blank_finish:
            self.parent += self.unindent_warning('Field list')
        return [], next_state, []

    def field(self, match):
        name, args = self.parse_field_marker(match)
        indented, indent, line_offset, blank_finish = \
              self.state_machine.get_first_known_indented(match.end())
        fieldnode = nodes.field()
        fieldnode += nodes.field_name(name, name)
        for arg in args:
            fieldnode += nodes.field_argument(arg, arg)
        fieldbody = nodes.field_body('\n'.join(indented))
        fieldnode += fieldbody
        if indented:
            self.nested_parse(indented, input_offset=line_offset,
                              node=fieldbody)
        return fieldnode, blank_finish

    def parse_field_marker(self, match):
        """Extract & return name & argument list from a field marker match."""
        field = match.string[1:]        # strip off leading ':'
        field = field[:field.find(':')] # strip off trailing ':' etc.
        tokens = field.split()
        return tokens[0], tokens[1:]    # first == name, others == args

    def option_marker(self, match, context, next_state):
        """Option list item."""
        optionlist = nodes.option_list()
        try:
            listitem, blank_finish = self.option_list_item(match)
        except MarkupError, detail:   # shouldn't happen; won't match pattern
            msg = self.reporter.error(
                  ('Invalid option list marker at line %s: %s'
                      % (self.state_machine.abs_line_number(), detail)))
            self.parent += msg
            indented, indent, line_offset, blank_finish = \
                  self.state_machine.get_first_known_indented(match.end())
            blockquote = self.block_quote(indented, line_offset)
            self.parent += blockquote
            if not blank_finish:
                self.parent += self.unindent_warning('Option list')
            return [], next_state, []
        self.parent += optionlist
        optionlist += listitem
        offset = self.state_machine.line_offset + 1   # next line
        newline_offset, blank_finish = self.nested_list_parse(
              self.state_machine.input_lines[offset:],
              input_offset=self.state_machine.abs_line_offset() + 1,
              node=optionlist, initial_state='OptionList',
              blank_finish=blank_finish)
        self.goto_line(newline_offset)
        if not blank_finish:
            self.parent += self.unindent_warning('Option list')
        return [], next_state, []

    def option_list_item(self, match):
        options = self.parse_option_marker(match)
        indented, indent, line_offset, blank_finish = \
              self.state_machine.get_first_known_indented(match.end())
        if not indented:                # not an option list item
            raise statemachine.TransitionCorrection('text')
        option_group = nodes.option_group('', *options)
        description = nodes.description('\n'.join(indented))
        option_list_item = nodes.option_list_item('', option_group,
                                                  description)
        if indented:
            self.nested_parse(indented, input_offset=line_offset,
                              node=description)
        return option_list_item, blank_finish

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
                                  'should be 1 or 2: "%s"' % (len(tokens),
                                                              optionstring))
        return optlist

    def doctest(self, match, context, next_state):
        data = '\n'.join(self.state_machine.get_text_block())
        self.parent += nodes.doctest_block(data, data)
        return [], next_state, []

    def table_top(self, match, context, next_state):
        """Top border of a table."""
        nodelist, blank_finish = self.table()
        self.parent += nodelist
        if not blank_finish:
            msg = self.reporter.warning(
                  'Blank line required after table at line %s.'
                  % (self.state_machine.abs_line_number() + 1))
            self.parent += msg
        return [], next_state, []

    def table(self):
        """Parse a table."""
        block, messages, blank_finish = self.isolate_table()
        if block:
            try:
                tabledata = self.tableparser.parse(block)
                tableline = (self.state_machine.abs_line_number() - len(block)
                             + 1)
                table = self.build_table(tabledata, tableline)
                nodelist = [table] + messages
            except TableMarkupError, detail:
                nodelist = self.malformed_table(block, str(detail)) + messages
        else:
            nodelist = messages
        return nodelist, blank_finish

    def isolate_table(self):
        messages = []
        blank_finish = 1
        try:
            block = self.state_machine.get_text_block(flush_left=1)
        except statemachine.UnexpectedIndentationError, instance:
            block, lineno = instance.args
            messages.append(self.reporter.error(
                  'Unexpected indentation at line %s.' % lineno))
            blank_finish = 0
        width = len(block[0].strip())
        for i in range(len(block)):
            block[i] = block[i].strip()
            if block[i][0] not in '+|': # check left edge
                blank_finish = 0
                self.state_machine.previous_line(len(block) - i)
                del block[i:]
                break
        if not self.table_top_pat.match(block[-1]): # find bottom
            blank_finish = 0
            # from second-last to third line of table:
            for i in range(len(block) - 2, 1, -1):
                if self.table_top_pat.match(block[i]):
                    self.state_machine.previous_line(len(block) - i + 1)
                    del block[i+1:]
                    break
            else:
                messages.extend(self.malformed_table(block))
                return [], messages, blank_finish
        for i in range(len(block)):     # check right edge
            if len(block[i]) != width or block[i][-1] not in '+|':
                messages.extend(self.malformed_table(block))
                return [], messages, blank_finish
        return block, messages, blank_finish

    def malformed_table(self, block, detail=''):
        data = '\n'.join(block)
        message = 'Malformed table at line %s; formatting as a ' \
                  'literal block.' % (self.state_machine.abs_line_number()
                                      - len(block) + 1)
        if detail:
            message += '\n' + detail
        nodelist = [self.reporter.error(message),
                    nodes.literal_block(data, data)]
        return nodelist

    def build_table(self, tabledata, tableline):
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
                thead += self.build_table_row(row, tableline)
        tbody = nodes.tbody()
        tgroup += tbody
        for row in bodyrows:
            tbody += self.build_table_row(row, tableline)
        return table

    def build_table_row(self, rowdata, tableline):
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
                self.nested_parse(cellblock, input_offset=tableline+offset,
                                  node=entry)
        return row


    explicit = Stuff()
    """Patterns and constants used for explicit markup recognition."""

    explicit.patterns = Stuff(
          target=re.compile(r"""
                            (
                              _               # anonymous target
                            |               # *OR*
                              (?P<quote>`?)   # optional open quote
                              (?![ `])        # first char. not space or
                                              # backquote
                              (?P<name>       # reference name
                                .+?
                              )
                              %(non_whitespace_escape_before)s
                              (?P=quote)      # close quote if open quote used
                            )
                            %(non_whitespace_escape_before)s
                            :               # end of reference name
                            ([ ]+|$)        # followed by whitespace
                            """ % vars(Inliner), re.VERBOSE),
          reference=re.compile(r"""
                               (
                                 (?P<simple>%(simplename)s)_
                               |                  # *OR*
                                 `                  # open backquote
                                 (?![ ])            # not space
                                 (?P<phrase>.+?)    # hyperlink phrase
                                 %(non_whitespace_escape_before)s
                                 `_                 # close backquote,
                                                    # reference mark
                               )
                               $                  # end of string
                               """ % vars(Inliner), re.VERBOSE),
          substitution=re.compile(r"""
                                  (
                                    (?![ ])          # first char. not space
                                    (?P<name>.+?)    # substitution text
                                    %(non_whitespace_escape_before)s
                                    \|               # close delimiter
                                  )
                                  ([ ]+|$)           # followed by whitespace
                                  """ % vars(Inliner), re.VERBOSE),)

    def footnote(self, match):
        indented, indent, offset, blank_finish = \
              self.state_machine.get_first_known_indented(match.end())
        label = match.group(1)
        name = normalize_name(label)
        footnote = nodes.footnote('\n'.join(indented))
        if name[0] == '#':              # auto-numbered
            name = name[1:]             # autonumber label
            footnote['auto'] = 1
            if name:
                footnote['name'] = name
            self.document.note_autofootnote(footnote)
        elif name == '*':               # auto-symbol
            name = ''
            footnote['auto'] = '*'
            self.document.note_symbol_footnote(footnote)
        else:                           # manually numbered
            footnote += nodes.label('', label)
            footnote['name'] = name
            self.document.note_footnote(footnote)
        if name:
            self.document.note_explicit_target(footnote, footnote)
        else:
            self.document.set_id(footnote)
        if indented:
            self.nested_parse(indented, input_offset=offset, node=footnote)
        return [footnote], blank_finish

    def citation(self, match):
        indented, indent, offset, blank_finish = \
              self.state_machine.get_first_known_indented(match.end())
        label = match.group(1)
        name = normalize_name(label)
        citation = nodes.citation('\n'.join(indented))
        citation += nodes.label('', label)
        citation['name'] = name
        self.document.note_citation(citation)
        self.document.note_explicit_target(citation, citation)
        if indented:
            self.nested_parse(indented, input_offset=offset, node=citation)
        return [citation], blank_finish

    def hyperlink_target(self, match):
        pattern = self.explicit.patterns.target
        lineno = self.state_machine.abs_line_number()
        block, indent, offset, blank_finish = \
              self.state_machine.get_first_known_indented(
              match.end(), until_blank=1, strip_indent=0)
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
            refname = self.is_reference(reference)
            if refname:
                target = nodes.target(blocktext, '', refname=refname)
                self.add_target(targetmatch.group('name'), '', target)
                self.document.note_indirect_target(target)
                return [target], blank_finish
        nodelist = []
        reference = ''.join([line.strip() for line in block])
        if reference.find(' ') != -1:
            warning = self.reporter.warning(
                  'Hyperlink target at line %s contains whitespace. '
                  'Perhaps a footnote was intended?'
                  % (self.state_machine.abs_line_number() - len(block) + 1),
                  '', nodes.literal_block(blocktext, blocktext))
            nodelist.append(warning)
        else:
            unescaped = unescape(reference)
            target = nodes.target(blocktext, '')
            self.add_target(targetmatch.group('name'), unescaped, target)
            nodelist.append(target)
        return nodelist, blank_finish

    def is_reference(self, reference):
        match = self.explicit.patterns.reference.match(
            normalize_name(reference))
        if not match:
            return None
        return unescape(match.group('simple') or match.group('phrase'))

    def add_target(self, targetname, refuri, target):
        if targetname:
            name = normalize_name(unescape(targetname))
            target['name'] = name
            if refuri:
                target['refuri'] = refuri
                self.document.note_external_target(target)
            else:
                self.document.note_internal_target(target)
            self.document.note_explicit_target(target, self.parent)
        else:                       # anonymous target
            if refuri:
                target['refuri'] = refuri
            target['anonymous'] = 1
            self.document.note_anonymous_target(target)

    def substitution_def(self, match):
        pattern = self.explicit.patterns.substitution
        lineno = self.state_machine.abs_line_number()
        block, indent, offset, blank_finish = \
              self.state_machine.get_first_known_indented(match.end(),
                                                          strip_indent=0)
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
        subname = subdefmatch.group('name')
        name = normalize_name(subname)
        substitutionnode = nodes.substitution_definition(
              blocktext, name=name, alt=subname)
        if block:
            block[0] = block[0].strip()
            newabsoffset, blank_finish = self.nested_list_parse(
                  block, input_offset=offset, node=substitutionnode,
                  initial_state='SubstitutionDef', blank_finish=blank_finish)
            self.state_machine.previous_line(
                  len(block) + offset - newabsoffset - 1)
            i = 0
            for node in substitutionnode[:]:
                if not (isinstance(node, nodes.Inline) or
                        isinstance(node, nodes.Text)):
                    self.parent += substitutionnode[i]
                    del substitutionnode[i]
                else:
                    i += 1
            if len(substitutionnode) == 0:
                msg = self.reporter.warning(
                      'Substitution definition "%s" empty or invalid at line '
                      '%s.' % (subname, self.state_machine.abs_line_number()),
                      '', nodes.literal_block(blocktext, blocktext))
                self.parent += msg
            else:
                del substitutionnode['alt']
                self.document.note_substitution_def(
                      substitutionnode, self.parent)
                return [substitutionnode], blank_finish
        else:
            msg = self.reporter.warning(
                  'Substitution definition "%s" missing contents at line %s.'
                  % (subname, self.state_machine.abs_line_number()), '',
                  nodes.literal_block(blocktext, blocktext))
            self.parent += msg
        return [], blank_finish

    def directive(self, match, **attributes):
        type_name = match.group(1)
        directivefunction = directives.directive(type_name,
                                                 self.memo.language)
        data = match.string[match.end():].strip()
        if directivefunction:
            return directivefunction(match, type_name, data, self,
                                     self.state_machine, attributes)
        else:
            return self.unknown_directive(type_name, data)

    def unknown_directive(self, type_name, data):
        lineno = self.state_machine.abs_line_number()
        indented, indent, offset, blank_finish = \
              self.state_machine.get_first_known_indented(0, strip_indent=0)
        text = '\n'.join(indented)
        error = self.reporter.error(
              'Unknown directive type "%s" at line %s.' % (type_name, lineno),
              '', nodes.literal_block(text, text))
        return [error], blank_finish

    def parse_extension_attributes(self, attribute_spec, datalines,
                                   blank_finish):
        """
        Parse `datalines` for a field list containing extension attributes
        matching `attribute_spec`.

        :Parameters:
            - `attribute_spec`: a mapping of attribute name to conversion
              function, which should raise an exception on bad input.
            - `datalines`: a list of input strings.
            - `blank_finish`:

        :Return:
            - Success value, 1 or 0.
            - An attribute dictionary on success, an error string on failure.
            - Updated `blank_finish` flag.
        """
        node = nodes.field_list()
        newline_offset, blank_finish = self.nested_list_parse(
              datalines, 0, node, initial_state='FieldList',
              blank_finish=blank_finish)
        if newline_offset != len(datalines): # incomplete parse of block
            return 0, 'invalid attribute block', blank_finish
        try:
            attributes = utils.extract_extension_attributes(node,
                                                            attribute_spec)
        except KeyError, detail:
            return 0, ('unknown attribute: "%s"' % detail), blank_finish
        except (ValueError, TypeError), detail:
            return 0, ('invalid attribute value:\n%s' % detail), blank_finish
        except utils.ExtensionAttributeError, detail:
            return 0, ('invalid attribute data: %s' % detail), blank_finish
        return 1, attributes, blank_finish

    def comment(self, match):
        if not match.string[match.end():].strip() \
              and self.state_machine.is_next_line_blank(): # an empty comment?
            return [nodes.comment()], 1 # "A tiny but practical wart."
        indented, indent, offset, blank_finish = \
              self.state_machine.get_first_known_indented(match.end())
        text = '\n'.join(indented)
        return [nodes.comment(text, text)], blank_finish

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
                      ([ ]+|$)          # whitespace or end of line
                      """ % Inliner.simplename, re.VERBOSE)),
          (citation,
           re.compile(r"""
                      \.\.[ ]+          # explicit markup start
                      \[(%s)\]          # citation label
                      ([ ]+|$)          # whitespace or end of line
                      """ % Inliner.simplename, re.VERBOSE)),
          (hyperlink_target,
           re.compile(r"""
                      \.\.[ ]+          # explicit markup start
                      _                 # target indicator
                      (?![ ])           # first char. not space
                      """, re.VERBOSE)),
          (substitution_def,
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
                      ([ ]+|$)          # whitespace or end of line
                      """ % Inliner.simplename, re.VERBOSE))]

    def explicit_markup(self, match, context, next_state):
        """Footnotes, hyperlink targets, directives, comments."""
        nodelist, blank_finish = self.explicit_construct(match)
        self.parent += nodelist
        self.explicit_list(blank_finish)
        return [], next_state, []

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
                          self.reporter.warning('%s: %s'
                          % (detail.__class__.__name__, detail)))
                    break
        nodelist, blank_finish = self.comment(match)
        return nodelist + errors, blank_finish

    def explicit_list(self, blank_finish):
        """
        Create a nested state machine for a series of explicit markup
        constructs (including anonymous hyperlink targets).
        """
        offset = self.state_machine.line_offset + 1   # next line
        newline_offset, blank_finish = self.nested_list_parse(
              self.state_machine.input_lines[offset:],
              input_offset=self.state_machine.abs_line_offset() + 1,
              node=self.parent, initial_state='Explicit',
              blank_finish=blank_finish)
        self.goto_line(newline_offset)
        if not blank_finish:
            self.parent += self.unindent_warning('Explicit markup')

    def anonymous(self, match, context, next_state):
        """Anonymous hyperlink targets."""
        nodelist, blank_finish = self.anonymous_target(match)
        self.parent += nodelist
        self.explicit_list(blank_finish)
        return [], next_state, []

    def anonymous_target(self, match):
        block, indent, offset, blank_finish \
              = self.state_machine.get_first_known_indented(match.end(),
                                                            until_blank=1)
        blocktext = match.string[:match.end()] + '\n'.join(block)
        if block and block[-1].strip()[-1:] == '_': # possible indirect target
            reference = escape2null(' '.join([line.strip()
                                              for line in block]))
            refname = self.is_reference(reference)
            if refname:
                target = nodes.target(blocktext, '', refname=refname,
                                      anonymous=1)
                self.document.note_anonymous_target(target)
                self.document.note_indirect_target(target)
                return [target], blank_finish
        nodelist = []
        reference = escape2null(''.join([line.strip() for line in block]))
        if reference.find(' ') != -1:
            warning = self.reporter.warning(
                  'Anonymous hyperlink target at line %s contains '
                  'whitespace. Perhaps a footnote was intended?'
                  % (self.state_machine.abs_line_number() - len(block) + 1),
                  '', nodes.literal_block(blocktext, blocktext))
            nodelist.append(warning)
        else:
            target = nodes.target(blocktext, '', anonymous=1)
            if reference:
                unescaped = unescape(reference)
                target['refuri'] = unescaped
            self.document.note_anonymous_target(target)
            nodelist.append(target)
        return nodelist, blank_finish

    def line(self, match, context, next_state):
        """Section title overline or transition marker."""
        if self.state_machine.match_titles:
            return [match.string], 'Line', []
        else:
            blocktext = self.state_machine.line
            msg = self.reporter.severe(
                  'Unexpected section title or transition at line %s.'
                  % self.state_machine.abs_line_number(), '',
                  nodes.literal_block(blocktext, blocktext))
            self.parent += msg
            return [], next_state, []

    def text(self, match, context, next_state):
        """Titles, definition lists, paragraphs."""
        return [match.string], 'Text', []


class RFC2822Body(Body):

    """
    RFC2822 headers are only valid as the first constructs in documents.  As
    soon as anything else appears, the `Body` state should take over.
    """

    patterns = Body.patterns.copy()     # can't modify the original
    patterns['rfc2822'] = r'[!-9;-~]+:( +|$)'
    initial_transitions = [(name, 'Body')
                           for name in Body.initial_transitions]
    initial_transitions.insert(-1, ('rfc2822', 'Body')) # just before 'text'

    def rfc2822(self, match, context, next_state):
        """RFC2822-style field list item."""
        fieldlist = nodes.field_list(CLASS='rfc2822')
        self.parent += fieldlist
        field, blank_finish = self.rfc2822_field(match)
        fieldlist += field
        offset = self.state_machine.line_offset + 1   # next line
        newline_offset, blank_finish = self.nested_list_parse(
              self.state_machine.input_lines[offset:],
              input_offset=self.state_machine.abs_line_offset() + 1,
              node=fieldlist, initial_state='RFC2822List',
              blank_finish=blank_finish)
        self.goto_line(newline_offset)
        if not blank_finish:
            self.parent += self.unindent_warning(
                  'RFC2822-style field list')
        return [], next_state, []

    def rfc2822_field(self, match):
        name = match.string[:match.string.find(':')]
        indented, indent, line_offset, blank_finish = \
              self.state_machine.get_first_known_indented(match.end())
        fieldnode = nodes.field()
        fieldnode += nodes.field_name(name, name)
        fieldbody = nodes.field_body('\n'.join(indented))
        fieldnode += fieldbody
        if indented:
            self.nested_parse(indented, input_offset=line_offset,
                              node=fieldbody)
        return fieldnode, blank_finish


class SpecializedBody(Body):

    """
    Superclass for second and subsequent compound element members.

    All transition methods are disabled. Override individual methods in
    subclasses to re-enable.
    """

    def invalid_input(self, match=None, context=None, next_state=None):
        """Not a compound element member. Abort this state machine."""
        self.state_machine.previous_line() # back up so parent SM can reassess
        raise EOFError

    indent = invalid_input
    bullet = invalid_input
    enumerator = invalid_input
    field_marker = invalid_input
    option_marker = invalid_input
    doctest = invalid_input
    table_top = invalid_input
    explicit_markup = invalid_input
    anonymous = invalid_input
    line = invalid_input
    text = invalid_input


class BulletList(SpecializedBody):

    """Second and subsequent bullet_list list_items."""

    def bullet(self, match, context, next_state):
        """Bullet list item."""
        if match.string[0] != self.parent['bullet']:
            # different bullet: new list
            self.invalid_input()
        listitem, blank_finish = self.list_item(match.end())
        self.parent += listitem
        self.blank_finish = blank_finish
        return [], 'BulletList', []


class DefinitionList(SpecializedBody):

    """Second and subsequent definition_list_items."""

    def text(self, match, context, next_state):
        """Definition lists."""
        return [match.string], 'Definition', []


class EnumeratedList(SpecializedBody):

    """Second and subsequent enumerated_list list_items."""

    def enumerator(self, match, context, next_state):
        """Enumerated list item."""
        format, sequence, text, ordinal = self.parse_enumerator(
              match, self.parent['enumtype'])
        if (sequence != self.parent['enumtype'] or
            format != self.format or
            ordinal != self.lastordinal + 1):
            # different enumeration: new list
            self.invalid_input()
        listitem, blank_finish = self.list_item(match.end())
        self.parent += listitem
        self.blank_finish = blank_finish
        self.lastordinal = ordinal
        return [], 'EnumeratedList', []


class FieldList(SpecializedBody):

    """Second and subsequent field_list fields."""

    def field_marker(self, match, context, next_state):
        """Field list field."""
        field, blank_finish = self.field(match)
        self.parent += field
        self.blank_finish = blank_finish
        return [], 'FieldList', []


class OptionList(SpecializedBody):

    """Second and subsequent option_list option_list_items."""

    def option_marker(self, match, context, next_state):
        """Option list item."""
        try:
            option_list_item, blank_finish = self.option_list_item(match)
        except MarkupError, detail:
            self.invalid_input()
        self.parent += option_list_item
        self.blank_finish = blank_finish
        return [], 'OptionList', []


class RFC2822List(SpecializedBody, RFC2822Body):

    """Second and subsequent RFC2822-style field_list fields."""

    patterns = RFC2822Body.patterns
    initial_transitions = RFC2822Body.initial_transitions

    def rfc2822(self, match, context, next_state):
        """RFC2822-style field list item."""
        field, blank_finish = self.rfc2822_field(match)
        self.parent += field
        self.blank_finish = blank_finish
        return [], 'RFC2822List', []

    blank = SpecializedBody.invalid_input


class Explicit(SpecializedBody):

    """Second and subsequent explicit markup construct."""

    def explicit_markup(self, match, context, next_state):
        """Footnotes, hyperlink targets, directives, comments."""
        nodelist, blank_finish = self.explicit_construct(match)
        self.parent += nodelist
        self.blank_finish = blank_finish
        return [], next_state, []

    def anonymous(self, match, context, next_state):
        """Anonymous hyperlink targets."""
        nodelist, blank_finish = self.anonymous_target(match)
        self.parent += nodelist
        self.blank_finish = blank_finish
        return [], next_state, []


class SubstitutionDef(Body):

    """
    Parser for the contents of a substitution_definition element.
    """

    patterns = {
          'embedded_directive': r'(%s)::( +|$)' % Inliner.simplename,
          'text': r''}
    initial_transitions = ['embedded_directive', 'text']

    def embedded_directive(self, match, context, next_state):
        if self.parent.has_key('alt'):
            attributes = {'alt': self.parent['alt']}
        else:
            attributes = {}
        nodelist, blank_finish = self.directive(match, **attributes)
        self.parent += nodelist
        if not self.state_machine.at_eof():
            self.blank_finish = blank_finish
        raise EOFError

    def text(self, match, context, next_state):
        if not self.state_machine.at_eof():
            self.blank_finish = self.state_machine.is_next_line_blank()
        raise EOFError


class Text(RSTState):

    """
    Classifier of second line of a text block.

    Could be a paragraph, a definition list item, or a title.
    """

    patterns = {'underline': Body.patterns['line'],
                'text': r''}
    initial_transitions = [('underline', 'Body'), ('text', 'Body')]

    def blank(self, match, context, next_state):
        """End of paragraph."""
        paragraph, literalnext = self.paragraph(
              context, self.state_machine.abs_line_number() - 1)
        self.parent += paragraph
        if literalnext:
            self.parent += self.literal_block()
        return [], 'Body', []

    def eof(self, context):
        if context:
            paragraph, literalnext = self.paragraph(
                  context, self.state_machine.abs_line_number() - 1)
            self.parent += paragraph
            if literalnext:
                self.parent += self.literal_block()
        return []

    def indent(self, match, context, next_state):
        """Definition list item."""
        definitionlist = nodes.definition_list()
        definitionlistitem, blank_finish = self.definition_list_item(context)
        definitionlist += definitionlistitem
        self.parent += definitionlist
        offset = self.state_machine.line_offset + 1   # next line
        newline_offset, blank_finish = self.nested_list_parse(
              self.state_machine.input_lines[offset:],
              input_offset=self.state_machine.abs_line_offset() + 1,
              node=definitionlist, initial_state='DefinitionList',
              blank_finish=blank_finish, blank_finish_state='Definition')
        self.goto_line(newline_offset)
        if not blank_finish:
            self.parent += self.unindent_warning('Definition list')
        return [], 'Body', []

    def underline(self, match, context, next_state):
        """Section title."""
        lineno = self.state_machine.abs_line_number()
        if not self.state_machine.match_titles:
            blocktext = context[0] + '\n' + self.state_machine.line
            msg = self.reporter.severe(
                  'Unexpected section title at line %s.' % lineno, '',
                  nodes.literal_block(blocktext, blocktext))
            self.parent += msg
            return [], next_state, []
        title = context[0].rstrip()
        underline = match.string.rstrip()
        source = title + '\n' + underline
        if len(title) > len(underline):
            blocktext = context[0] + '\n' + self.state_machine.line
            msg = self.reporter.info(
                  'Title underline too short at line %s.' % lineno, '',
                  nodes.literal_block(blocktext, blocktext))
            self.parent += msg
        style = underline[0]
        context[:] = []
        self.section(title, source, style, lineno - 1)
        return [], next_state, []

    def text(self, match, context, next_state):
        """Paragraph."""
        startline = self.state_machine.abs_line_number() - 1
        msg = None
        try:
            block = self.state_machine.get_text_block(flush_left=1)
        except statemachine.UnexpectedIndentationError, instance:
            block, lineno = instance.args
            msg = self.reporter.error(
                  'Unexpected indentation at line %s.' % lineno)
        lines = context + block
        paragraph, literalnext = self.paragraph(lines, startline)
        self.parent += paragraph
        self.parent += msg
        if literalnext:
            try:
                self.state_machine.next_line()
            except IndexError:
                pass
            self.parent += self.literal_block()
        return [], next_state, []

    def literal_block(self):
        """Return a list of nodes."""
        indented, indent, offset, blank_finish = \
              self.state_machine.get_indented()
        nodelist = []
        while indented and not indented[-1].strip():
            indented.pop()
        if indented:
            data = '\n'.join(indented)
            nodelist.append(nodes.literal_block(data, data))
            if not blank_finish:
                nodelist.append(self.unindent_warning('Literal block'))
        else:
            nodelist.append(self.reporter.warning(
                  'Literal block expected at line %s; none found.'
                  % self.state_machine.abs_line_number()))
        return nodelist

    def definition_list_item(self, termline):
        indented, indent, line_offset, blank_finish = \
              self.state_machine.get_indented()
        definitionlistitem = nodes.definition_list_item('\n'.join(termline
                                                                  + indented))
        termlist, messages = self.term(
              termline, self.state_machine.abs_line_number() - 1)
        definitionlistitem += termlist
        definition = nodes.definition('', *messages)
        definitionlistitem += definition
        if termline[0][-2:] == '::':
            definition += self.reporter.info(
                  'Blank line missing before literal block? Interpreted as a '
                  'definition list item. At line %s.' % (line_offset + 1))
        self.nested_parse(indented, input_offset=line_offset, node=definition)
        return definitionlistitem, blank_finish

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

    def invalid_input(self, match=None, context=None, next_state=None):
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
        self.state_machine.previous_line(2) # so parent SM can reassess
        return []

    def indent(self, match, context, next_state):
        """Definition list item."""
        definitionlistitem, blank_finish = self.definition_list_item(context)
        self.parent += definitionlistitem
        self.blank_finish = blank_finish
        return [], 'DefinitionList', []


class Line(SpecializedText):

    """Second line of over- & underlined section title or transition marker."""

    eofcheck = 1                        # @@@ ???
    """Set to 0 while parsing sections, so that we don't catch the EOF."""

    def eof(self, context):
        """Transition marker at end of section or document."""
        if self.eofcheck:               # ignore EOFError with sections
            transition = nodes.transition(context[0])
            self.parent += transition
            msg = self.reporter.error(
                  'Document or section may not end with a transition '
                  '(line %s).' % (self.state_machine.abs_line_number() - 1))
            self.parent += msg
        self.eofcheck = 1
        return []

    def blank(self, match, context, next_state):
        """Transition marker."""
        transition = nodes.transition(context[0])
        if len(self.parent) == 0:
            msg = self.reporter.error(
                  'Document or section may not begin with a transition '
                  '(line %s).' % (self.state_machine.abs_line_number() - 1))
            self.parent += msg
        elif isinstance(self.parent[-1], nodes.transition):
            msg = self.reporter.error(
                  'At least one body element must separate transitions; '
                  'adjacent transitions at line %s.'
                  % (self.state_machine.abs_line_number() - 1))
            self.parent += msg
        self.parent += transition
        return [], 'Body', []

    def text(self, match, context, next_state):
        """Potential over- & underlined title."""
        lineno = self.state_machine.abs_line_number() - 1
        overline = context[0]
        title = match.string
        underline = ''
        try:
            underline = self.state_machine.next_line()
        except IndexError:
            blocktext = overline + '\n' + title
            msg = self.reporter.severe(
                  'Incomplete section title at line %s.' % lineno, '',
                  nodes.literal_block(blocktext, blocktext))
            self.parent += msg
            return [], 'Body', []
        source = '%s\n%s\n%s' % (overline, title, underline)
        overline = overline.rstrip()
        underline = underline.rstrip()
        if not self.transitions['underline'][0].match(underline):
            msg = self.reporter.severe(
                  'Missing underline for overline at line %s.' % lineno, '',
                  nodes.literal_block(source, source))
            self.parent += msg
            return [], 'Body', []
        elif overline != underline:
            msg = self.reporter.severe(
                  'Title overline & underline mismatch at ' 'line %s.'
                   % lineno, '', nodes.literal_block(source, source))
            self.parent += msg
            return [], 'Body', []
        title = title.rstrip()
        if len(title) > len(overline):
            msg = self.reporter.info(
                  'Title overline too short at line %s.'% lineno, '',
                  nodes.literal_block(source, source))
            self.parent += msg
        style = (overline[0], underline[0])
        self.eofcheck = 0               # @@@ not sure this is correct
        self.section(title.lstrip(), source, style, lineno + 1)
        self.eofcheck = 1
        return [], 'Body', []

    indent = text                       # indented title

    def underline(self, match=None, context=None, next_state=None):
        blocktext = context[0] + '\n' + self.state_machine.line
        msg = self.reporter.error(
              'Invalid section title or transition marker at line %s.'
              % (self.state_machine.abs_line_number() - 1), '',
              nodes.literal_block(blocktext, blocktext))
        self.parent += msg
        return [], 'Body', []


state_classes = (Body, BulletList, DefinitionList, EnumeratedList, FieldList,
                 OptionList, Explicit, Text, Definition, Line,
                 SubstitutionDef, RFC2822Body, RFC2822List)
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

def unescape(text, restore_backslashes=0):
    """Return a string with nulls removed or restored to backslashes."""
    if restore_backslashes:
        return text.replace('\x00', '\\')
    else:
        return ''.join(text.split('\x00'))
