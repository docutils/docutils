# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
A finite state machine specialized for regular-expression-based text filters,
this module defines the following classes:

- `StateMachine`, a state machine
- `State`, a state superclass
- `StateMachineWS`, a whitespace-sensitive version of `StateMachine`
- `StateWS`, a state superclass for use with `StateMachineWS`
- `SearchStateMachine`, uses `re.search()` instead of `re.match()`
- `SearchStateMachineWS`, uses `re.search()` instead of `re.match()`

Exception classes:

- `StateMachineError`
- `UnknownStateError`
- `DuplicateStateError`
- `UnknownTransitionError`
- `DuplicateTransitionError`
- `TransitionPatternNotFound`
- `TransitionMethodNotFound`
- `UnexpectedIndentationError`
- `TransitionCorrection`: Raised to switch to another transition.
- `StateCorrection`: Raised to switch to another state & transition.

Functions:

- `string2lines()`: split a multi-line string into a list of one-line strings
- `extract_indented()`: return indented lines with minimum indentation removed

How To Use This Module
======================
(See the individual classes, methods, and attributes for details.)

1. Import it: ``import statemachine`` or ``from statemachine import ...``.
   You will also need to ``import re``.

2. Derive a subclass of `State` (or `StateWS`) for each state in your state
   machine::

       class MyState(statemachine.State):

   Within the state's class definition:

   a) Include a pattern for each transition, in `State.patterns`::

          patterns = {'atransition': r'pattern', ...}

   b) Include a list of initial transitions to be set up automatically, in
      `State.initial_transitions`::

          initial_transitions = ['atransition', ...]

   c) Define a method for each transition, with the same name as the
      transition pattern::

          def atransition(self, match, context, next_state):
              # do something
              result = [...]  # a list
              return context, next_state, result
              # context, next_state may be altered

      Transition methods may raise an `EOFError` to cut processing short.

   d) You may wish to override the `State.bof()` and/or `State.eof()` implicit
      transition methods, which handle the beginning- and end-of-file.

   e) In order to handle nested processing, you may wish to override the
      attributes `State.nested_sm` and/or `State.nested_sm_kwargs`.

      If you are using `StateWS` as a base class, in order to handle nested
      indented blocks, you may wish to:

      - override the attributes `StateWS.indent_sm`,
        `StateWS.indent_sm_kwargs`, `StateWS.known_indent_sm`, and/or
        `StateWS.known_indent_sm_kwargs`;
      - override the `StateWS.blank()` method; and/or
      - override or extend the `StateWS.indent()`, `StateWS.known_indent()`,
        and/or `StateWS.firstknown_indent()` methods.

3. Create a state machine object::

       sm = StateMachine(state_classes=[MyState, ...],
                         initial_state='MyState')

4. Obtain the input text, which needs to be converted into a tab-free list of
   one-line strings. For example, to read text from a file called
   'inputfile'::

       input_string = open('inputfile').read()
       input_lines = statemachine.string2lines(input_string)

5. Run the state machine on the input text and collect the results, a list::

       results = sm.run(input_lines)

6. Remove any lingering circular references::

       sm.unlink()
"""

__docformat__ = 'restructuredtext'

import sys
import re


class StateMachine:

    """
    A finite state machine for text filters using regular expressions.

    The input is provided in the form of a list of one-line strings (no
    newlines). States are subclasses of the `State` class. Transitions consist
    of regular expression patterns and transition methods, and are defined in
    each state.

    The state machine is started with the `run()` method, which returns the
    results of processing in a list.
    """

    def __init__(self, state_classes, initial_state, debug=0):
        """
        Initialize a `StateMachine` object; add state objects.

        Parameters:

        - `state_classes`: a list of `State` (sub)classes.
        - `initial_state`: a string, the class name of the initial state.
        - `debug`: a boolean; produce verbose output if true (nonzero).
        """

        self.input_lines = None
        """List of strings (without newlines). Filled by `self.run()`."""

        self.input_offset = 0
        """Offset of `self.input_lines` from the beginning of the file."""

        self.line = None
        """Current input line."""

        self.line_offset = -1
        """Current input line offset from beginning of `self.input_lines`."""

        self.debug = debug
        """Debugging mode on/off."""

        self.initial_state = initial_state
        """The name of the initial state (key to `self.states`)."""

        self.current_state = initial_state
        """The name of the current state (key to `self.states`)."""

        self.states = {}
        """Mapping of {state_name: State_object}."""

        self.add_states(state_classes)

        self.observers = []
        """List of bound methods or functions to call whenever the current
        line changes.  Observers are called with one argument, ``self``.
        Cleared at the end of `run()`."""

    def unlink(self):
        """Remove circular references to objects no longer required."""
        for state in self.states.values():
            state.unlink()
        self.states = None

    def run(self, input_lines, input_offset=0, context=None):
        """
        Run the state machine on `input_lines`. Return results (a list).

        Reset `self.line_offset` and `self.current_state`. Run the
        beginning-of-file transition. Input one line at a time and check for a
        matching transition. If a match is found, call the transition method
        and possibly change the state. Store the context returned by the
        transition method to be passed on to the next transition matched.
        Accumulate the results returned by the transition methods in a list.
        Run the end-of-file transition. Finally, return the accumulated
        results.

        Parameters:

        - `input_lines`: a list of strings without newlines.
        - `input_offset`: the line offset of `input_lines` from the beginning
          of the file.
        - `context`: application-specific storage.
        """
        self.runtime_init()
        self.input_lines = input_lines
        self.input_offset = input_offset
        self.line_offset = -1
        self.current_state = self.initial_state
        if self.debug:
            print >>sys.stderr, ('\nStateMachine.run: input_lines:\n| %s' %
                                 '\n| '.join(self.input_lines))
        transitions = None
        results = []
        state = self.get_state()
        try:
            if self.debug:
                print >>sys.stderr, ('\nStateMachine.run: bof transition')
            context, result = state.bof(context)
            results.extend(result)
            while 1:
                try:
                    try:
                        self.next_line()
                        if self.debug:
                            print >>sys.stderr, ('\nStateMachine.run: line:\n'
                                                 '| %s' % self.line)
                        context, next_state, result = self.check_line(
                            context, state, transitions)
                    except EOFError:
                        if self.debug:
                            print >>sys.stderr, (
                                '\nStateMachine.run: %s.eof transition'
                                % state.__class__.__name__)
                        result = state.eof(context)
                        results.extend(result)
                        break
                    else:
                        results.extend(result)
                except TransitionCorrection, exception:
                    self.previous_line() # back up for another try
                    transitions = (exception.args[0],)
                    if self.debug:
                        print >>sys.stderr, (
                              '\nStateMachine.run: TransitionCorrection to '
                              'state "%s", transition %s.'
                              % (state.__class__.__name, transitions[0]))
                    continue
                except StateCorrection, exception:
                    self.previous_line() # back up for another try
                    next_state = exception.args[0]
                    if len(exception.args) == 1:
                        transitions = None
                    else:
                        transitions = (exception.args[1],)
                    if self.debug:
                        print >>sys.stderr, (
                              '\nStateMachine.run: StateCorrection to state '
                              '"%s", transition %s.'
                              % (next_state, transitions[0]))
                else:
                    transitions = None
                state = self.get_state(next_state)
        except:
            self.error()
            raise
        self.observers = []
        return results

    def get_state(self, next_state=None):
        """
        Return current state object; set it first if `next_state` given.

        Parameter `next_state`: a string, the name of the next state.

        Exception: `UnknownStateError` raised if `next_state` unknown.
        """
        if next_state:
            if self.debug and next_state != self.current_state:
                print >>sys.stderr, \
                      ('\nStateMachine.get_state: Changing state from '
                       '"%s" to "%s" (input line %s).'
                       % (self.current_state, next_state,
                          self.abs_line_number()))
            self.current_state = next_state
        try:
            return self.states[self.current_state]
        except KeyError:
            raise UnknownStateError(self.current_state)

    def next_line(self, n=1):
        """Load `self.line` with the `n`'th next line and return it."""
        try:
            try:
                self.line_offset += n
                self.line = self.input_lines[self.line_offset]
            except IndexError:
                self.line = None
                raise EOFError
            return self.line
        finally:
            self.notify_observers()

    def is_next_line_blank(self):
        """Return 1 if the next line is blank or non-existant."""
        try:
            return not self.input_lines[self.line_offset + 1].strip()
        except IndexError:
            return 1

    def at_eof(self):
        """Return 1 if the input is at or past end-of-file."""
        return self.line_offset >= len(self.input_lines) - 1

    def at_bof(self):
        """Return 1 if the input is at or before beginning-of-file."""
        return self.line_offset <= 0

    def previous_line(self, n=1):
        """Load `self.line` with the `n`'th previous line and return it."""
        self.line_offset -= n
        if self.line_offset < 0:
            self.line = None
        else:
            self.line = self.input_lines[self.line_offset]
        self.notify_observers()
        return self.line

    def goto_line(self, line_offset):
        """Jump to absolute line offset `line_offset`, load and return it."""
        try:
            try:
                self.line_offset = line_offset - self.input_offset
                self.line = self.input_lines[self.line_offset]
            except IndexError:
                self.line = None
                raise EOFError
            return self.line
        finally:
            self.notify_observers()

    def abs_line_offset(self):
        """Return line offset of current line, from beginning of file."""
        return self.line_offset + self.input_offset

    def abs_line_number(self):
        """Return line number of current line (counting from 1)."""
        return self.line_offset + self.input_offset + 1

    def get_text_block(self, flush_left=0):
        """
        Return a contiguous block of text.

        If `flush_left` is true, raise `UnexpectedIndentationError` if an
        indented line is encountered before the text block ends (with a blank
        line).
        """
        block = []
        for line in self.input_lines[self.line_offset:]:
            if not line.strip():
                break
            if flush_left and (line[0] == ' '):
                self.next_line(len(block) - 1) # advance to last line of block
                raise UnexpectedIndentationError(block,
                                                 self.abs_line_number() + 1)
            block.append(line)
        self.next_line(len(block) - 1)  # advance to last line of block
        return block

    def check_line(self, context, state, transitions=None):
        """
        Examine one line of input for a transition match & execute its method.

        Parameters:

        - `context`: application-dependent storage.
        - `state`: a `State` object, the current state.
        - `transitions`: an optional ordered list of transition names to try,
          instead of ``state.transition_order``.

        Return the values returned by the transition method:

        - context: possibly modified from the parameter `context`;
        - next state name (`State` subclass name);
        - the result output of the transition, a list.

        When there is no match, ``state.no_match()`` is called and its return
        value is returned.
        """
        if transitions is None:
            transitions =  state.transition_order
        state_correction = None
        if self.debug:
            print >>sys.stderr, (
                  '\nStateMachine.check_line: state="%s", transitions=%r.'
                  % (state.__class__.__name__, transitions))
        for name in transitions:
            pattern, method, next_state = state.transitions[name]
            if self.debug:
                print >>sys.stderr, (
                      '\nStateMachine.check_line: Trying transition "%s" '
                      'in state "%s".' % (name, state.__class__.__name__))
            match = self.match(pattern)
            if match:
                if self.debug:
                    print >>sys.stderr, (
                          '\nStateMachine.check_line: Matched transition '
                          '"%s" in state "%s".'
                          % (name, state.__class__.__name__))
                return method(match, context, next_state)
        else:
            return state.no_match(context, transitions)

    def match(self, pattern):
        """
        Return the result of a regular expression match.

        Parameter `pattern`: an `re` compiled regular expression.
        """
        return pattern.match(self.line)

    def add_state(self, state_class):
        """
        Initialize & add a `state_class` (`State` subclass) object.

        Exception: `DuplicateStateError` raised if `state_class` was already
        added.
        """
        statename = state_class.__name__
        if self.states.has_key(statename):
            raise DuplicateStateError(statename)
        self.states[statename] = state_class(self, self.debug)

    def add_states(self, state_classes):
        """
        Add `state_classes` (a list of `State` subclasses).
        """
        for state_class in state_classes:
            self.add_state(state_class)

    def runtime_init(self):
        """
        Initialize `self.states`.
        """
        for state in self.states.values():
            state.runtime_init()

    def error(self):
        """Report error details."""
        type, value, module, line, function = _exception_data()
        print >>sys.stderr, '%s: %s' % (type, value)
        print >>sys.stderr, 'input line %s' % (self.abs_line_number())
        print >>sys.stderr, ('module %s, line %s, function %s'
                             % (module, line, function))

    def attach_observer(self, observer):
        """
        The `observer` parameter is a function or bound method which takes one
        argument, ``self`` (this StateMachine object).
        """
        self.observers.append(observer)

    def detach_observer(self, observer):
        self.observers.remove(observer)

    def notify_observers(self):
        for observer in self.observers:
            observer(self)


class State:

    """
    State superclass. Contains a list of transitions, and transition methods.

    Transition methods all have the same signature. They take 3 parameters:

    - An `re` match object. ``match.string`` contains the matched input line,
      ``match.start()`` gives the start index of the match, and
      ``match.end()`` gives the end index.
    - A context object, whose meaning is application-defined (initial value
      ``None``). It can be used to store any information required by the state
      machine, and the retured context is passed on to the next transition
      method unchanged.
    - The name of the next state, a string, taken from the transitions list;
      normally it is returned unchanged, but it may be altered by the
      transition method if necessary.

    Transition methods all return a 3-tuple:

    - A context object, as (potentially) modified by the transition method.
    - The next state name (a return value of ``None`` means no state change).
    - The processing result, a list, which is accumulated by the state
      machine.

    Transition methods may raise an `EOFError` to cut processing short.

    There are two implicit transitions, and corresponding transition methods
    are defined: `bof()` handles the beginning-of-file, and `eof()` handles
    the end-of-file. These methods have non-standard signatures and return
    values. `bof()` returns the initial context and results, and may be used
    to return a header string, or do any other processing needed. `eof()`
    should handle any remaining context and wrap things up; it returns the
    final processing result.

    Typical applications need only subclass `State` (or a subclass), set the
    `patterns` and `initial_transitions` class attributes, and provide
    corresponding transition methods. The default object initialization will
    take care of constructing the list of transitions.
    """

    patterns = None
    """
    {Name: pattern} mapping, used by `make_transition()`. Each pattern may
    be a string or a compiled `re` pattern. Override in subclasses.
    """

    initial_transitions = None
    """
    A list of transitions to initialize when a `State` is instantiated.
    Each entry is either a transition name string, or a (transition name, next
    state name) pair. See `make_transitions()`. Override in subclasses.
    """

    nested_sm = None
    """
    The `StateMachine` class for handling nested processing.

    If left as ``None``, `nested_sm` defaults to the class of the state's
    controlling state machine. Override it in subclasses to avoid the default.
    """

    nested_sm_kwargs = None
    """
    Keyword arguments dictionary, passed to the `nested_sm` constructor.

    Two keys must have entries in the dictionary:

    - Key 'state_classes' must be set to a list of `State` classes.
    - Key 'initial_state' must be set to the name of the initial state class.

    If `nested_sm_kwargs` is left as ``None``, 'state_classes' defaults to the
    class of the current state, and 'initial_state' defaults to the name of
    the class of the current state. Override in subclasses to avoid the
    defaults.
    """

    def __init__(self, state_machine, debug=0):
        """
        Initialize a `State` object; make & add initial transitions.

        Parameters:

        - `statemachine`: the controlling `StateMachine` object.
        - `debug`: a boolean; produce verbose output if true (nonzero).
        """

        self.transition_order = []
        """A list of transition names in search order."""

        self.transitions = {}
        """
        A mapping of transition names to 3-tuples containing
        (compiled_pattern, transition_method, next_state_name). Initialized as
        an instance attribute dynamically (instead of as a class attribute)
        because it may make forward references to patterns and methods in this
        or other classes.
        """

        self.add_initial_transitions()

        self.state_machine = state_machine
        """A reference to the controlling `StateMachine` object."""

        self.debug = debug
        """Debugging mode on/off."""

        if self.nested_sm is None:
            self.nested_sm = self.state_machine.__class__
        if self.nested_sm_kwargs is None:
            self.nested_sm_kwargs = {'state_classes': [self.__class__],
                                     'initial_state': self.__class__.__name__}

    def runtime_init(self):
        """
        Initialize this `State` before running the state machine; called from
        `self.state_machine.run()`.
        """
        pass

    def unlink(self):
        """Remove circular references to objects no longer required."""
        self.state_machine = None

    def add_initial_transitions(self):
        """Make and add transitions listed in `self.initial_transitions`."""
        if self.initial_transitions:
            names, transitions = self.make_transitions(
                  self.initial_transitions)
            self.add_transitions(names, transitions)

    def add_transitions(self, names, transitions):
        """
        Add a list of transitions to the start of the transition list.

        Parameters:

        - `names`: a list of transition names.
        - `transitions`: a mapping of names to transition tuples.

        Exceptions: `DuplicateTransitionError`, `UnknownTransitionError`.
        """
        for name in names:
            if self.transitions.has_key(name):
                raise DuplicateTransitionError(name)
            if not transitions.has_key(name):
                raise UnknownTransitionError(name)
        self.transition_order[:0] = names
        self.transitions.update(transitions)

    def add_transition(self, name, transition):
        """
        Add a transition to the start of the transition list.

        Parameter `transition`: a ready-made transition 3-tuple.

        Exception: `DuplicateTransitionError`.
        """
        if self.transitions.has_key(name):
            raise DuplicateTransitionError(name)
        self.transition_order[:0] = [name]
        self.transitions[name] = transition

    def remove_transition(self, name):
        """
        Remove a transition by `name`.

        Exception: `UnknownTransitionError`.
        """
        try:
            del self.transitions[name]
            self.transition_order.remove(name)
        except:
            raise UnknownTransitionError(name)

    def make_transition(self, name, next_state=None):
        """
        Make & return a transition tuple based on `name`.

        This is a convenience function to simplify transition creation.

        Parameters:

        - `name`: a string, the name of the transition pattern & method. This
          `State` object must have a method called '`name`', and a dictionary
          `self.patterns` containing a key '`name`'.
        - `next_state`: a string, the name of the next `State` object for this
          transition. A value of ``None`` (or absent) implies no state change
          (i.e., continue with the same state).

        Exceptions: `TransitionPatternNotFound`, `TransitionMethodNotFound`.
        """
        if next_state is None:
            next_state = self.__class__.__name__
        try:
            pattern = self.patterns[name]
            if not hasattr(pattern, 'match'):
                pattern = re.compile(pattern)
        except KeyError:
            raise TransitionPatternNotFound(
                  '%s.patterns[%r]' % (self.__class__.__name__, name))
        try:
            method = getattr(self, name)
        except AttributeError:
            raise TransitionMethodNotFound(
                  '%s.%s' % (self.__class__.__name__, name))
        return (pattern, method, next_state)

    def make_transitions(self, name_list):
        """
        Return a list of transition names and a transition mapping.

        Parameter `name_list`: a list, where each entry is either a transition
        name string, or a 1- or 2-tuple (transition name, optional next state
        name).
        """
        stringtype = type('')
        names = []
        transitions = {}
        for namestate in name_list:
            if type(namestate) is stringtype:
                transitions[namestate] = self.make_transition(namestate)
                names.append(namestate)
            else:
                transitions[namestate[0]] = self.make_transition(*namestate)
                names.append(namestate[0])
        return names, transitions

    def no_match(self, context, transitions):
        """
        Called when there is no match from `StateMachine.check_line()`.

        Return the same values returned by transition methods:

        - context: unchanged;
        - next state name: ``None``;
        - empty result list.

        Override in subclasses to catch this event.
        """
        return context, None, []

    def bof(self, context):
        """
        Handle beginning-of-file. Return unchanged `context`, empty result.

        Override in subclasses.

        Parameter `context`: application-defined storage.
        """
        return context, []

    def eof(self, context):
        """
        Handle end-of-file. Return empty result.

        Override in subclasses.

        Parameter `context`: application-defined storage.
        """
        return []

    def nop(self, match, context, next_state):
        """
        A "do nothing" transition method.

        Return unchanged `context` & `next_state`, empty result. Useful for
        simple state changes (actionless transitions).
        """
        return context, next_state, []


class StateMachineWS(StateMachine):

    """
    `StateMachine` subclass specialized for whitespace recognition.

    There are three methods provided for extracting indented text blocks:
    
    - `get_indented()`: use when the indent is unknown.
    - `get_known_indented()`: use when the indent is known for all lines.
    - `get_first_known_indented()`: use when only the first line's indent is
      known.
    """

    def get_indented(self, until_blank=0, strip_indent=1):
        """
        Return a block of indented lines of text, and info.

        Extract an indented block where the indent is unknown for all lines.

        :Parameters:
            - `until_blank`: Stop collecting at the first blank line if true
              (1).
            - `strip_indent`: Strip common leading indent if true (1,
              default).

        :Return:
            - the indented block (a list of lines of text),
            - its indent,
            - its first line offset from BOF, and
            - whether or not it finished with a blank line.
        """
        offset = self.abs_line_offset()
        indented, indent, blank_finish = extract_indented(
              self.input_lines[self.line_offset:], until_blank, strip_indent)
        if indented:
            self.next_line(len(indented) - 1) # advance to last indented line
        while indented and not indented[0].strip():
            indented.pop(0)
            offset += 1
        return indented, indent, offset, blank_finish

    def get_known_indented(self, indent, until_blank=0, strip_indent=1):
        """
        Return an indented block and info.

        Extract an indented block where the indent is known for all lines.
        Starting with the current line, extract the entire text block with at
        least `indent` indentation (which must be whitespace, except for the
        first line).

        :Parameters:
            - `indent`: The number of indent columns/characters.
            - `until_blank`: Stop collecting at the first blank line if true
              (1).
            - `strip_indent`: Strip `indent` characters of indentation if true
              (1, default).

        :Return:
            - the indented block,
            - its first line offset from BOF, and
            - whether or not it finished with a blank line.
        """
        offset = self.abs_line_offset()
        indented = [self.line[indent:]]
        for line in self.input_lines[self.line_offset + 1:]:
            if line[:indent].strip():
                blank_finish = not indented[-1].strip() and len(indented) > 1
                break
            if until_blank and line.strip():
                blank_finish = 1
                break
            if strip_indent:
                indented.append(line[indent:])
            else:
                indented.append(line)
        else:
            blank_finish = 1
        if indented:
            self.next_line(len(indented) - 1) # advance to last indented line
        while indented and not indented[0].strip():
            indented.pop(0)
            offset += 1
        return indented, offset, blank_finish

    def get_first_known_indented(self, indent, until_blank=0, strip_indent=1,
                                 strip_top=1):
        """
        Return an indented block and info.

        Extract an indented block where the indent is known for the first line
        and unknown for all other lines.

        :Parameters:
            - `indent`: The first line's indent (# of columns/characters).
            - `until_blank`: Stop collecting at the first blank line if true
              (1).
            - `strip_indent`: Strip `indent` characters of indentation if true
              (1, default).
            - `strip_top`: Strip blank lines from the beginning of the block.

        :Return:
            - the indented block,
            - its indent,
            - its first line offset from BOF, and
            - whether or not it finished with a blank line.
        """
        offset = self.abs_line_offset()
        indented = [self.line[indent:]]
        indented[1:], indent, blank_finish = extract_indented(
              self.input_lines[self.line_offset + 1:], until_blank,
              strip_indent)
        self.next_line(len(indented) - 1) # advance to last indented line
        if strip_top:
            while indented and not indented[0].strip():
                indented.pop(0)
                offset += 1
        return indented, indent, offset, blank_finish


class StateWS(State):

    """
    State superclass specialized for whitespace (blank lines & indents).

    Use this class with `StateMachineWS`.  The transitions 'blank' (for blank
    lines) and 'indent' (for indented text blocks) are added automatically,
    before any other transitions.  The transition method `blank()` handles
    blank lines and `indent()` handles nested indented blocks.  Indented
    blocks trigger a new state machine to be created by `indent()` and run.
    The class of the state machine to be created is in `indent_sm`, and the
    constructor keyword arguments are in the dictionary `indent_sm_kwargs`.

    The methods `known_indent()` and `firstknown_indent()` are provided for
    indented blocks where the indent (all lines' and first line's only,
    respectively) is known to the transition method, along with the attributes
    `known_indent_sm` and `known_indent_sm_kwargs`.  Neither transition method
    is triggered automatically.
    """

    indent_sm = None
    """
    The `StateMachine` class handling indented text blocks.

    If left as ``None``, `indent_sm` defaults to the value of
    `State.nested_sm`.  Override it in subclasses to avoid the default.
    """

    indent_sm_kwargs = None
    """
    Keyword arguments dictionary, passed to the `indent_sm` constructor.

    If left as ``None``, `indent_sm_kwargs` defaults to the value of
    `State.nested_sm_kwargs`. Override it in subclasses to avoid the default.
    """

    known_indent_sm = None
    """
    The `StateMachine` class handling known-indented text blocks.

    If left as ``None``, `known_indent_sm` defaults to the value of
    `indent_sm`.  Override it in subclasses to avoid the default.
    """

    known_indent_sm_kwargs = None
    """
    Keyword arguments dictionary, passed to the `known_indent_sm` constructor.

    If left as ``None``, `known_indent_sm_kwargs` defaults to the value of
    `indent_sm_kwargs`. Override it in subclasses to avoid the default.
    """

    ws_patterns = {'blank': ' *$',
                   'indent': ' +'}
    """Patterns for default whitespace transitions.  May be overridden in
    subclasses."""

    ws_initial_transitions = ('blank', 'indent')
    """Default initial whitespace transitions, added before those listed in
    `State.initial_transitions`.  May be overridden in subclasses."""

    def __init__(self, state_machine, debug=0):
        """
        Initialize a `StateSM` object; extends `State.__init__()`.

        Check for indent state machine attributes, set defaults if not set.
        """
        State.__init__(self, state_machine, debug)
        if self.indent_sm is None:
            self.indent_sm = self.nested_sm
        if self.indent_sm_kwargs is None:
            self.indent_sm_kwargs = self.nested_sm_kwargs
        if self.known_indent_sm is None:
            self.known_indent_sm = self.indent_sm
        if self.known_indent_sm_kwargs is None:
            self.known_indent_sm_kwargs = self.indent_sm_kwargs

    def add_initial_transitions(self):
        """
        Add whitespace-specific transitions before those defined in subclass.

        Extends `State.add_initial_transitions()`.
        """
        State.add_initial_transitions(self)
        if self.patterns is None:
            self.patterns = {}
        self.patterns.update(self.ws_patterns)
        names, transitions = self.make_transitions(
            self.ws_initial_transitions)
        self.add_transitions(names, transitions)

    def blank(self, match, context, next_state):
        """Handle blank lines. Does nothing. Override in subclasses."""
        return self.nop(match, context, next_state)

    def indent(self, match, context, next_state):
        """
        Handle an indented text block. Extend or override in subclasses.

        Recursively run the registered state machine for indented blocks
        (`self.indent_sm`).
        """
        indented, indent, line_offset, blank_finish = \
              self.state_machine.get_indented()
        sm = self.indent_sm(debug=self.debug, **self.indent_sm_kwargs)
        results = sm.run(indented, input_offset=line_offset)
        return context, next_state, results

    def known_indent(self, match, context, next_state):
        """
        Handle a known-indent text block. Extend or override in subclasses.

        Recursively run the registered state machine for known-indent indented
        blocks (`self.known_indent_sm`). The indent is the length of the
        match, ``match.end()``.
        """
        indented, line_offset, blank_finish = \
              self.state_machine.get_known_indented(match.end())
        sm = self.known_indent_sm(debug=self.debug,
                                 **self.known_indent_sm_kwargs)
        results = sm.run(indented, input_offset=line_offset)
        return context, next_state, results

    def first_known_indent(self, match, context, next_state):
        """
        Handle an indented text block (first line's indent known).

        Extend or override in subclasses.

        Recursively run the registered state machine for known-indent indented
        blocks (`self.known_indent_sm`). The indent is the length of the
        match, ``match.end()``.
        """
        indented, line_offset, blank_finish = \
              self.state_machine.get_first_known_indented(match.end())
        sm = self.known_indent_sm(debug=self.debug,
                                 **self.known_indent_sm_kwargs)
        results = sm.run(indented, input_offset=line_offset)
        return context, next_state, results


class _SearchOverride:

    """
    Mix-in class to override `StateMachine` regular expression behavior.

    Changes regular expression matching, from the default `re.match()`
    (succeeds only if the pattern matches at the start of `self.line`) to
    `re.search()` (succeeds if the pattern matches anywhere in `self.line`).
    When subclassing a `StateMachine`, list this class **first** in the
    inheritance list of the class definition.
    """

    def match(self, pattern):
        """
        Return the result of a regular expression search.

        Overrides `StateMachine.match()`.

        Parameter `pattern`: `re` compiled regular expression.
        """
        return pattern.search(self.line)


class SearchStateMachine(_SearchOverride, StateMachine):
    """`StateMachine` which uses `re.search()` instead of `re.match()`."""
    pass


class SearchStateMachineWS(_SearchOverride, StateMachineWS):
    """`StateMachineWS` which uses `re.search()` instead of `re.match()`."""
    pass


class StateMachineError(Exception): pass
class UnknownStateError(StateMachineError): pass
class DuplicateStateError(StateMachineError): pass
class UnknownTransitionError(StateMachineError): pass
class DuplicateTransitionError(StateMachineError): pass
class TransitionPatternNotFound(StateMachineError): pass
class TransitionMethodNotFound(StateMachineError): pass
class UnexpectedIndentationError(StateMachineError): pass


class TransitionCorrection(Exception):

    """
    Raise from within a transition method to switch to another transition.

    Raise with one argument, the new transition name.
    """


class StateCorrection(Exception):

    """
    Raise from within a transition method to switch to another state.

    Raise with one or two arguments: new state name, and an optional new
    transition name.
    """


def string2lines(astring, tab_width=8, convert_whitespace=0,
                 whitespace=re.compile('[\v\f]')):
    """
    Return a list of one-line strings with tabs expanded and no newlines.

    Each tab is expanded with between 1 and `tab_width` spaces, so that the
    next character's index becomes a multiple of `tab_width` (8 by default).

    Parameters:

    - `astring`: a multi-line string.
    - `tab_width`: the number of columns between tab stops.
    - `convert_whitespace`: convert form feeds and vertical tabs to spaces?
    """
    if convert_whitespace:
        astring = whitespace.sub(' ', astring)
    return [s.expandtabs(tab_width) for s in astring.splitlines()]

def extract_indented(lines, until_blank=0, strip_indent=1):
    """
    Extract and return a list of indented lines of text.

    Collect all lines with indentation, determine the minimum indentation,
    remove the minimum indentation from all indented lines (unless
    `strip_indent` is false), and return them. All lines up to but not
    including the first unindented line will be returned.

    :Parameters:
        - `lines`: a list of one-line strings without newlines.
        - `until_blank`: Stop collecting at the first blank line if true (1).
        - `strip_indent`: Strip common leading indent if true (1, default).

    :Return:
        - a list of indented lines with mininum indent removed;
        - the amount of the indent;
        - whether or not the block finished with a blank line or at the end of
          `lines`.
    """
    source = []
    indent = None
    for line in lines:
        if line and line[0] != ' ':     # line not indented
            # block finished properly iff the last indented line was blank
            blank_finish = len(source) and not source[-1].strip()
            break
        stripped = line.lstrip()
        if until_blank and not stripped: # blank line
            blank_finish = 1
            break
        source.append(line)
        if not stripped:                # blank line
            continue
        lineindent = len(line) - len(stripped)
        if indent is None:
            indent = lineindent
        else:
            indent = min(indent, lineindent)
    else:
        blank_finish = 1                 # block ends at end of lines
    if indent:
        if strip_indent:
            source = [s[indent:] for s in source]
        return source, indent, blank_finish
    else:
        return [], 0, blank_finish

def _exception_data():
    """
    Return exception information:

    - the exception's class name;
    - the exception object;
    - the name of the file containing the offending code;
    - the line number of the offending code;
    - the function name of the offending code.
    """
    type, value, traceback = sys.exc_info()
    while traceback.tb_next:
        traceback = traceback.tb_next
    code = traceback.tb_frame.f_code
    return (type.__name__, value, code.co_filename, traceback.tb_lineno,
            code.co_name)
