#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Version: 1.3
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

A finite state machine specialized for regular-expression-based text filters,
this module defines the following classes:

- `StateMachine`, a state machine
- `State`, a state superclass
- `StateMachineWS`, a whitespace-sensitive version of `StateMachine`
- `StateWS`, a state superclass for use with `StateMachineWS`
- `SearchStateMachine`, uses `re.search()` instead of `re.match()`
- `SearchStateMachineWS`, uses `re.search()` instead of `re.match()`

Exception classes:

- `UnknownStateError`
- `DuplicateStateError`
- `UnknownTransitionError`
- `DuplicateTransitionError`
- `TransitionPatternNotFound`
- `TransitionMethodNotFound`
- `UnexpectedIndentationError`
- `TransitionCorrection`: Raised to switch to another transition.

Functions:

- `string2lines()`: split a multi-line string into a list of one-line strings
- `extractindented()`: return indented lines with minimum indentation removed

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
      `State.initialtransitions`::

          initialtransitions = ['atransition', ...]

   c) Define a method for each transition, with the same name as the
      transition pattern::

          def atransition(self, match, context, nextstate):
              # do something
              result = [...]  # a list
              return context, nextstate, result
              # context, nextstate may be altered

      Transition methods may raise an `EOFError` to cut processing short.

   d) You may wish to override the `State.bof()` and/or `State.eof()` implicit
      transition methods, which handle the beginning- and end-of-file.

   e) In order to handle nested processing, you may wish to override the
      attributes `State.nestedSM` and/or `State.nestedSMkwargs`.

      If you are using `StateWS` as a base class, in order to handle nested
      indented blocks, you may wish to:

      - override the attributes `StateWS.indentSM`, `StateWS.indentSMkwargs`,
        `StateWS.knownindentSM`, and/or `StateWS.knownindentSMkwargs`;
      - override the `StateWS.blank()` method; and/or
      - override or extend the `StateWS.indent()`, `StateWS.knownindent()`,
        and/or `StateWS.firstknownindent()` methods.

3. Create a state machine object::

       sm = StateMachine(stateclasses=[MyState, ...], initialstate='MyState')

4. Obtain the input text, which needs to be converted into a tab-free list of
   one-line strings. For example, to read text from a file called
   'inputfile'::

       inputstring = open('inputfile').read()
       inputlines = statemachine.string2lines(inputstring)

6. Run the state machine on the input text and collect the results, a list::

       results = sm.run(inputlines)

7. Remove any lingering circular references::

       sm.unlink()
"""

__docformat__ = 'restructuredtext'

import sys, re, string


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

    def __init__(self, stateclasses, initialstate, debug=0):
        """
        Initialize a `StateMachine` object; add state objects.

        Parameters:

        - `stateclasses`: a list of `State` (sub)classes.
        - `initialstate`: a string, the class name of the initial state.
        - `debug`: a boolean; produce verbose output if true (nonzero).
        """

        self.inputlines = None
        """List of strings (without newlines). Filled by `self.run()`."""

        self.inputoffset = 0
        """Offset of `self.inputlines` from the beginning of the file."""

        self.line = None
        """Current input line."""

        self.lineoffset = None
        """Current input line offset from beginning of `self.inputlines`."""

        self.debug = debug
        """Debugging mode on/off."""

        self.initialstate = initialstate
        """The name of the initial state (key to `self.states`)."""

        self.currentstate = initialstate
        """The name of the current state (key to `self.states`)."""

        self.states = {}
        """Mapping of {state_name: State_object}."""

        self.addstates(stateclasses)

    def unlink(self):
        """Remove circular references to objects no longer required."""
        for state in self.states.values():
            state.unlink()
        self.states = None

    def run(self, inputlines, inputoffset=0):
        """
        Run the state machine on `inputlines`. Return results (a list).

        Reset `self.lineoffset` and `self.currentstate`. Run the
        beginning-of-file transition. Input one line at a time and check for a
        matching transition. If a match is found, call the transition method
        and possibly change the state. Store the context returned by the
        transition method to be passed on to the next transition matched.
        Accumulate the results returned by the transition methods in a list.
        Run the end-of-file transition. Finally, return the accumulated
        results.

        Parameters:

        - `inputlines`: a list of strings without newlines.
        - `inputoffset`: the line offset of `inputlines` from the beginning of
          the file.
        """
        self.inputlines = inputlines
        self.inputoffset = inputoffset
        self.lineoffset = -1
        self.currentstate = self.initialstate
        if self.debug:
            print >>sys.stderr, ('\nStateMachine.run: inputlines:\n| %s' %
                                 '\n| '.join(self.inputlines))
        context = None
        results = []
        state = self.getstate()
        try:
            if self.debug:
                print >>sys.stderr, ('\nStateMachine.run: bof transition')
            context, result = state.bof(context)
            results.extend(result)
            while 1:
                try:
                    self.nextline()
                    if self.debug:
                        print >>sys.stderr, ('\nStateMachine.run: line:\n| %s'
                                             % self.line)
                except IndexError:
                    break
                try:
                    context, nextstate, result = self.checkline(context, state)
                except EOFError:
                    break
                state = self.getstate(nextstate)
                results.extend(result)
            if self.debug:
                print >>sys.stderr, ('\nStateMachine.run: eof transition')
            result = state.eof(context)
            results.extend(result)
        except:
            self.error()
            raise
        return results

    def getstate(self, nextstate=None):
        """
        Return current state object; set it first if `nextstate` given.

        Parameter `nextstate`: a string, the name of the next state.

        Exception: `UnknownStateError` raised if `nextstate` unknown.
        """
        if nextstate:
            if self.debug and nextstate != self.currentstate:
                print >>sys.stderr, \
                      ('\nStateMachine.getstate: Changing state from '
                       '"%s" to "%s" (input line %s).'
                       % (self.currentstate, nextstate, self.abslineno()))
            self.currentstate = nextstate
        try:
            return self.states[self.currentstate]
        except KeyError:
            raise UnknownStateError(self.currentstate)

    def nextline(self, n=1):
        """Load `self.line` with the `n`'th next line and return it."""
        self.lineoffset += n
        self.line = self.inputlines[self.lineoffset]
        return self.line

    def nextlineblank(self):
        """Return 1 if the next line is blank or non-existant."""
        try:
            return not self.inputlines[self.lineoffset + 1].strip()
        except IndexError:
            return 1

    def ateof(self):
        """Return 1 if the input is at or past end-of-file."""
        return self.lineoffset >= len(self.inputlines) - 1

    def atbof(self):
        """Return 1 if the input is at or before beginning-of-file."""
        return self.lineoffset <= 0

    def previousline(self, n=1):
        """Load `self.line` with the `n`'th previous line and return it."""
        self.lineoffset -= n
        self.line = self.inputlines[self.lineoffset]
        return self.line

    def gotoline(self, lineoffset):
        """Jump to absolute line offset `lineoffset`, load and return it."""
        self.lineoffset = lineoffset - self.inputoffset
        self.line = self.inputlines[self.lineoffset]
        return self.line

    def abslineoffset(self):
        """Return line offset of current line, from beginning of file."""
        return self.lineoffset + self.inputoffset

    def abslineno(self):
        """Return line number of current line (counting from 1)."""
        return self.lineoffset + self.inputoffset + 1

    def gettextblock(self):
        """Return a contiguous block of text."""
        block = []
        for line in self.inputlines[self.lineoffset:]:
            if not line.strip():
                break
            block.append(line)
        self.nextline(len(block) - 1)  # advance to last line of block
        return block

    def getunindented(self):
        """
        Return a contiguous, flush-left block of text.

        Raise `UnexpectedIndentationError` if an indented line is encountered
        before the text block ends (with a blank line).
        """
        block = [self.line]
        for line in self.inputlines[self.lineoffset + 1:]:
            if not line.strip():
                break
            if line[0] == ' ':
                self.nextline(len(block) - 1) # advance to last line of block
                raise UnexpectedIndentationError(block, self.abslineno() + 1)
            block.append(line)
        self.nextline(len(block) - 1)  # advance to last line of block
        return block

    def checkline(self, context, state):
        """
        Examine one line of input for a transition match.

        Parameters:

        - `context`: application-dependent storage.
        - `state`: a `State` object, the current state.

        Return the values returned by the transition method:

        - context: possibly modified from the parameter `context`;
        - next state name (`State` subclass name), or ``None`` if no match;
        - the result output of the transition, a list.
        """
        if self.debug:
            print >>sys.stdout, ('\nStateMachine.checkline: '
                                 'context "%s", state "%s"' %
                                 (context, state.__class__.__name__))
        context, nextstate, result = self.matchtransition(context, state)
        return context, nextstate, result

    def matchtransition(self, context, state):
        """
        Try to match the current line to a transition & execute its method.

        Parameters:

        - `context`: application-dependent storage.
        - `state`: a `State` object, the current state.

        Return the values returned by the transition method:

        - context: possibly modified from the parameter `context`, unchanged
          if no match;
        - next state name (`State` subclass name), or ``None`` if no match;
        - the result output of the transition, a list (empty if no match).
        """
        if self.debug:
            print >>sys.stderr, (
                  '\nStateMachine.matchtransition: state="%s", transitions=%r.'
                  % (state.__class__.__name__, state.transitionorder))
        for name in state.transitionorder:
            while 1:
                pattern, method, nextstate = state.transitions[name]
                if self.debug:
                    print >>sys.stderr, (
                          '\nStateMachine.matchtransition: Trying transition '
                          '"%s" in state "%s".'
                          % (name, state.__class__.__name__))
                match = self.match(pattern)
                if match:
                    if self.debug:
                        print >>sys.stderr, (
                              '\nStateMachine.matchtransition: Matched '
                              'transition "%s" in state "%s".'
                              % (name, state.__class__.__name__))
                    try:
                        return method(match, context, nextstate)
                    except TransitionCorrection, detail:
                        name = str(detail)
                        continue        # try again with new transition name
                break
        else:
            return context, None, []    # no match

    def match(self, pattern):
        """
        Return the result of a regular expression match.

        Parameter `pattern`: an `re` compiled regular expression.
        """
        return pattern.match(self.line)

    def addstate(self, stateclass):
        """
        Initialize & add a `stateclass` (`State` subclass) object.

        Exception: `DuplicateStateError` raised if `stateclass` already added.
        """
        statename = stateclass.__name__
        if self.states.has_key(statename):
            raise DuplicateStateError(statename)
        self.states[statename] = stateclass(self, self.debug)

    def addstates(self, stateclasses):
        """
        Add `stateclasses` (a list of `State` subclasses).
        """
        for stateclass in stateclasses:
            self.addstate(stateclass)

    def error(self):
        """Report error details."""
        type, value, module, line, function = _exceptiondata()
        print >>sys.stderr, '%s: %s' % (type, value)
        print >>sys.stderr, 'input line %s' % (self.abslineno())
        print >>sys.stderr, ('module %s, line %s, function %s'
                             % (module, line, function))


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
    `patterns` and `initialtransitions` class attributes, and provide
    corresponding transition methods. The default object initialization will
    take care of constructing the list of transitions.
    """

    patterns = None
    """
    {Name: pattern} mapping, used by `maketransition()`. Each pattern may
    be a string or a compiled `re` pattern. Override in subclasses.
    """

    initialtransitions = None
    """
    A list of transitions to initialize when a `State` is instantiated.
    Each entry is either a transition name string, or a (transition name, next
    state name) pair. See `maketransitions()`. Override in subclasses.
    """

    nestedSM = None
    """
    The `StateMachine` class for handling nested processing.

    If left as ``None``, `nestedSM` defaults to the class of the state's
    controlling state machine. Override it in subclasses to avoid the default.
    """

    nestedSMkwargs = None
    """
    Keyword arguments dictionary, passed to the `nestedSM` constructor.

    Two keys must have entries in the dictionary:

    - Key 'stateclasses' must be set to a list of `State` classes.
    - Key 'initialstate' must be set to the name of the initial state class.

    If `nestedSMkwargs` is left as ``None``, 'stateclasses' defaults to the
    class of the current state, and 'initialstate' defaults to the name of the
    class of the current state. Override in subclasses to avoid the defaults.
    """

    def __init__(self, statemachine, debug=0):
        """
        Initialize a `State` object; make & add initial transitions.

        Parameters:

        - `statemachine`: the controlling `StateMachine` object.
        - `debug`: a boolean; produce verbose output if true (nonzero).
        """

        self.transitionorder = []
        """A list of transition names in search order."""

        self.transitions = {}
        """
        A mapping of transition names to 3-tuples containing
        (compiled_pattern, transition_method, next_state_name). Initialized as
        an instance attribute dynamically (instead of as a class attribute)
        because it may make forward references to patterns and methods in this
        or other classes.
        """

        if self.initialtransitions:
            names, transitions = self.maketransitions(self.initialtransitions)
            self.addtransitions(names, transitions)

        self.statemachine = statemachine
        """A reference to the controlling `StateMachine` object."""

        self.debug = debug
        """Debugging mode on/off."""

        if self.nestedSM is None:
            self.nestedSM = self.statemachine.__class__
        if self.nestedSMkwargs is None:
            self.nestedSMkwargs = {'stateclasses': [self.__class__],
                                   'initialstate': self.__class__.__name__}

    def unlink(self):
        """Remove circular references to objects no longer required."""
        self.statemachine = None

    def addtransitions(self, names, transitions):
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
        self.transitionorder[:0] = names
        self.transitions.update(transitions)

    def addtransition(self, name, transition):
        """
        Add a transition to the start of the transition list.

        Parameter `transition`: a ready-made transition 3-tuple.

        Exception: `DuplicateTransitionError`.
        """
        if self.transitions.has_key(name):
            raise DuplicateTransitionError(name)
        self.transitionorder[:0] = [name]
        self.transitions[name] = transition

    def removetransition(self, name):
        """
        Remove a transition by `name`.

        Exception: `UnknownTransitionError`.
        """
        try:
            del self.transitions[name]
            self.transitionorder.remove(name)
        except:
            raise UnknownTransitionError(name)

    def maketransition(self, name, nextstate=None):
        """
        Make & return a transition tuple based on `name`.

        This is a convenience function to simplify transition creation.

        Parameters:

        - `name`: a string, the name of the transition pattern & method. This
          `State` object must have a method called '`name`', and a dictionary
          `self.patterns` containing a key '`name`'.
        - `nextstate`: a string, the name of the next `State` object for this
          transition. A value of ``None`` (or absent) implies no state change
          (i.e., continue with the same state).

        Exceptions: `TransitionPatternNotFound`, `TransitionMethodNotFound`.
        """
        if nextstate is None:
            nextstate = self.__class__.__name__
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
        return (pattern, method, nextstate)

    def maketransitions(self, namelist):
        """
        Return a list of transition names and a transition mapping.

        Parameter `namelist`: a list, where each entry is either a
        transition name string, or a 1- or 2-tuple (transition name, optional
        next state name).
        """
        stringtype = type('')
        names = []
        transitions = {}
        for namestate in namelist:
            if type(namestate) is stringtype:
                transitions[namestate] = self.maketransition(namestate)
                names.append(namestate)
            else:
                transitions[namestate[0]] = self.maketransition(*namestate)
                names.append(namestate[0])
        return names, transitions

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

    def nop(self, match, context, nextstate):
        """
        A "do nothing" transition method.

        Return unchanged `context` & `nextstate`, empty result. Useful for
        simple state changes (actionless transitions).
        """
        return context, nextstate, []


class StateMachineWS(StateMachine):

    """
    `StateMachine` subclass specialized for whitespace recognition.

    The transitions 'blank' (for blank lines) and 'indent' (for indented text
    blocks) are defined implicitly, and are checked before any other
    transitions. The companion `StateWS` class defines default transition
    methods. There are three methods provided for extracting indented text
    blocks:

    - `getindented()`: use when the indent is unknown.
    - `getknownindented()`: use when the indent is known for all lines.
    - `getfirstknownindented()`: use when only the first line's indent is
      known.
    """

    spaces = re.compile(' *')
    """Indentation recognition pattern."""

    def checkline(self, context, state):
        """
        Examine one line of input for whitespace first, then transitions.

        Extends `StateMachine.checkline()`.
        """
        if self.debug:
            print >>sys.stdout, ('\nStateMachineWS.checkline: '
                                 'context "%s", state "%s"' %
                                 (context, state.__class__.__name__))
        context, nextstate, result = self.checkwhitespace(context, state)
        if nextstate == '':             # no whitespace match
            return StateMachine.checkline(self, context, state)
        else:
            return context, nextstate, result

    def checkwhitespace(self, context, state):
        """
        Check for a blank line or increased indent. Call the state's
        transition method if a match is found.

        Parameters:

        - `context`: application-dependent storage.
        - `state`: a `State` object, the current state.

        Return the values returned by the transition method:

        - context, possibly modified from the parameter `context`;
        - next state name (`State` subclass name), or '' (empty string) if no
          match;
        - the result output of the transition, a list (empty if no match).
        """
        if self.debug:
            print >>sys.stdout, ('\nStateMachineWS.checkwhitespace: '
                                 'context "%s", state "%s"' %
                                 (context, state.__class__.__name__))
        match = self.spaces.match(self.line)
        indent = match.end()
        if indent == len(self.line):
            if self.debug:
                print >>sys.stdout, ('\nStateMachineWS.checkwhitespace: '
                                     'implicit transition "blank" matched')
            return state.blank(match, context, self.currentstate)
        elif indent:
            if self.debug:
                print >>sys.stdout, ('\nStateMachineWS.checkwhitespace: '
                                     'implicit transition "indent" matched')
            return state.indent(match, context, self.currentstate)
        else:
            return context, '', []      # neither blank line nor indented

    def getindented(self, uptoblank=0, stripindent=1):
        """
        Return a indented lines of text and info.

        Extract an indented block where the indent is unknown for all lines.

        :Parameters:
            - `uptoblank`: Stop collecting at the first blank line if true (1).
            - `stripindent`: Strip common leading indent if true (1, default).

        :Return:
            - the indented block (a list of lines of text),
            - its indent,
            - its first line offset from BOF, and
            - whether or not it finished with a blank line.
        """
        offset = self.abslineoffset()
        indented, indent, blankfinish = extractindented(
              self.inputlines[self.lineoffset:], uptoblank, stripindent)
        if indented:
            self.nextline(len(indented) - 1) # advance to last indented line
        while indented and not indented[0].strip():
            indented.pop(0)
            offset += 1
        return indented, indent, offset, blankfinish

    def getknownindented(self, indent, uptoblank=0, stripindent=1):
        """
        Return an indented block and info.

        Extract an indented block where the indent is known for all lines.
        Starting with the current line, extract the entire text block with at
        least `indent` indentation (which must be whitespace, except for the
        first line).

        :Parameters:
            - `indent`: The number of indent columns/characters.
            - `uptoblank`: Stop collecting at the first blank line if true (1).
            - `stripindent`: Strip `indent` characters of indentation if true
              (1, default).

        :Return:
            - the indented block,
            - its first line offset from BOF, and
            - whether or not it finished with a blank line.
        """
        offset = self.abslineoffset()
        indented = [self.line[indent:]]
        for line in self.inputlines[self.lineoffset + 1:]:
            if line[:indent].strip():
                blankfinish = not indented[-1].strip() and len(indented) > 1
                break
            if uptoblank and line.strip():
                blankfinish = 1
                break
            if stripindent:
                indented.append(line[indent:])
            else:
                indented.append(line)
        else:
            blankfinish = 1
        if indented:
            self.nextline(len(indented) - 1) # advance to last indented line
        while indented and not indented[0].strip():
            indented.pop(0)
            offset += 1
        return indented, offset, blankfinish

    def getfirstknownindented(self, indent, uptoblank=0, stripindent=1):
        """
        Return an indented block and info.

        Extract an indented block where the indent is known for the first line
        and unknown for all other lines.

        :Parameters:
            - `indent`: The first line's indent (# of columns/characters).
            - `uptoblank`: Stop collecting at the first blank line if true (1).
            - `stripindent`: Strip `indent` characters of indentation if true
              (1, default).

        :Return:
            - the indented block,
            - its indent,
            - its first line offset from BOF, and
            - whether or not it finished with a blank line.
        """
        offset = self.abslineoffset()
        indented = [self.line[indent:]]
        indented[1:], indent, blankfinish = extractindented(
              self.inputlines[self.lineoffset + 1:], uptoblank, stripindent)
        self.nextline(len(indented) - 1)  # advance to last indented line
        while indented and not indented[0].strip():
            indented.pop(0)
            offset += 1
        return indented, indent, offset, blankfinish


class StateWS(State):

    """
    State superclass specialized for whitespace (blank lines & indents).

    Use this class with `StateMachineWS`. The transition method `blank()`
    handles blank lines and `indent()` handles nested indented blocks.
    Indented blocks trigger a new state machine to be created by `indent()`
    and run. The class of the state machine to be created is in `indentSM`,
    and the constructor keyword arguments are in the dictionary
    `indentSMkwargs`.

    The methods `knownindent()` and `firstknownindent()` are provided for
    indented blocks where the indent (all lines' and first line's only,
    respectively) is known to the transition method, along with the attributes
    `knownindentSM` and `knownindentSMkwargs`. Neither transition method is
    triggered automatically.
    """

    indentSM = None
    """
    The `StateMachine` class handling indented text blocks.

    If left as ``None``, `indentSM` defaults to the value of `State.nestedSM`.
    Override it in subclasses to avoid the default.
    """

    indentSMkwargs = None
    """
    Keyword arguments dictionary, passed to the `indentSM` constructor.

    If left as ``None``, `indentSMkwargs` defaults to the value of
    `State.nestedSMkwargs`. Override it in subclasses to avoid the default.
    """

    knownindentSM = None
    """
    The `StateMachine` class handling known-indented text blocks.

    If left as ``None``, `knownindentSM` defaults to the value of `indentSM`.
    Override it in subclasses to avoid the default.
    """

    knownindentSMkwargs = None
    """
    Keyword arguments dictionary, passed to the `knownindentSM` constructor.

    If left as ``None``, `knownindentSMkwargs` defaults to the value of
    `indentSMkwargs`. Override it in subclasses to avoid the default.
    """

    def __init__(self, statemachine, debug=0):
        """
        Initialize a `StateSM` object; extends `State.__init__()`.

        Check for indent state machine attributes, set defaults if not set.
        """
        State.__init__(self, statemachine, debug)
        if self.indentSM is None:
            self.indentSM = self.nestedSM
        if self.indentSMkwargs is None:
            self.indentSMkwargs = self.nestedSMkwargs
        if self.knownindentSM is None:
            self.knownindentSM = self.indentSM
        if self.knownindentSMkwargs is None:
            self.knownindentSMkwargs = self.indentSMkwargs

    def blank(self, match, context, nextstate):
        """Handle blank lines. Does nothing. Override in subclasses."""
        return self.nop(match, context, nextstate)

    def indent(self, match, context, nextstate):
        """
        Handle an indented text block. Extend or override in subclasses.

        Recursively run the registered state machine for indented blocks
        (`self.indentSM`).
        """
        indented, indent, lineoffset, blankfinish = \
              self.statemachine.getindented()
        sm = self.indentSM(debug=self.debug, **self.indentSMkwargs)
        results = sm.run(indented, inputoffset=lineoffset)
        return context, nextstate, results

    def knownindent(self, match, context, nextstate):
        """
        Handle a known-indent text block. Extend or override in subclasses.

        Recursively run the registered state machine for known-indent indented
        blocks (`self.knownindentSM`). The indent is the length of the match,
        ``match.end()``.
        """
        indented, lineoffset, blankfinish = \
              self.statemachine.getknownindented(match.end())
        sm = self.knownindentSM(debug=self.debug, **self.knownindentSMkwargs)
        results = sm.run(indented, inputoffset=lineoffset)
        return context, nextstate, results

    def firstknownindent(self, match, context, nextstate):
        """
        Handle an indented text block (first line's indent known).

        Extend or override in subclasses.

        Recursively run the registered state machine for known-indent indented
        blocks (`self.knownindentSM`). The indent is the length of the match,
        ``match.end()``.
        """
        indented, lineoffset, blankfinish = \
              self.statemachine.getfirstknownindented(match.end())
        sm = self.knownindentSM(debug=self.debug, **self.knownindentSMkwargs)
        results = sm.run(indented, inputoffset=lineoffset)
        return context, nextstate, results


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


class UnknownStateError(Exception): pass
class DuplicateStateError(Exception): pass
class UnknownTransitionError(Exception): pass
class DuplicateTransitionError(Exception): pass
class TransitionPatternNotFound(Exception): pass
class TransitionMethodNotFound(Exception): pass
class UnexpectedIndentationError(Exception): pass


class TransitionCorrection(Exception):

    """
    Raise from within a transition method to switch to another transition.
    """


_whitespace_conversion_table = string.maketrans('\v\f', '  ')

def string2lines(astring, tabwidth=8, convertwhitespace=0):
    """
    Return a list of one-line strings with tabs expanded and no newlines.

    Each tab is expanded with between 1 and `tabwidth` spaces, so that the
    next character's index becomes a multiple of `tabwidth` (8 by default).

    Parameters:

    - `astring`: a multi-line string.
    - `tabwidth`: the number of columns between tab stops.
    - `convertwhitespace`: convert form feeds and vertical tabs to spaces?
    """
    if convertwhitespace:
        astring = astring.translate(_whitespace_conversion_table)
    return [s.expandtabs(tabwidth) for s in astring.splitlines()]

def extractindented(lines, uptoblank=0, stripindent=1):
    """
    Extract and return a list of indented lines of text.

    Collect all lines with indentation, determine the minimum indentation,
    remove the minimum indentation from all indented lines (unless
    `stripindent` is false), and return them. All lines up to but not
    including the first unindented line will be returned.

    :Parameters:
        - `lines`: a list of one-line strings without newlines.
        - `uptoblank`: Stop collecting at the first blank line if true (1).
        - `stripindent`: Strip common leading indent if true (1, default).

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
            blankfinish = len(source) and not source[-1].strip()
            break
        stripped = line.lstrip()
        if uptoblank and not stripped: # blank line
            blankfinish = 1
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
        blankfinish = 1                 # block ends at end of lines
    if indent:
        if stripindent:
            source = [s[indent:] for s in source]
        return source, indent, blankfinish
    else:
        return [], 0, blankfinish

def _exceptiondata():
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
