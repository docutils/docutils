#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Test module for statemachine.py.
"""

import unittest, sys, re
from DocutilsTestSupport import statemachine
try:
    import mypdb as pdb
except:
    import pdb
pdb.tracenow = 0

debug = 0
testtext = statemachine.string2lines("""\
First paragraph.

- This is a bullet list. First list item.
  Second line of first para.

  Second para.

      block quote

- Second list item. Example::

        a
      literal
           block

Last paragraph.""")
expected = ('StateMachine1 text1 blank1 bullet1 knownindent1 '
            'StateMachine2 text2 text2 blank2 text2 blank2 indent2 '
            'StateMachine3 text3 blank3 finished3 finished2 '
            'bullet1 knownindent1 '
            'StateMachine2 text2 blank2 literalblock2(4) finished2 '
            'text1 finished1').split()
para1 = testtext[:2]
item1 = [line[2:] for line in testtext[2:9]]
item2 = [line[2:] for line in testtext[9:-1]]
lbindent = 6
literalblock = [line[lbindent:] for line in testtext[11:-1]]
para2 = testtext[-1]


class MockState(statemachine.StateWS):

    patterns = {'bullet': re.compile(r'- '),
                'text': ''}
    initialtransitions = ['bullet', ['text']]
    levelholder = [0]

    def bof(self, context):
        self.levelholder[0] += 1
        self.level = self.levelholder[0]
        if self.debug: print >>sys.stderr, 'StateMachine%s' % self.level
        return [], ['StateMachine%s' % self.level]

    def blank(self, match, context, nextstate):
        result = ['blank%s' % self.level]
        if self.debug: print >>sys.stderr, 'blank%s' % self.level
        if context and context[-1] and context[-1][-2:] == '::':
            result.extend(self.literalblock())
        return [], None, result

    def indent(self, match, context, nextstate):
        if self.debug: print >>sys.stderr, 'indent%s' % self.level
        context, nextstate, result = statemachine.StateWS.indent(
              self, match, context, nextstate)
        return context, nextstate, ['indent%s' % self.level] + result

    def knownindent(self, match, context, nextstate):
        if self.debug: print >>sys.stderr, 'knownindent%s' % self.level
        context, nextstate, result = statemachine.StateWS.knownindent(
              self, match, context, nextstate)
        return context, nextstate, ['knownindent%s' % self.level] + result

    def bullet(self, match, context, nextstate):
        if self.debug: print >>sys.stderr, 'bullet%s' % self.level
        context, nextstate, result \
              = self.knownindent(match, context, nextstate)
        return [], nextstate, ['bullet%s' % self.level] + result

    def text(self, match, context, nextstate):
        if self.debug: print >>sys.stderr, 'text%s' % self.level
        return [match.string], nextstate, ['text%s' % self.level]

    def literalblock(self):
        indented, indent, offset, good = self.statemachine.getindented()
        if self.debug: print >>sys.stderr, 'literalblock%s(%s)' % (self.level,
                                                                   indent)
        return ['literalblock%s(%s)' % (self.level, indent)]

    def eof(self, context):
        self.levelholder[0] -= 1
        if self.debug: print >>sys.stderr, 'finished%s' % self.level
        return ['finished%s' % self.level]


class EmptySMTests(unittest.TestCase):

    def setUp(self):
        self.sm = statemachine.StateMachine(
              stateclasses=[], initialstate='State')
        self.sm.debug = debug

    def test_addstate(self):
        self.sm.addstate(statemachine.State)
        self.assert_(len(self.sm.states) == 1)
        self.assertRaises(statemachine.DuplicateStateError, self.sm.addstate,
                          statemachine.State)
        self.sm.addstate(statemachine.StateWS)
        self.assert_(len(self.sm.states) == 2)

    def test_addstates(self):
        self.sm.addstates((statemachine.State, statemachine.StateWS))
        self.assertEqual(len(self.sm.states), 2)

    def test_getstate(self):
        self.assertRaises(statemachine.UnknownStateError, self.sm.getstate)
        self.sm.addstates((statemachine.State, statemachine.StateWS))
        self.assertRaises(statemachine.UnknownStateError, self.sm.getstate,
                          'unknownState')
        self.assert_(isinstance(self.sm.getstate('State'),
                                statemachine.State))
        self.assert_(isinstance(self.sm.getstate('StateWS'),
                                statemachine.State))
        self.assertEqual(self.sm.currentstate, 'StateWS')


class EmptySMWSTests(EmptySMTests):

    def setUp(self):
        self.sm = statemachine.StateMachineWS(
              stateclasses=[], initialstate='State')
        self.sm.debug = debug


class SMWSTests(unittest.TestCase):

    def setUp(self):
        self.sm = statemachine.StateMachineWS([MockState], 'MockState',
                                              debug=debug)
        self.sm.debug = debug
        self.sm.states['MockState'].levelholder[0] = 0

    def tearDown(self):
        self.sm.unlink()

    def test___init__(self):
        self.assertEquals(self.sm.states.keys(), ['MockState'])
        self.assertEquals(len(self.sm.states['MockState'].transitions), 2)

    def test_getindented(self):
        self.sm.inputlines = testtext
        self.sm.lineoffset = -1
        self.sm.nextline(3)
        indented, offset, good = self.sm.getknownindented(2)
        self.assertEquals(indented, item1)
        self.assertEquals(offset, len(para1))
        self.failUnless(good)
        self.sm.nextline()
        indented, offset, good = self.sm.getknownindented(2)
        self.assertEquals(indented, item2)
        self.assertEquals(offset, len(para1) + len(item1))
        self.failUnless(good)
        self.sm.previousline(3)
        if self.sm.debug:
            print '\ntest_getindented: self.sm.line:\n', self.sm.line
        indented, indent, offset, good = self.sm.getindented()
        if self.sm.debug:
            print '\ntest_getindented: indented:\n', indented
        self.assertEquals(indent, lbindent)
        self.assertEquals(indented, literalblock)
        self.assertEquals(offset, (len(para1) + len(item1) + len(item2)
                                   - len(literalblock)))
        self.failUnless(good)

    def test_gettextblock(self):
        self.sm.inputlines = testtext
        self.sm.lineoffset = -1
        self.sm.nextline()
        textblock = self.sm.gettextblock()
        self.assertEquals(textblock, testtext[:1])
        self.sm.nextline(2)
        textblock = self.sm.gettextblock()
        self.assertEquals(textblock, testtext[2:4])

    def test_getunindented(self):
        self.sm.inputlines = testtext
        self.sm.lineoffset = -1
        self.sm.nextline()
        textblock = self.sm.getunindented()
        self.assertEquals(textblock, testtext[:1])
        self.sm.nextline()
        self.assertRaises(statemachine.UnexpectedIndentationError,
                          self.sm.getunindented)

    def test_run(self):
        self.assertEquals(self.sm.run(testtext), expected)


class EmptyClass:
    pass


class EmptyStateTests(unittest.TestCase):

    def setUp(self):
        self.state = statemachine.State(EmptyClass(), debug=debug)
        self.state.patterns = {'nop': 'dummy',
                               'nop2': 'dummy',
                               'nop3': 'dummy',
                               'bogus': 'dummy'}
        self.state.nop2 = self.state.nop3 = self.state.nop

    def test_addtransitions(self):
        self.assertEquals(len(self.state.transitions), 0)
        self.state.addtransitions(['None'], {'None': None})
        self.assertEquals(len(self.state.transitions), 1)
        self.assertRaises(statemachine.UnknownTransitionError,
                          self.state.addtransitions, ['bogus'], {})
        self.assertRaises(statemachine.DuplicateTransitionError,
                          self.state.addtransitions, ['None'], {'None': None})

    def test_addtransition(self):
        self.assertEquals(len(self.state.transitions), 0)
        self.state.addtransition('None', None)
        self.assertEquals(len(self.state.transitions), 1)
        self.assertRaises(statemachine.DuplicateTransitionError,
                          self.state.addtransition, 'None', None)

    def test_removetransition(self):
        self.assertEquals(len(self.state.transitions), 0)
        self.state.addtransition('None', None)
        self.assertEquals(len(self.state.transitions), 1)
        self.state.removetransition('None')
        self.assertEquals(len(self.state.transitions), 0)
        self.assertRaises(statemachine.UnknownTransitionError,
                          self.state.removetransition, 'None')

    def test_maketransition(self):
        dummy = re.compile('dummy')
        self.assertEquals(self.state.maketransition('nop', 'bogus'),
                          (dummy, self.state.nop, 'bogus'))
        self.assertEquals(self.state.maketransition('nop'),
                          (dummy, self.state.nop,
                           self.state.__class__.__name__))
        self.assertRaises(statemachine.TransitionPatternNotFound,
                          self.state.maketransition, 'None')
        self.assertRaises(statemachine.TransitionMethodNotFound,
                          self.state.maketransition, 'bogus')

    def test_maketransitions(self):
        dummy = re.compile('dummy')
        self.assertEquals(self.state.maketransitions(('nop', ['nop2'],
                                                      ('nop3', 'bogus'))),
                          (['nop', 'nop2', 'nop3'],
                           {'nop': (dummy, self.state.nop,
                                    self.state.__class__.__name__),
                            'nop2': (dummy, self.state.nop2,
                                     self.state.__class__.__name__),
                            'nop3': (dummy, self.state.nop3, 'bogus')}))


class MiscTests(unittest.TestCase):

    s2l_string = "hello\tthere\thow are\tyou?\n\tI'm fine\tthanks.\n"
    s2l_expected = ['hello   there   how are you?',
                    "        I'm fine        thanks."]
    indented_string = """\
        a
      literal
           block"""

    def test_string2lines(self):
        self.assertEquals(statemachine.string2lines(self.s2l_string),
                          self.s2l_expected)

    def test_extractindented(self):
        block = statemachine.string2lines(self.indented_string)
        self.assertEquals(statemachine.extractindented(block),
                          ([s[6:] for s in block], 6, 1))
        self.assertEquals(statemachine.extractindented(self.s2l_expected),
                          ([], 0, 0))


if __name__ == '__main__':
    unittest.main()
