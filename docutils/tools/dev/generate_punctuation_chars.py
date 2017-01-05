#!/usr/bin/env python
# -*- coding: utf-8 -*-
# :Copyright: © 2011, 2016 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: http://www.spdx.org/licenses/BSD-2-Clause

# :Id: $Id$
#
# ::

import sys, re
import unicodedata

# import the punctuation_chars module from the source or Py3k build
# path for local Python modules
if sys.version_info < (3,):
    sys.path.insert(0, '../../docutils')
else:
    sys.path.insert(0, '../../build/lib')
    unichr = chr

from docutils.utils.punctuation_chars import (openers, closers, delimiters,
                                              closing_delimiters)

# (re)generate the utils.punctuation_chars module
# ===============================================
#
# The category of some characters may change with the development of the
# Unicode standard. This tool checks the patterns in `utils.punctuation_chars`
# against a re-calculation based on the "unicodedata" stdlib module
# which may give different results for different Python versions.
#
# Updating the patterns with a new (Python|Unicode standard) version is an API
# change (may render valid rST documents invalid). It should only be done for
# "feature releases" and requires also updating the specification of `inline
# markup recognition rules`_ in ../../docs/ref/rst/restructuredtext.txt.
#
# Generation of the  character category patterns
# ----------------------------------------------
#
#
# Unicode punctuation character categories
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# For details about Unicode categories, see
# http://www.unicode.org/Public/5.1.0/ucd/UCD.html#General_Category_Values
# ::

unicode_punctuation_categories = {
    # 'Pc': 'Connector', # not used in Docutils inline markup recognition
    'Pd': 'Dash',
    'Ps': 'Open',
    'Pe': 'Close',
    'Pi': 'Initial quote', # may behave like Ps or Pe depending on usage
    'Pf': 'Final quote', # may behave like Ps or Pe depending on usage
    'Po': 'Other'
    }
"""Unicode character categories for punctuation"""


# generate character pattern strings
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ::

def unicode_charlists(categories, cp_min=0, cp_max=None):
    """Return dictionary of Unicode character lists.

    For each of the `catagories`, an item contains a list with all Unicode
    characters with `cp_min` <= code-point <= `cp_max` that belong to
    the category.

    The default values check every code-point supported by Python
    (`sys.maxint` is 0x10FFFF in a "wide" build and 0xFFFF in a "narrow"
    build, i.e. ucs4 and ucs2 respectively).
    """
    # Determine highest code point with one of the given categories
    # (may shorten the search time considerably if there are many
    # categories with not too high characters):
    if cp_max is None:
        cp_max = max(x for x in range(sys.maxunicode+1)
                    if unicodedata.category(unichr(x)) in categories)
        # print(cp_max) # => 74867 for unicode_punctuation_categories
    charlists = {}
    for cat in categories:
        charlists[cat] = [unichr(x) for x in range(cp_min, cp_max+1)
                            if unicodedata.category(unichr(x)) == cat]
    return charlists


# Character categories in Docutils
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ::

def character_category_patterns():

    """Docutils character category patterns.

    Return list of pattern strings for the categories "Open", "Close",
    "Delimiters" and "Closing-Delimiters" used in the `inline markup
    recognition rules`_.
    """

    cp_min = 160 # ASCII chars have special rules for backwards compatibility
    ucharlists = unicode_charlists(unicode_punctuation_categories, cp_min)
    """Strings of characters in Unicode punctuation character categories"""

    # match opening/closing characters
    # --------------------------------
    # Rearange the lists to ensure matching characters at the same
    # index position.

    # low quotation marks are also used as closers (e.g. in Greek)
    # move them to category Pi:
    ucharlists['Ps'].remove(u'‚') # 201A  SINGLE LOW-9 QUOTATION MARK
    ucharlists['Ps'].remove(u'„') # 201E  DOUBLE LOW-9 QUOTATION MARK
    ucharlists['Pi'] += [u'‚', u'„']

    ucharlists['Pi'].remove(u'‛') # 201B  SINGLE HIGH-REVERSED-9 QUOTATION MARK
    ucharlists['Pi'].remove(u'‟') # 201F  DOUBLE HIGH-REVERSED-9 QUOTATION MARK
    ucharlists['Pf'] += [u'‛', u'‟']

    # 301F  LOW DOUBLE PRIME QUOTATION MARK misses the opening pendant:
    ucharlists['Ps'].insert(ucharlists['Pe'].index(u'\u301f'), u'\u301d')

    # print(u''.join(ucharlists['Ps']).encode('utf8')
    # print(u''.join(ucharlists['Pe']).encode('utf8')
    # print(u''.join(ucharlists['Pi']).encode('utf8')
    # print(u''.join(ucharlists['Pf']).encode('utf8')

    # The Docutils character categories
    # ---------------------------------
    #
    # The categorization of ASCII chars is non-standard to reduce
    # both false positives and need for escaping. (see `inline markup
    # recognition rules`_)

    # allowed before markup if there is a matching closer
    openers = [u'"\'(<\\[{']
    for category in ('Ps', 'Pi', 'Pf'):
        openers.extend(ucharlists[category])

    # allowed after markup if there is a matching opener
    closers = [u'"\')>\\]}']
    for category in ('Pe', 'Pf', 'Pi'):
        closers.extend(ucharlists[category])

    # non-matching, allowed on both sides
    delimiters = [u'\\-/:']
    for category in ('Pd', 'Po'):
        delimiters.extend(ucharlists[category])

    # non-matching, after markup
    closing_delimiters = [r'\\.,;!?']

    # # Test open/close matching:
    # for i in range(min(len(openers),len(closers))):
    #     print('%4d    %s    %s' % (i, openers[i].encode('utf8'),
    #                                closers[i].encode('utf8'))

    return [u''.join(chars) for chars in (openers, closers, delimiters,
                                            closing_delimiters)]

def separate_wide_chars(s):
    """Return (s1,s2) with characters above 0xFFFF in s2"""
    maxunicode_narrow = 0xFFFF
    l1 = [ch for ch in s if ord(ch) <= maxunicode_narrow]
    l2 = [ch for ch in s if ord(ch) > maxunicode_narrow]
    return ''.join(l1), ''.join(l2)

def mark_intervals(s):
    """Return s with shortcut notation for runs of consecutive characters

    Sort string and replace 'cdef' by 'c-f' and similar.
    """
    l =[]
    s = [ord(ch) for ch in s]
    s.sort()
    for n in s:
        try:
            if l[-1][-1]+1 == n:
                l[-1].append(n)
            else:
                l.append([n])
        except IndexError:
            l.append([n])

    l2 = []
    for i in l:
        i = [unichr(n) for n in i]
        if len(i) > 2:
            i = i[0], u'-', i[-1]
        l2.extend(i)

    return ''.join(l2)

def wrap_string(s, startstring= "(u'",
                    endstring = "')", wrap=65):
    """Line-wrap a unicode string literal definition."""
    c = len(startstring)
    contstring = "'\n" + ' ' * (len(startstring)-2) + "u'"
    l = [startstring]
    for ch in s.replace("'", r"\'"):
        c += 1
        if ch == '\\' and c > wrap:
            c = len(startstring)
            ch = contstring + ch
        l.append(ch)
    l.append(endstring)
    return ''.join(l)


def print_differences(old, new, name):
    """List characters missing in old/new."""
    if old != new:
        print('new %s:' % name)
        for c in new:
            if c not in old:
                print('  %04x'%ord(c), unicodedata.name(c))
        print('removed %s:' % name)
        for c in old:
            if c not in new:
                print('  %04x'%ord(c), unicodedata.name(c))


# Output
# ------
#
# ::

if __name__ == '__main__':

# (Re)create and compare character patterns
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ::

    (o, c, d, cd) = character_category_patterns()
    o, o_wide = separate_wide_chars(o)
    c, c_wide = separate_wide_chars(c)
    d, d_wide = separate_wide_chars(d)
    d = d[:5] + mark_intervals(d[5:])
    d_wide = mark_intervals(d_wide)

    print_differences(openers, o, 'openers')
    if o_wide:
        print('+ openers-wide = ur"""%s"""' % o_wide.encode('utf8'))
    print_differences(closers, c, 'closers')
    if c_wide:
        print('+ closers-wide = ur"""%s"""' % c_wide.encode('utf8'))

    print_differences(delimiters, d + d_wide, 'delimiters')
    print_differences(closing_delimiters, cd, 'closing_delimiters')

# Print literal code to define the character sets
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This code can be copied to punctuation_chars.py if an update is wanted.

# Unicode version::

    print('# based on Unicode version %s' % unicodedata.unidata_version)

# `openers` and `closers` must be verbose and keep order because they are
# also used in `match_chars()`::

    print(wrap_string(o.encode('unicode-escape').decode(),
                      startstring="openers = (u'"))
    print(wrap_string(c.encode('unicode-escape').decode(),
                      startstring="closers = (u'"))

# delimiters: sort and use shortcut for intervals (saves ~150 characters)::

    print(wrap_string(d.encode('unicode-escape').decode(),
                      startstring="delimiters = (u'"))

# add characters in the upper plane only in a "wide" build::

    print('if sys.maxunicode >= 0x10FFFF: # "wide" build')
    print(wrap_string(d_wide.encode('unicode-escape').decode(),
                      startstring="    delimiters += (u'"))

# additional closing delimiters::

    print(wrap_string(cd.encode('unicode-escape').decode(),
                      startstring="closing_delimiters = (u'"))


# test prints
# ~~~~~~~~~~~
#
# For interactive use in development you may uncomment the following
# definitions::

    # print "wide" Unicode characters:
    # ucharlists = unicode_charlists(unicode_punctuation_categories)
    # for key in ucharlists:
    #     if key.endswith('wide'):
    #         print key, ucharlists[key]

    # print 'openers = ', repr(openers)
    # print 'closers = ', repr(closers)
    # print 'delimiters = ', repr(delimiters)
    # print 'closing_delimiters = ', repr(closing_delimiters)

    # ucharlists = unicode_charlists(unicode_punctuation_categories)
    # for cat, chars in ucharlists.items():
    #     # print cat, chars
    #     # compact output (visible with a comprehensive font):
    #     print (u":%s: %s" % (cat, u''.join(chars))).encode('utf8')

# verbose print
#
# ::

    # print 'openers:'
    # for ch in openers:
    #     print ch.encode('utf8'), unicodedata.name(ch)
    # print 'closers:'
    # for ch in closers:
    #     print ch.encode('utf8'), unicodedata.name(ch)
    # print 'delimiters:'
    # for ch in delimiters:
    #     print ch.encode('utf8'), unicodedata.name(ch)
    # print 'closing_delimiters:'
    # for ch in closing_delimiters:
    #     print ch.encode('utf8'), unicodedata.name(ch)
