#! /usr/bin/env python

"""
Analysis of the re.UNICODE flag on whitespace recognition.
"""

# Running this program produces this output:
"""
Regular expressions:

1. '\\s'
2. '\\s', re.UNICODE
3. u'(?![\xa0\u202f])\\s', re.UNICODE

===  =========  =======  =========================  =======
Cat  Codepoint  Decimal  Name/Description           Regexps
===  =========  =======  =========================  =======
Cc    U+0009         9   (HT) TAB \t                1 2 3
Cc    U+000a        10   (LF) LINE FEED \n          1 2 3
Cc    U+000b        11   (VT) VERTICAL TAB \v       1 2 3
Cc    U+000c        12   (FF) FORM FEED \f          1 2 3
Cc    U+000d        13   (CR) CARRIAGE RETURN \r    1 2 3
Cc    U+001c        28   (FS) FILE SEPARATOR          2 3
Cc    U+001d        29   (GS) GROUP SEPARATOR         2 3
Cc    U+001e        30   (RS) RECORD SEPARATOR        2 3
Cc    U+001f        31   (US) UNIT SEPARATOR          2 3
Zs    U+0020        32   SPACE                      1 2 3
Cc    U+0085       133   (NEL) NEXT LINE              2 3
Zs    U+00a0       160   NO-BREAK SPACE               2  
Zs    U+1680      5760   OGHAM SPACE MARK             2 3
Zs    U+2000      8192   EN QUAD                      2 3
Zs    U+2001      8193   EM QUAD                      2 3
Zs    U+2002      8194   EN SPACE                     2 3
Zs    U+2003      8195   EM SPACE                     2 3
Zs    U+2004      8196   THREE-PER-EM SPACE           2 3
Zs    U+2005      8197   FOUR-PER-EM SPACE            2 3
Zs    U+2006      8198   SIX-PER-EM SPACE             2 3
Zs    U+2007      8199   FIGURE SPACE                 2 3
Zs    U+2008      8200   PUNCTUATION SPACE            2 3
Zs    U+2009      8201   THIN SPACE                   2 3
Zs    U+200a      8202   HAIR SPACE                   2 3
Zs    U+200b      8203   ZERO WIDTH SPACE             2 3
Zl    U+2028      8232   LINE SEPARATOR               2 3
Zp    U+2029      8233   PARAGRAPH SEPARATOR          2 3
Zs    U+202f      8239   NARROW NO-BREAK SPACE        2  
Zs    U+205f      8287   MEDIUM MATHEMATICAL SPACE    2 3
Zs    U+3000     12288   IDEOGRAPHIC SPACE            2 3
===  =========  =======  =========================  =======
"""

# For Unicode category information, see
# http://www.unicode.org/Public/UNIDATA/UCD.html#General_Category_Values
"""
========  ====================
Category  Description
========  ====================
Zs        Separator, Space
Zl        Separator, Line
Zp        Separator, Paragraph
Cc        Other, Control
Cf        Other, Format
========  ====================
"""

import re
import unicodedata

charnames = {9: '(HT) TAB \\t',
             10: '(LF) LINE FEED \\n',
             11: '(VT) VERTICAL TAB \\v',
             12: '(FF) FORM FEED \\f',
             13: '(CR) CARRIAGE RETURN \\r',
             28: '(FS) FILE SEPARATOR',
             29: '(GS) GROUP SEPARATOR',
             30: '(RS) RECORD SEPARATOR',
             31: '(US) UNIT SEPARATOR',
             133: '(NEL) NEXT LINE'}

pats = [re.compile(r'\s'),
        re.compile(r'\s', re.UNICODE),
        re.compile(u'(?![\u00a0\u202f])\\s', re.UNICODE),]

border = '===  =========  =======  =========================  ======='
header = 'Cat  Codepoint  Decimal  Name/Description           Regexps'

print 'Regular expressions:\n'
for i, pat in enumerate(pats):
    if pat.flags & re.UNICODE:
        flag = ', re.UNICODE'
    else:
        flag = ''
    print '%s. %r%s' % (i + 1, pat.pattern, flag)
print

print border
print header
print border

chars = []
for u in range(0x10000):
    c = unichr(u)
    category = unicodedata.category(c)
    if category[:0] in 'ZC':            # Z: whitespace; C: controls
        respace = 0
        parts = []
        for i, pat in enumerate(pats):
            if pat.search(c):
                parts.append(str(i + 1))
                respace += 1
            else:
                parts.append(' ')
        if category.startswith('Z') or respace:
            print ('%s    U+%04x     %5s   %-25s  %s'
                   % (category, u, u,
                      unicodedata.name(c, charnames.get(u, repr(c))),
                      ' '.join(parts)))
            chars.append(c)
print border
