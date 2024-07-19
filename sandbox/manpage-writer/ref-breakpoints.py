# see groff_man_style on

#  Hyperlink macros
#      Man  page  cross references like ls(1) are best presented with .MR.  Text may be
#      hyperlinked to email addresses with .MT/.ME or other URIs with .UR/.UE.   Hyper‐
#      linked  text  is  supported  on  HTML and terminal output devices; terminals and
#      pager programs must support ECMA-48 OSC  8  escape  sequences  (see  grotty(1)).
#      When  device support is unavailable or disabled with the U register (see section
#      “Options” below), .MT and .UR URIs are rendered between angle brackets after the
#      linked text.
#
#      .MT, .ME, .UR, and .UE are GNU extensions not defined on systems  running  AT&T,
#      Plan 9, or Solaris troff; see an-ext.tmac in section “Files” below.  Plan 9 from
#      User Space's troff implements .MR.
#
#      The arguments to .MR, .MT, and .UR should be prepared for typesetting since they
#      can appear in the output.  Use special character escape sequences to encode Uni‐
#      code  basic  Latin  characters  where  necessary, particularly the hyphen-minus.
#      (See section “Portability” below.)  URIs can be lengthy; rendering them can  re‐
#      sult  in jarring adjustment or variations in line length, or troff warnings when
#      a hyperlink is longer than an output  line.   The  application  of  non-printing
#      break point escape sequences \: after each slash (or series thereof), and before
#      each  dot  (or  series  thereof)  is recommended as a rule of thumb.  The former
#      practice avoids forcing a trailing slash in a URI onto a separate  output  line,
#      and  the  latter  helps the reader to avoid mistakenly interpreting a dot at the
#      end of a line as a period (or multiple dots as an ellipsis).  Thus,
#             .UR http://\:example\:.com/\:fb8afcfbaebc74e\:.cc
#      has several potential break points in the  URI  shown.   Consider  adding  break
#      points  before  or after at signs in email addresses, and question marks, amper‐
#      sands, and number signs in HTTP(S) URIs.  The formatter removes  \:  escape  se‐
#      quences  from  hyperlinks  when supplying device control commands to output dri‐
#      vers.
#      

tests = (
        ("///abc.de", r"///\:abc.de"),
        ("/abc.de/", r"/\:abc.de/"),
        ("http://abc.de", r"http://\:abc.de"),
        ("http://abc.de/fg", r"http://\:abc.de/\:fg"),
        ("http://abc.de/fg?q=abc", r"http://\:abc.de/\:fg?\:q=abc"),
        ("http://abc.de/fg/?q=abc", r"http://\:abc.de/\:fg/?\:q=abc"),
        ("http://abc.de/fg/?q=abc&me#", r"http://\:abc.de/\:fg/?\:q=abc&\:me#"),
        ("me@home.here", r"me@\:home.here"),
        )

import re

BREAKPOINT = r'\:'

# after series of slash
# after?before at sign
# after question marks
# after question ampersands
# after question number signs
# (?=.) avoids matching the of string
MATCH = re.compile(r'([/@?&#]+)(?=.)')

def insert_breakpoints(s):
    return MATCH.sub( r'\1'+BREAKPOINT, s)

err = 0
cnt = 0

for t in tests:
    cnt += 1
    got = insert_breakpoints(t[0])
    if t[1] != got:
        print( "FAIL {0} got {1} expected {2}".format(t[0], got, t[1]) )
        err += 1

print(f"{err} errors of {cnt} tests")

