#!/usr/bin/env python

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This file has been placed in the public domain.

# Call: create_unimap.py < unicode.xml > unicode_latex.py
#
# Get unicode.xml from
# <http://www.w3.org/2003/entities/xml/unicode.xml>.

from xml.dom import minidom
import sys, re, textwrap

def w(s):
    if isinstance(s, unicode):
        s = s.encode('utf8')
    sys.stdout.write(s)

text_map = {}
math_map = {}

class Visitor:
    
    """Node visitor for contents of unicode.xml."""

    def visit_character(self, node):
        for n in node.childNodes:
            if n.nodeName == 'latex':
                code = node.attributes['dec'].value
                if '-' in code:
                    # I don't know what this means, but we probably
                    # don't need it....
                    continue
                if int(code) < 128:
                    # Wrong (maps "-" to "$-$", which is too wide) and
                    # unnecessary (maps "a" to "{a}").
                    continue
                latex_code = n.childNodes[0].nodeValue.encode('ascii').strip()
                if node.attributes['mode'].value == 'math':
                    math_map[unichr(int(code))] = '$%s$' % latex_code
                else:
                    text_map[unichr(int(code))] = '{%s}' % latex_code

    def visit__comment(self, node):
        if node.nodeValue.find('icense') != -1:
            print '# Revision: $%s$' % 'Revision'
            print '# Date: $%s$' % 'Date'
            print '#'
            print '# This is a mapping of Unicode characters to LaTeX'
            print '# equivalents.  The information has been extracted from'
            print '# <http://www.w3.org/2003/entities/xml/unicode.xml>.'
            print '# The extraction has been done by the "create_unimap.py"'
            print '# script written by Felix Wiemann.'
            print '#'
            print '# This file may be used and distributed under the terms'
            print '# set forth in the original copyright notice of'
            print '# unicode.xml.'
            print '#'
            print '# Original copyright notice of unicode.xml follows:'
            print '#'
            print '#'
            for line in node.nodeValue.strip().splitlines():
                print '# ' + line
            print

def call_visitor(node, visitor=Visitor()):
    if isinstance(node, minidom.Text):
        name = 'Text'
    else:
        name = node.nodeName.replace('#', '_')
    if hasattr(visitor, 'visit_' + name):
        getattr(visitor, 'visit_' + name)(node)
    for child in node.childNodes:
        call_visitor(child)
    if hasattr(visitor, 'depart_' + name):
        getattr(visitor, 'depart_' + name)(node)

document = minidom.parse(sys.stdin)
call_visitor(document)

unicode_map = math_map
unicode_map.update(text_map)
# Now unicode_map contains the text entries plus dollar-enclosed math
# entries for those chars for which no text entry exists.
print 'unicode_map = %r' % unicode_map
