#!/usr/bin/env python

"""
:Author: Garth Kidd
:Contact: garth@deadlybloodyserious.com
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.
"""

import sys, os, getopt
import docutils.utils
from docutils.parsers.rst import Parser


usage_header = """\
quicktest.py: quickly test the restructuredtext parser.

Usage::

    quicktest.py [options] [filename]

``filename`` is the name of the file to use as input (default is stdin).

Options:
"""

options = [('pretty', 'p',
            'output pretty pseudo-xml: no "&abc;" entities (default)'),
           ('test', 't', 'output test-ready data (input & expected output, '
            'ready to be copied to a parser test module)'),
           ('rawxml', 'r', 'output raw XML'),
           ('styledxml=', 's', 'output raw XML with XSL style sheet reference '
            '(filename supplied in the option argument)'),
           ('xml', 'x', 'output pretty XML (indented)'),
           ('debug', 'd', 'debug mode (lots of output)'),
           ('help', 'h', 'show help text')]
"""See distutils.fancy_getopt.FancyGetopt.__init__ for a description of the
data structure: (long option, short option, description)."""

def usage():
    print usage_header
    for longopt, shortopt, description in options:
        if longopt[-1:] == '=':
            opts = '-%s arg, --%sarg' % (shortopt, longopt)
        else:
            opts = '-%s, --%s' % (shortopt, longopt),
        print '%-15s' % opts,
        if len(opts) > 14:
            print '%-16s' % '\n',
        while len(description) > 60:
            limit = description.rindex(' ', 0, 60)
            print description[:limit].strip()
            description = description[limit + 1:]
            print '%-15s' % ' ',
        print description

def _pretty(input, document, optargs):
    return document.pformat()

def _rawxml(input, document, optargs):
    return document.asdom().toxml()

def _styledxml(input, document, optargs):
    docnode = document.asdom().childNodes[0]
    return '%s\n%s\n%s' % (
          '<?xml version="1.0" encoding="ISO-8859-1"?>',
          '<?xml-stylesheet type="text/xsl" href="%s"?>' % optargs['styledxml'],
          docnode.toxml())

def _prettyxml(input, document, optargs):
    return document.asdom().toprettyxml('    ', '\n')

def _test(input, document, optargs):
    tq = '"""'
    output = document.pformat()         # same as _pretty()
    return """\
    totest['change_this_test_name'] = [
[%s\\
%s
%s,
%s\\
%s
%s],
]
""" % ( tq, escape(input.rstrip()), tq, tq, escape(output.rstrip()), tq )

def escape(text):
    """
    Return `text` in a form compatible with triple-double-quoted Python strings.
    """
    text = text.replace('\\', '\\\\')   # escape backslashes
    text = text.replace('"""', '""\\"') # break up triple-double-quotes
    text = text.replace(' \n', ' \\n\\\n') # protect trailing whitespace
    return text

_outputFormatters = {
    'rawxml': _rawxml,
    'styledxml': _styledxml,
    'xml': _prettyxml,
    'pretty' : _pretty,
    'test': _test
    }

def format(outputFormat, input, document, optargs):
    formatter = _outputFormatters[outputFormat]
    return formatter(input, document, optargs)

def getArgs():
    if os.name == 'mac' and len(sys.argv) <= 1:
        return macGetArgs()
    else:
        return posixGetArgs(sys.argv[1:])

def posixGetArgs(argv):
    outputFormat = 'pretty'
    # convert fancy_getopt style option list to getopt.getopt() arguments
    shortopts = ''.join([option[1] + ':' * (option[0][-1:] == '=')
                         for option in options if option[1]])
    longopts = [option[0] for option in options if option[0]]
    try:
        opts, args = getopt.getopt(argv, shortopts, longopts)
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    optargs = {'debug': 0}
    for o, a in opts:
        if o in ['-h', '--help']:
            usage()
            sys.exit()
        elif o in ['-r', '--rawxml']:
            outputFormat = 'rawxml'
        elif o in ['-s', '--styledxml']:
            outputFormat = 'styledxml'
            optargs['styledxml'] = a
        elif o in ['-x', '--xml']:
            outputFormat = 'xml'
        elif o in ['-p', '--pretty']:
            outputFormat = 'pretty'
        elif o in ['-t', '--test']:
            outputFormat = 'test'
        elif o in ['-d', '--debug']:
            optargs['debug'] = 1
        else:
            raise getopt.GetoptError, "getopt should have saved us!"
    if len(args) > 1:
        print "Only one file at a time, thanks."
        usage()
        sys.exit(1)
    if len(args) == 1:
        inputFile = open(args[0])
    else:
        inputFile = sys.stdin
    return inputFile, outputFormat, optargs

def macGetArgs():
    import EasyDialogs
    EasyDialogs.Message("""\
In the following window, please:

1. Choose an output format from the "Option" list.
2. Click "Add" (if you don't, the default format will
   be "pretty").
3. Click "Add existing file..." and choose an input file.
4. Click "OK".""")
    optionlist = [(longopt, description)
                  for (longopt, shortopt, description) in options]
    argv = EasyDialogs.GetArgv(optionlist=optionlist, addnewfile=0, addfolder=0)
    return posixGetArgs(argv)

def main():
    inputFile, outputFormat, optargs = getArgs() # process cmdline arguments
    parser = Parser()
    input = inputFile.read()
    document = docutils.utils.newdocument(debug=optargs['debug'])
    parser.parse(input, document)
    output = format(outputFormat, input, document, optargs)
    print output,


if __name__ == '__main__':
    sys.stderr = sys.stdout
    main()
