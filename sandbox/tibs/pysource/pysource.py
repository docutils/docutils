#! /usr/bin/env python
"""A simple command line interface for the pysource package

This will become more interesting later on...
"""

import os
import sys
import getopt

import docutils.nodes
import docutils.utils

# Local modules
import utils
import visit
import html

import transform

__docformat__ = "reST"


# ----------------------------------------------------------------------
def rest_document(filename):
    """Return an reST document.
    """

    from docutils.parsers.rst import Parser

    file = open(filename)
    try:
        text = file.read()
    finally:
        file.close()

    parser = Parser()
    docroot = docutils.utils.new_document()
    parser.parse(text,docroot)
    return docroot


# ----------------------------------------------------------------------
# I don't much like the Unix command line style (I always thought the
# VMS approach was much more flexible and sensible), but it *does* seem
# to be what people expect these days...

options = [
    # long option, short, description
    ("verbose",    "v",   "Report on progress in more detail"),
    ("quiet",      "q",   "Suppress normal progress messages"),
    ("text",       "t",   "The input file is a plain (text) reST file"),
    ("show",       "s",   "Output basic information about the input"),
    ("ast",        "a",   "Output a representation of the AST"),
    ("xml",        "x",   "Output an XML representation of the input"),
    ("html",       "h",   "Output an HTML representation of the input [default]"),
    ("pretty",     "p",   "Output a 'pretty' representation of the input"),
    ("doctest",    "d",   "Treat a reST file as doctest input."),
    ("help",       "h",   "Show 'help' information"),
    ("new",        "n",   "Use David Goodger's HTML Writer (sort of)"),
    ("stdout",     "",    "Write output to stdout, instead of a file"),
    ]

def print_usage():
    docstring = visit.Docstring(main.__doc__)
    docstring.show(sys.stdout)
    print
    print "    <switches> are:"
    print
    for longopt, shortopt, description in options:
        if shortopt:
            print "      -%s, --%-9s %s"%(shortopt,longopt,description)
        else:
            print "          --%-9s %s"%(longopt,description)

def main():
    """The command line interface to docutil's Python documentation extractor.

    Usage: ``pysource.py <switches> <inpath> [<outfile>]``

        <inpath> is the path to a package or module.

        <outfile> is the path to the output file. If it's not given, then
        output will be written to a file with the same name as the input
        file, but defaulting to the current directory, and with extension
        derived from the type of output:

        - show   -> ``.show``
        - ast    -> ``.ast``
        - xml    -> ``.xml``
        - html   -> ``.html``
        - pretty -> ``.pretty``

        (unless --stdout is requested). The default is --html.

        Note that progress messages (and ``verb`` information) are written
        to ``sys.stderr``.
    """

    if len(sys.argv) <= 1:
        print "Not enough arguments"
        print_usage()
        return

    shortopts = ''.join([option[1] for option in options])
    longopts = [option[0] for option in options]
    try:
        opts, args = getopt.getopt(sys.argv[1:], shortopts, longopts)
    except getopt.GetoptError,detail:
        print "Problem with options:",detail
        print_usage()
        return

    verbose = 0
    quiet   = 0
    input_format = "python"
    output_format = "html"
    want_docutilstree = 1
    new_writer = 0
    write_stdout = 0

    for opt, arg in opts:
        if opt in ["-?","--help"]:
            print_usage()
            return
        elif opt in ["-n","--new"]:
            new_writer = 1
            output_format = "html"
            print >>sys.stderr, "Using new HTML Writer"
        elif opt in ["-v","--verbose"]:
            verbose = 1
        elif opt in ["-q","--quiet"]:
            quiet = 1
        elif opt in ["-t","--text"]:
            input_format = "text"
        elif opt in ["-s","--show"]:
            output_format = "show"
            want_docutilstree = 0
        elif opt in ["-a","--ast"]:
            output_format = "ast"
            want_docutilstree = 0
        elif opt in ["-x","--xml"]:
            output_format = "xml"
        elif opt in ["-h","--html"]:
            output_format = "html"
        elif opt in ["-d","--doctest"]:
            output_format = "doctest"
        elif opt in ["-p","--pretty"]:
            output_format = "pretty"
        elif opt == "--stdout":
            write_stdout = 1
        else:
            raise getopt.GetoptError, "getopt should have saved us!"

    if len(args) == 0 or len(args) > 2:
        print "Please specify an input and an (optional) output"
        print_usage()
        return

    if input_format == "text" and \
       output_format not in ["xml","html","pretty","doctest"]:
        print "--text only supports --xml, --html, --pretty or --doctest"
        print_usage()
        return

    if input_format == "text" and output_format == "doctest" \
       and len(args) == 2:
        print "--doctest does not accept an output file"
        # Should it?
        print_usage()
        return

    if write_stdout and len(args) == 2:
        print "It doesn't make sense to specify --stdout and an output file"
        print_usage()
        return

    filename = args[0]
    if len(args) == 2:
        if not quiet:
            print >>sys.stderr, "... Output will go to",args[1]
        outstream = open(args[1],"w")
    elif write_stdout:
        if not quiet:
            print >>sys.stderr, "... Output will go to standard output"
        outstream = sys.stdout
    else:
        head,tail = os.path.split(filename)
        base,ext  = os.path.splitext(tail)
        outname = "%s.%s"%(base,output_format)
        if not quiet:
            print >>sys.stderr, "... Output will go to",outname
        outstream = open(outname,"w")

    try:
        if output_format == "doctest":
            from doctest import Tester
            if not quiet: print >>sys.stderr, "*** Doctesting the document"
            t = Tester(globs={},verbose=verbose)
            (fail,total) = t.runstring(open(filename).read(),filename)
            if not quiet and not verbose:
                print >>sys.stderr, "*** ",
                if fail:
                    print "%d of"%fail,
                if total == 1:
                    print "1 example",
                else:
                    print "%d examples"%total,
                if fail:
                    print "failed"
                else:
                    print "passed"
        elif input_format == "text":
            if not quiet: print >>sys.stderr, "*** Making the document"
            document = rest_document(filename)
        else:
            if os.path.isdir(filename):
                print >>sys.stderr, "*** Parsing the Python package",filename
                thing = visit.Package(None, filename)
            else:
                print >>sys.stderr, "*** Parsing the Python file",filename
                thing = visit.Module(None, filename)

            if want_docutilstree:
                if not quiet:
                    print >>sys.stderr, "*** Making the document"
                with_groups = not new_writer
                process = transform.Process(with_groups)
                document = process(thing)

        if output_format == "show":
            thing.show(outstream)
        elif output_format == "ast":
            thing.show_ast(outstream)
            print
        elif output_format == "pretty":
            outstream.write(document.pformat(indent="  "))
        elif output_format == "xml":
            if not quiet: print >>sys.stderr, "*** Producing a DOM tree"
            domtree = document.asdom()
            if not quiet: print >>sys.stderr, "    Writing XML"
            domtree.writexml(outstream,indent="", addindent="  ",newl="\n")
        elif output_format == "html":
            if not quiet: print >>sys.stderr, "*** Writing HTML"
            if new_writer:
                from docutils.writers.html4css1 import Writer
                writer = Writer()
                writer.write(document,outstream)
            elif input_format == "text":
                writer = html.Writer()
                writer(document,outstream)
            else:
                writer = html.PythonWriter()
                writer(document,outstream)
    finally:
        if outstream != sys.stdout:
            outstream.close()


# ----------------------------------------------------------------------
if __name__ == "__main__":
    main()
