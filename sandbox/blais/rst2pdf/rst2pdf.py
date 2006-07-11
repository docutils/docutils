#!/usr/bin/env python

# Author: Martin Blais
# Contact: blais@furius.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
A minimal front end to the Docutils Publisher, producing LaTeX, compiling the
LaTeX file, getting the produced output to the destination location and finally
deleting all the messy temporary files that this process generates.
"""

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

# stdlib imports
import os, tempfile, shutil
from os.path import *
from subprocess import Popen, call, PIPE

# docutils imports
from docutils.core import default_usage, default_description, Publisher


description = ('Generates PDF documents from standalone reStructuredText '
               'sources, via LaTeX.  ' + default_description)


def publish_cmdline(reader=None, reader_name='standalone',
                    parser=None, parser_name='restructuredtext',
                    writer=None, writer_name='pseudoxml',
                    settings=None, settings_spec=None,
                    settings_overrides=None, config_section=None,
                    enable_exit_status=1, argv=None,
                    usage=default_usage, description=default_description):
    """
    See docutils.core.publish_cmdline.

    We just modified this function to return the parsed destination file.
    """
    pub = Publisher(reader, parser, writer, settings=settings)
    pub.set_components(reader_name, parser_name, writer_name)
    output = pub.publish(
        argv, usage, description, settings_spec, settings_overrides,
        config_section=config_section, enable_exit_status=enable_exit_status)
    return output, pub.settings._source, pub.settings._destination


def main():
    """
    Main program.
    """
    # Convert the input file into a string of LaTeX.
    # A string of LaTeX.  Ah, ah.
    output, src, dest = publish_cmdline(writer_name='latex',
                                        description=description)

    # If not specified, compute the name of the destination file.
    srcbase = splitext(src)[0]
    if dest is None:
        dest = srcbase + '.pdf'
    elif isdir(dest):
        dest = join(dest, srcbase + '.pdf')

    # Write the LaTeX output file to a temporary file in the same directory as
    # the input file.  We have to use that directory because of the possibility
    # of relative filenames/included files.
    cwd = dirname(src) or os.getcwd()
    pre_files = os.listdir(cwd)
    
    f = tempfile.NamedTemporaryFile('w', dir=cwd,
                                    prefix='rst2pdf-', suffix='.tex')
    f.write(output)
    f.flush()
    
    tex_fn = basename(f.name)
    tmp_prefix = splitext(tex_fn)[0]
    try:
        # Invoke PDF LaTeX on the LaTeX file.
        p = Popen(('pdflatex', '-interaction=nonstopmode', f.name), cwd=cwd)
        rc = p.wait()
        if rc != 0:
            raise SystemExit("Error: Non-zero exit status.")

        # Copy the temporary file to its destination.
        pdf_fn = splitext(f.name)[0] + '.pdf'
        shutil.copyfile(pdf_fn, dest)

    finally:
        # Clean up the temporary files created by the process.
        post_files = os.listdir(cwd)
        diff_files = frozenset(post_files) - frozenset(pre_files)

        for fn in diff_files:
            if fn == tex_fn:
                continue
            if fn.startswith(tmp_prefix):
                print "Deleting '%s'" % fn
                os.unlink(join(cwd, fn))

    # Delete the temporary LaTeX file.
    f.close()
    del f

    
if __name__ == '__main__':
    main()

