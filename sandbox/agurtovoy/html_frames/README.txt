
Docutils HTML/frames writer
***************************

This is an implementation of Docutils__ writer that produces "framed"
or "chunked" HTML documents. Please see `Docutils Sandbox
repository`__ for the latest and greatest version.

Note that the writer requires your reStructuredText__ documents to contain 
the `contents directive`__ that instructs the docutils front-end to 
generate a table of contents used by the writer to determine how to split
the document.

After the writer is installed (``python setup.py install``), its typical
invocation looks like this::

    rst2htmlframes.py README.txt README.html

Examples of writer's output are available at http://www.mywikinet.com/docutils/
and http://boost.org/libs/mpl/doc/.

Known issues:

  - ``--no-doc-title`` option is not supported

Author: `Aleksey Gurtovoy`__


__ http://docutils.sourceforge.net/
__ http://docutils.sourceforge.net/sandbox/README.html
__ http://docutils.sourceforge.net/rst.html
__ http://docutils.sourceforge.net/spec/rst/directives.html#table-of-contents
__ mailto:agurtovoy@meta-comm.com
