
Docutils HTML/frames writer
***************************

This is a writer for Docutils__ that produces "framed" HTML documents.

Note that the writer requires your reStructuredText__ documents to contain 
the `contents directive`__ that instructs the docutils front-end to 
generate a table of contents used by the writer to determine how to split
the document.

Known issues:

  - ``--no-doc-title`` option is not supported

Author: `Aleksey Gurtovoy`__


__ http://docutils.sourceforge.net/
__ http://docutils.sourceforge.net/rst.html
__ http://docutils.sourceforge.net/spec/rst/directives.html#table-of-contents
__ mailto:agurtovoy@meta-comm.com
