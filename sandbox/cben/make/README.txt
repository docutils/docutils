`<Makefile.docutils>`_ is a makefile that handles most things one would want
to do with docutils.  For minimal applications it can be used directly;
typically you would include it in your makefiles, e.g.::

    DOCS = foo.txt bar.txt
    
    include Makefile.docutils

For quick jobs ``make DOCS=foo.txt\ bar.txt`` or just ``make`` (taking all
``*.txt`` files) will also work.
    
This makefile is very flexible and can be configured by setting many
variables.  It also supports pre/postprocessing, currently with
`sandbox/cben/rolehack <../rolehack>`_ scripts and rudimentary
output-format-dependant adaptation of ``.*`` names with sed scripts.

The makefile is heavily commented and the comments themeselves are in reST
(sort of literate programming).  A script (`<make2rst.py>`_) is provided that
can convert the whole file to legal reST - run ``make`` in this directory to
build the docs (``make ps pdf`` would give you more formats).

Enjoy and tell me (cben@users.sf.net) if you miss any functionality or
flexibility...
