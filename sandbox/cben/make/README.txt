`Makefile.docutils <Makefile.docutils>`_ is a makefile that handles most
things one would want to do with docutils.  For minimal applications it can be
used directly; typically you would include it in your makefiles, e.g.::

    DOCS = foo.txt bar.txt
    
    include Makefile.docutils

This makefile is very flexible and can be configured by setting many
variables.  It also supports pre/postprocessing, currently with
`sandbox/cben/rolehack <../rolehack>`_ scripts and rudimentary
output-format-dependant convertion of ``.*`` names with sed scripts.

Enjoy and tell me (cben@users.sf.net) if you miss any functionality or
flexibility...
