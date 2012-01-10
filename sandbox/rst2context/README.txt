rst2context
===========

This is an attempt to rewrite the latex2e writer to instead produce
ConTeXt (http://wiki.contextgarden.net) code. Currently the code is a
dirty mixture of the original latex2e writer with new code
implementing ConTeXt support. Everybody is welcome to help; contact me
at alquaknaa (at) gmail (dot) com.

Current state
-------------

Currently there is very basic support of:

- the section hierarchy
- styling (*emph*, **bold**, ``literal``)
- ordered and unordered lists (including nesting, but needs testing).

This document can be (though barely) processed by rst2context.
