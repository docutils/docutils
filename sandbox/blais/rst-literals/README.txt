================================================================
   rst-literals: Extracts literal blocks from a ReST document
================================================================
:Author: Martin Blais <blais@furius.ca>
:Date: 2007
:Abstract:

    Extracts literal blocks from a ReST document and save them out to
    files.

    Each literal block can be typed by placing a string like
    ``#!<type>`` on the first line of the literal block. This can be
    done, for example, #to mark some literal blocks as SQL code.
    Literal blocks that are not typed are #associated to the type
    'generic'.



