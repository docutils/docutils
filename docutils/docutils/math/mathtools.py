#!/usr/bin/env python
# -*- coding: utf8 -*-
# :Copyright: © 2011 Günter Milde.
#             Released  without warranties or conditions of any kind
#             under the terms of the Apache License, Version 2.0
#             http://www.apache.org/licenses/LICENSE-2.0
# :Id: $Id$

# mathtools.py: helpers for Docutils math support
# ============================================================

def pick_math_environment(code, numbered=False):
    """Return the right math environment to display `code`.

    The test simply looks for line-breaks (``\\``) outside environments.
    Multi-line formulae are set with ``align``, one-liners with
    ``equation``.

    If `numbered` evaluates to ``False``, the "starred" versions are used
    to suppress numbering.
    """
    # cut out environment content:
    chunks = code.split(r'\begin{')
    toplevel_code = ''.join([chunk.split(r'\end{')[-1]
                             for chunk in chunks])
    if toplevel_code.find(r'\\') >= 0:
        env = 'align'
    else:
        env = 'equation'
    if not numbered:
        env += '*'
    return env
