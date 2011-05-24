#!/usr/bin/env python
# -*- coding: utf8 -*-
# :Copyright: © 2011 Günter Milde.
#             Released  without warranties or conditions of any kind
#             under the terms of the Apache License, Version 2.0
#             (http://www.apache.org/licenses/LICENSE-2.0)
# :Id: $Id$

"""
This is the Docutils (Python Documentation Utilities) "math" sub-package.

It contains various modules for conversion between different math formats
(LaTeX, MathML, HTML).

:math2html:    LaTeX math -> HTML conversion from eLyXer
:latex2mathml: LaTeX math -> presentational MathML
:mathtools:    helpers for Docutils math support
:unimathsymbols2tex: Unicode symbol to LaTeX math translation table
"""


# While Docutils components can be used as parts in programs released
# under GPL v3, it is not possible to include GPLed-code in a public
# domain or Apache-2 licensed program.
#
# math2html.py is part of eLyXer_, released under the `GNU General
# Public License`_ version 3 or later.  The author kindly relicensed it
# for Docutils under the Apache License, Version 2.0.
#
# .. _Docutils policies: docs/dev/policies.txt
# .. _eLyXer: http://www.nongnu.org/elyxer/
