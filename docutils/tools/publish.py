#!/usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

A minimal front-end to the Docutils Publisher.

This module takes advantage of the default values defined in `publish()`.
"""

import sys
from docutils.core import publish


if len(sys.argv) == 2:
    publish(source=sys.argv[1])
elif len(sys.argv) == 3:
    publish(source=sys.argv[1], destination=sys.argv[2])
elif len(sys.argv) > 3:
    print >>sys.stderr, 'Maximum 2 arguments allowed.'
    sys.exit(1)
else:
    publish()
