#!/usr/bin/env python

"""
:Author: Engelbert Gruber
:Contact: grubert@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

A minimal front-end to the Docutils Publisher.

This module takes advantage of the default values defined in `publish()`.
"""

import sys

# sandbox version only
import docutils.writers
if not docutils.writers._writer_aliases.has_key('pdf'):
    docutils.writers._writer_aliases['pdf'] = 'rlpdf'
# end sandbox hack

from docutils.core import publish
from docutils import utils



reporter = utils.Reporter(2, 4)
#reporter.setconditions('nodes.Node.walkabout', 2, 4, debug=1)

if len(sys.argv) == 2:
    publish(writer_name='pdf', source=sys.argv[1], reporter=reporter)
elif len(sys.argv) == 3:
    publish(writer_name='pdf', source=sys.argv[1], destination=sys.argv[2],
            reporter=reporter)
elif len(sys.argv) > 3:
    print >>sys.stderr, 'Maximum 2 arguments allowed.'
    sys.exit(1)
else:
    publish()
