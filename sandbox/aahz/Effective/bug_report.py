reST_text = """
``spam``

blah blah

.. index::
    ``xyzzy``
    plugh
    spam ``eggs``
"""

import sys
import zipfile
from cStringIO import StringIO

from docutils import core, io

import EffDirectives
import EffParser
import EffMap

class MIF: pass
writer = MIF
writer = None

if writer is MIF:
    import MIFwriter as EffWriter
    pub = core.Publisher(writer=EffWriter.Writer(EffMap.StyleMap))
else:
    pub = core.Publisher()
    pub.set_writer('pseudoxml')

inliner = EffParser.Inliner()
pub.set_reader('standalone', EffParser.Parser(inliner=inliner), 
    'restructuredtext')
settings = pub.get_settings()
#settings.doctitle_xform = 0
settings.traceback = 1
pub.source = io.StringInput(reST_text)
pub.destination = io.StringOutput(encoding='utf-8')
content = pub.publish()

print content
