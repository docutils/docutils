import sys
import zipfile
from cStringIO import StringIO

from docutils import core, io

outfile = sys.argv[2]

import EffDirectives
import EffParser
import EffMap

class OO: pass
class MIF: pass
writer = MIF
writer = OO

if writer is OO:
    import OOtext
    import OOwriter as EffWriter
elif writer is MIF:
    import MIFwriter as EffWriter
else:
    raise "Bad writer"

pub = core.Publisher(writer=EffWriter.Writer(EffMap.StyleMap))
inliner = EffParser.Inliner()
pub.set_reader('standalone', EffParser.Parser(inliner=inliner), 
    'restructuredtext')
settings = pub.get_settings()
#settings.doctitle_xform = 0
settings.traceback = 1
pub.source = io.FileInput(source_path=sys.argv[1])
pub.destination = io.StringOutput(encoding='utf-8')
content = pub.publish()

if writer is OO:
    manifest_list = [
        ('content.xml', content),
        ('styles.xml', OOtext.styles)
        ]

    manifest_entries = []
    for docname, _ in manifest_list:
        manifest_entries.append(OOtext.manifest_format % docname)
    manifest = OOtext.manifest % '\n '.join(manifest_entries)
    manifest_list.append( ('META-INF/manifest.xml', manifest) )

    zip = zipfile.ZipFile(outfile, "w")
    for docname, contents in manifest_list:
        zinfo = zipfile.ZipInfo(docname)
        zip.writestr(zinfo, contents)
    zip.close()
elif writer is MIF:
    file(outfile, 'wb').write(content)
