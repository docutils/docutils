import sys
import zipfile
from cStringIO import StringIO

from docutils import core, io

import OOdirectives
import OOtext
import OOwriter

pub = core.Publisher(writer=OOwriter.Writer())
pub.set_reader('standalone', None, 'restructuredtext')
settings = pub.get_settings()
pub.source = io.FileInput(settings, source_path=sys.argv[1])
pub.destination = io.StringOutput(settings)
content = pub.publish()

manifest_list = [
    ('content.xml', content),
    ('styles.xml', OOtext.styles)
    ]

manifest_entries = []
for docname, _ in manifest_list:
    manifest_entries.append(OOtext.manifest_format % docname)
manifest = OOtext.manifest % '\n '.join(manifest_entries)
manifest_list.append( ('META-INF/manifest.xml', manifest) )

zip = zipfile.ZipFile(sys.argv[2], "w")
for docname, contents in manifest_list:
    zinfo = zipfile.ZipInfo(docname)
    zip.writestr(zinfo, contents)
zip.close()
