import sys
import os

SOURCE_EXT = '.fig'

TypeList = {
    'eps': {
        'ext': '.eps',
        'cmd': 'fig2dev -L eps %s %s'
        },
    'pdf': {
        'ext': '.pdf',
        'cmd': 'fig2dev -L pdf %s %s'
        }
    }

type = TypeList[sys.argv[1]]
SourceDir = sys.argv[2]
DestDir = sys.argv[3]

for name in os.listdir(SourceDir):
    base, ext = os.path.splitext(name)
    if ext == SOURCE_EXT:
        src = SourceDir + '/' + name
        dest = DestDir + '/' + base + type['ext']
        cmd = type['cmd'] % (src, dest)
        print cmd
        os.system(cmd)
