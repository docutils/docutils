#!/usr/bin/python -tt

# restblog.py
#
# Base functions for reStructuredBlog
#
# (C) Magnus Lyckå, Thinkware AB, 2003
import mx.DateTime, locale, os
locale.setlocale(locale.LC_ALL, 'C') # I want English month names etc
from docutils.core import publish_string, default_description

source_pattern = 'blog*.txt'
timestamp_format = '%Y-%m-%d %H:%M'

class Metadata:
    def __init__(self, fn):
        self.fn = fn

    def timestamp(self):
        return mx.DateTime.localtime(os.stat(self.fn)[-2])

    def subject(self):
        return file(self.fn).readline().strip()

    def targetname(self):
        return self.fn[:-3]+'html'

def makepage(fn, text):
    #text = ('<html><head><title>%s</title><head>\n'
    #        '<body>%s</body></html>')
    #file(fn+'.html', 'w').write(text % (title, body))
    file(fn+'.html', 'w').write(publish_string(text, writer_name='html'))
