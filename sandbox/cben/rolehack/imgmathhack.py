#!/usr/bin/env python
"""
Treat the default, ``texmath`` and ``texdisplay`` as LaTeX math and convert to
inline images.

.. note::
    This runs external commands and leaves files after itself!  You need:

    - ``tex_to_images`` (part of ``festival``, does anybody know a tool that
      is more commonly availiable?  It's a Perl script which could be asily
      recoded in Python but I'm too lazy...).  It, in turn, relies upon:

      - LaTeX
      - ``dviselect`` (part of ``dviutils``)
      - ``dvips``
      - Ghoscript
      - netpbm tools
"""

import os, os.path

from rolehack import *

class Tex_to_images(object):
    """Feeds math to ``tex_to_images``.  Always goes through ppm."""
    def __init__(self, dir='./mathhack', out_pattern='mathhack_NNN',
                 options='-s 1.5', converter='pnmtopng', extension='.png'):
        self.counter = 1
        self.options = options
        self.dir = dir
        self.out_pattern = out_pattern
        self.converter = converter
        self.extension = extension
    def process(self, text):
        """Returns output filename."""
        self.fname = self.out_pattern.replace('NNN', str(self.counter))
        self.counter += 1
        fpath = self.fpath = os.path.join(self.dir, self.fname)
        f = file(fpath, 'w')
        f.write('@Start\n%s\n@End\n' % (text,))
        f.close()
        os.system(('tex_to_images -f ppm -d %(dir)s -o %(fname)s.ppm '
                   '%(options)s < %(fpath)s >& /dev/null' % vars(self)))
        if self.converter:
            os.system('%s < %s.ppm > %s%s' %
                      (self.converter, fpath, fpath, self.extension))
            fpath += self.extension
        else:
            fpath += '.ppm'
        return fpath
    def texmath(self, text):
        return 'image:: %s\n    :align: middle\n' % (self.process(text),)
    def texdisplay(self, text):
        return 'image:: %s\n    :align: center\n' % (self.process(text),)

child = Tex_to_images()
texmath = child.texmath
texdisplay = child.texdisplay

main({'texmath': texmath, 'texdisplay': texdisplay}, default=texmath)
