#!/usr/bin/env python
"""
Convert latex math to images.  Treats the default and ``texmath`` roles as
inline LaTeX math and the ``texmath::`` directive as display latex math.

If you include a file which also needs mathhack/imgmathhack preprocessing,
write a name containing ``.mathhack`` in the include directive and it will be
replaced with ``.imgmathhack`` when preprocessed by this script (of course,
you should create both preprocessed versions of the file).

.. note::
    This runs external commands and leaves files after itself!  To reduce
    running time when images are not changed and to reuse images for equal
    fomulas, image names are md5 of the formula (hoping that no collisions
    will happen) and images that already exist are not rebuilt.  You should
    purge the ``imgmath`` subdirectory manually to get rid of unused formulas.

    You'll need:

    - ``tex_to_images``, last version seems to live at the `speech_tools
      CVS`__.
      
      __ http://cvs.sourceforge.net/viewcvs.py/*checkout*/
         emu/speech_tools/scripts/tex_to_images.prl?rev=HEAD

      It, in turn, relies upon:

      - LaTeX
      - ``dviselect`` (part of ``dviutils``)
      - ``dvips``
      - Ghoscript
      - netpbm tools
"""

import os, os.path, md5

from rolehack import *

class Tex_to_images(object):
    """Feeds math to ``tex_to_images``.  Always goes through ppm."""
    def __init__(self, dir='./imgmath', options='-s 1.5',
                 converter='pnmtopng', extension='.png'):
        try:
            os.mkdir(dir)
        except OSError:
            pass
        self.options = options
        self.dir = dir
        self.converter = converter
        self.extension = extension
    def process(self, text):
        """Returns output filename."""
        dir = self.dir
        extension = self.extension
        options = self.options
        converter = self.converter
        fname = md5.new(text).hexdigest()
        fpath = os.path.join(dir, fname)
        if not os.path.exists(fpath + extension):
            f = file(fpath, 'w')
            f.write('@Start\n%s\n@End\n' % (text,))
            f.close()
            os.system(('tex_to_images -f ppm -d %(dir)s -o %(fname)s.tmp '
                       '%(options)s < %(fpath)s >& /dev/null' % vars()))
            if self.converter:
                os.system('%s < %s.tmp > %s%s' %
                          (self.converter, fpath, fpath, extension))
            else:
                os.rename(fpath + '.tmp', fpath + '.ppm')
            os.remove(fpath + '.tmp')
        return fpath + extension
    def texmath(self, text):
        text = ' '.join(text.split())
        src = self.process(text)
        return '''\
image:: %(src)s
    :align: middle
    :class: texmath
    :alt: %(text)s
''' % locals()
    def texdisplay(self, text):
        src = self.process(text)
        return '''\
image:: %(src)s
    :align: center
    :class: texdisplay
    :alt: %(text)s
''' % locals()

child = Tex_to_images()
texmath = child.texmath
texdisplay = child.texdisplay

def mangle_include(text):
    return 'include:: ' + text.replace('.mathhack', '.imgmathhack')

main({'texmath': texmath}, texmath,
     {'texmath': texdisplay, 'include': mangle_include})
