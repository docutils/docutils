"""abc.py

Implements a 'music' directive that accepts musical passages written
in ABC notation.  Requires that abcm2ps and Ghostscript be installed.

.. music:: 

    % Sample file to test various features of abc2ps
    X:1
    T:Scale
    M:C
    K: clef=bass
     "C,"C,"D,"D,

"""

import os, sys, tempfile, popen2

from docutils import nodes
from docutils.parsers.rst.directives import register_directive

music_counter = 1

def ABCDirective (name, arguments, options, content, lineno,
                  content_offset, block_text, state, state_machine):
    abc_temp = tempfile.NamedTemporaryFile("wt")
    abc_temp.write("\n".join(content))
    abc_temp.flush()
    fd, ps_temp = tempfile.mkstemp()
    global music_counter
    output_path = state.document.settings._destination or '.' 
    output_name = os.path.join(output_path, 'music-%i.png' % music_counter)
    music_counter += 1
    sts1 = sts2 = sts3 = 0
    pipe = popen2.Popen3('abcm2ps -O/dev/stdout - < %s > %s'
                         % (abc_temp.name, ps_temp),
                         capturestderr=True
                         )
    pipe.tochild.close()
    pipe.fromchild.read()
    errors = pipe.childerr.read()
    errors = parse_errors(errors, lineno)
    sts1 = pipe.wait()
    if sts1 == 0:
        sts2 = os.system('gs -q -sOutputFile=%s -sDEVICE=pngmono - <%s'
                    % (output_name, ps_temp))
    if sts2 == 0:
        sts3 = os.system('gs -q -sOutputFile=%s -sDEVICE=pngmono - <%s'
                    % (output_name, ps_temp))
    try:
        os.unlink(ps_temp)
    except os.error:
        pass
    
    abc_temp.close()
    if sts1 != 0 or sts2 != 0:
        error = state_machine.reporter.error(
                  'Error processing music directive:\n\t' +
                  ('\n\t'.join(errors)),
                  nodes.literal_block(block_text, block_text),
                  line=lineno)
        return [error]
    else:
        # Crop excess whitespace from the image
        crop_image(output_name)
        
        # Return an image directive.
        options['uri'] = os.path.basename(output_name)
        return [nodes.image(output_name, **options)]

import re
error_pat = re.compile('(in line )(\d+)')
def parse_errors (errors, line_start):
    lines = errors.split('\n')
    lines =  [ line for line in lines if line.startswith('Error')]
    def f (m):
        return m.group(1) + str(line_start+int(m.group(2)))
    lines = [ error_pat.sub(f, line) for line in lines]
    return lines


def crop_image (filename):
    """Reads the image specified by filename, crops excess space off it,
    and writes it out to the same file."""
    from PIL import Image
    image = Image.open(filename)
    for edge in 'NSWE':
        image = _crop(image, edge)
    image.save(filename)   

def _crop (image, edge):
    im_x, im_y = image.size
    if edge == 'N':
        start = (0,0)
        scan_increment = (1,0)
        line_increment = (0,1)
    elif edge == 'S':
        start = (0,im_y-1)
        scan_increment = (1,0)
        line_increment = (0,-1)
    elif edge == 'E':
        start = (im_x-1,0)
        scan_increment = (0,1)
        line_increment = (-1,0)
    elif edge == 'W':
        start = (0,0)
        scan_increment = (0,1)
        line_increment = (1,0)

    # Look for blank lines
    def is_white (color):
        return color == 255
    
    def is_blank (x,y):
        x_inc, y_inc = scan_increment
        while 0 <= x < im_x and 0 <= y < im_y:
            point = image.getpixel((x,y))
            if not is_white(point):
                return False
            x += x_inc
            y += y_inc
        return True

    # Look for blank edges, jumping in increments of JUMP pixels.
    JUMP = 5
    x, y = start
    x_inc, y_inc = line_increment
    while is_blank(x,y):
        x += x_inc * JUMP
        y += y_inc * JUMP

    # Found a non-blank line, so scan back
    x -= x_inc
    y -= y_inc
    while not is_blank(x,y):
        x -= x_inc
        y -= y_inc

    # OK; x,y are now on the first blank line, so crop it
    if edge == 'N':
        box = (0,y, im_x, im_y)
    elif edge == 'S':
        box = (0,0, im_x, y)
    elif edge == 'E':
        box = (0,0, x, im_y)
    elif edge == 'W':
        box = (x,0, im_x, im_y)
        
    image = image.crop(box)
    image.load()
    return image
        
        
ABCDirective.content = True

def register():
    register_directive('music', ABCDirective)
