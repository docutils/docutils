"""
ASCII art to image converter.

This is the main modlue that contains the parser.
See svg.py and aa.py for output modules, that can reder the parsed structure.

(C) 2006 Chris Liechti <cliechti@gmx.net>
"""

import pprint
import svg
import aa

NOMINAL_SIZE = 2
LEFT = TOP = 0
CENTER = NOMINAL_SIZE/2
RIGHT = BOTTOM = NOMINAL_SIZE

CLASS_LINE = 'line'
CLASS_STRING = 'str'
CLASS_RECTANGLE = 'rect'

# - - - - - - - - - - - - - - Shapes - - - - - - - - - - - - - - -
class Point:
    """A single point. This class is primary use is to represent coordinates
       for the other shapes.
    """
    def __init__(self, x, y):
        self.x = x
        self.y = y
        
    def __repr__(self):
        return 'Point(%r, %r)' % (self.x, self.y)

class Line:
    """Line with starting and ending point. Both ends can have arrows"""
    def __init__(self, start, end, start_style=None, end_style=None):
        self.start = start
        self.end = end
        self.start_style = start_style 
        self.end_style = end_style
    
    def __repr__(self):
        return 'Line(%r, %r, %r, %r)' % (
            self.start,
            self.end,
            self.start_style,
            self.end_style
        )

class Rectangle:
    """Rectangle with to edge coordiantes."""
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2
    def __repr__(self):
        return 'Rectangle(%r, %r)' % (self.p1, self.p2)

class Label:
    """A label at a position"""
    def __init__(self, position, text):
        self.position = position
        self.text = text
    def __repr__(self):
        return 'Label(%r, %r)' % (self.position, self.text)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class AsciiArtImage:
    """This class hold a ASCII art figure and has methods to parse it.
       The resaulting list of shapes is also stored here.
    """
    ARROW_HEADS = list('<>Vv^')
    
    def __init__(self, text):
        """Take a ASCII art figure and store it, prepare for ``recognize``"""
        #XXX TODO tab expansion
        #detect size of input image
        self.image = []
        max_x = 0
        for y, line in enumerate(text.splitlines()):
            max_x = max(max_x, len(line))
            self.image.append(line)
        self.width = max_x
        self.height = y+1
        #make sure it's rectangular
        for y, line in enumerate(self.image):
            if len(line) < max_x:
                self.image[y] = line + ' '*(max_x-len(line))
        #initialize other data structures
        self.classification = [[None]*self.width for y in range(self.height)]
        self.shapes = []
        self.nominal_size = NOMINAL_SIZE
        
    def __repr__(self):
        return '%s\n%s' % (
            '\n'.join([','.join([str(self.classification[y][x])
                for x in range(self.width)])
                    for y in range(self.height)]),
            pprint.pformat(self.shapes)
        )
        
    def __str__(self):
        """Return the original image"""
        return '\n'.join([self.image[y] for y in range(self.height)])
    
    def get(self, x, y):
        """Get character from image. Gives no error for access out of
           bounds, just returns a space. This simplifies the scanner
           functions.
        """
        try:
            return self.image[y][x]
        except IndexError:
            return ' '

    def tag(self, coordinates, classification):
        """Tag coordinates as used, store classification"""
        for x, y in coordinates:
            self.classification[y][x] = classification

    def recognize(self):
        """Try to convert ASCII are to vector graphics."""
        #XXX search for symbols
        #search for standard shapes
        for y in range(self.height):
            for x in range(self.width):
                #if not yet classified, check for a line
                character = self.image[y][x]
                if self.classification[y][x] != CLASS_LINE:
                    if character == '-':
                        self.shapes.append(self._follow_horizontal_line(x, y))
                    elif character == '|':
                        self.shapes.append(self._follow_vertical_line(x, y))
                    elif character == '_':
                        self.shapes.append(self._follow_lower_horizontal_line(x, y))
                if self.classification[y][x] is None:
                    if character == 'X' and (self.get(x+1,y) == 'X' or self.get(x,y+1) == 'X'):
                        self.shapes.append(self._follow_filled_rectangle(x, y))
                    elif character.isalnum():
                        self.shapes.append(self._follow_horizontal_string(x, y))
                    elif character == '.':
                        self.shapes.append(Point(x*NOMINAL_SIZE+CENTER,y*NOMINAL_SIZE+CENTER)) #XXX

    def _follow_vertical_line(self, x, y):
        """find a vertical line with optional arrow heads"""
        #follow line to the bottom
        _, end_y, line_end_style = self._follow_line(x, y, dy=1, line_character='|')
        #follow line to the top
        _, start_y, line_start_style = self._follow_line(x, y, dy=-1, line_character='|')
        #if a '+' follows a line, then the line is streched to hit the '+' center
        start_y_fix = end_y_fix = 0
        if self.get(x, start_y-1) == '+':
            start_y_fix = -NOMINAL_SIZE+CENTER
        if self.get(x, end_y+1) == '+':
            end_y_fix = CENTER
        #tag characters as used
        self.tag([(x, y) for y in range(start_y, end_y+1)], CLASS_LINE)
        #return the new shape object
        return Line(
            Point(x*NOMINAL_SIZE+CENTER, start_y*NOMINAL_SIZE+TOP+start_y_fix),
            Point(x*NOMINAL_SIZE+CENTER, end_y*NOMINAL_SIZE+BOTTOM+end_y_fix),
            line_start_style,
            line_end_style
        )
    
    def _follow_horizontal_line(self, x, y):
        """find a horizontal line with optional arrow heads"""
        #follow line to the right
        end_x, _, line_end_style = self._follow_line(x, y, dx=1, line_character='-')
        #follow line to the left
        start_x, _, line_start_style = self._follow_line(x, y, dx=-1, line_character='-')
        start_x_fix = end_x_fix = 0
        if self.get(start_x-1, y) == '+':
            start_x_fix = -NOMINAL_SIZE+CENTER
        if self.get(end_x+1, y) == '+':
            end_x_fix = CENTER
        self.tag([(x, y) for x in range(start_x, end_x+1)], CLASS_LINE)
        return Line(
            Point(start_x*NOMINAL_SIZE+LEFT+start_x_fix, y*NOMINAL_SIZE+CENTER),
            Point(end_x*NOMINAL_SIZE+RIGHT+end_x_fix, y*NOMINAL_SIZE+CENTER),
            line_start_style,
            line_end_style
        )

    def _follow_lower_horizontal_line(self, x, y):
        """find a horizontal line, the line is aligned to the bottom and a bit
           wider, so that it can be used for shapes like this:
              ___
           __|   |___
        """
        #follow line to the right
        end_x, _, line_end_style = self._follow_line(x, y, dx=1, line_character='_')
        #follow line to the left
        start_x, _, line_start_style = self._follow_line(x, y, dx=-1, line_character='_')
        self.tag([(x, y) for x in range(start_x, end_x+1)], CLASS_LINE)
        return Line(
            Point(start_x*NOMINAL_SIZE+LEFT-CENTER, y*NOMINAL_SIZE+BOTTOM),
            Point(end_x*NOMINAL_SIZE+RIGHT+CENTER, y*NOMINAL_SIZE+BOTTOM),
            line_start_style,
            line_end_style
        )

    def _follow_line(self, x, y, dx=0, dy=0, line_character=None):
        """helper function for all the line functions"""
        #follow line in the given direction
        while 0 <= x < self.width and 0<= y < self.height and self.get(x+dx, y+dy) == line_character:
            x += dx
            y += dy
        #check for arrow head
        if self.get(x+dx, y+dy) in self.ARROW_HEADS:
            line_end_style = 'arrow' #XXX
            x += dx
            y += dy
        else:
            line_end_style = None
        return x, y, line_end_style

    def _follow_filled_rectangle(self, start_x, start_y):
        """detect the size of a filled rectangle. width is scanned first.
           shapes like these:
           
               XXXX
               XX
           
           are detected as two rectangles.
        """
        x = start_x
        y = start_y
        #expand as fas as possible to the right
        while x < self.width and self.get(x+1, y) == 'X':
            x += 1
        #expand height as long as the width stays the same
        while y < self.height and False not in [self.get(i,y+1) == 'X' for i in range(start_x, x+1)]:
            y += 1
        for i in range(start_y, y+1):
            self.tag([(x, i) for x in range(start_x, x+1)], CLASS_RECTANGLE)
        return Rectangle(
            Point(start_x*NOMINAL_SIZE+LEFT, start_y*NOMINAL_SIZE+TOP),
            Point(x*NOMINAL_SIZE+RIGHT, y*NOMINAL_SIZE+BOTTOM),
        )
        
    def _follow_horizontal_string(self, start_x, y):
        """find a string. may contain single spaces, but the detection is
           aborted after more than one space.
           
           Text one   Text two
        """
        #follow line in the given direction
        x = start_x
        text = []
        text.append(self.get(x, y))
        is_first_space = True
        while 0 <= x < self.width \
          and (self.get(x+1, y).isalnum() \
            or (self.get(x+1, y) == ' ' and is_first_space)) \
        :
            x += 1
            text.append(self.get(x, y))
            if self.get(x, y) == ' ':
                is_first_space = False
            else:
                is_first_space = True
        if text[-1] == ' ':
            del text[-1]
            x -= 1
        self.tag([(x, y) for x in range(start_x, x+1)], CLASS_STRING)
        return Label(
            Point(start_x*NOMINAL_SIZE+LEFT, y*NOMINAL_SIZE+BOTTOM),
            ''.join(text)
        )


def render(text):
    """helper function for tests. scan the given image and create svg output"""
    aaimg = AsciiArtImage(text)
    print text
    aaimg.recognize()
    aav = aa.AsciiOutputVisitor()
    pprint.pprint(aaimg.shapes)
    aav.visit(aaimg)
    print aav
    svgout = svg.SVGOutputVisitor(
        file('aafigure_%x.svg' % (long(hash(text)) & 0xffffffffL,), 'w'),
        scale = 10
    )
    svgout.visit(aaimg)


if __name__ == '__main__':
    aaimg = AsciiArtImage("""
        ---> | ^|   |
        <--- | || --+--
        <--> | |V   |
     __             __
    |  |__  +---+  |__|
            |box|   ..
            +---+  Xenophon
    """)
    print aaimg
    aaimg.recognize()
    print "%r" % aaimg
    aav = aa.AsciiOutputVisitor()
    aav.visit(aaimg)
    print aav

