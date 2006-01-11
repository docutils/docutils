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

CLASS_LINE = 'line'
CLASS_STRING = 'str'
CLASS_RECTANGLE = 'rect'
CLASS_JOIN = 'join'


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
    def __init__(self, start, end, thick=False):
        self.thick = thick
        #accept complex numbers as coordinates too, but convert them to Point instaces
        if not isinstance(start, Point):
            start = Point(start.real, start.imag)
        if not isinstance(end, Point):
            end = Point(end.real, end.imag)
        #now save the points
        self.start = start
        self.end = end
    
    def __repr__(self):
        return 'Line(%r, %r)' % (self.start, self.end)

class Rectangle:
    """Rectangle with to edge coordiantes."""
    def __init__(self, p1, p2):
        if not isinstance(p1, Point):
            p1 = Point(p1.real, p1.imag)
        if not isinstance(p2, Point):
            p2 = Point(p2.real, p2.imag)
        self.p1 = p1
        self.p2 = p2
    def __repr__(self):
        return 'Rectangle(%r, %r)' % (self.p1, self.p2)

class Circle:
    """Rectangle with to edge coordiantes."""
    def __init__(self, center, radius):
        if not isinstance(center, Point):
            center = Point(center.real, center.imag)
        self.center = center
        self.radius = radius
        
    def __repr__(self):
        return 'Circle(%r, %r)' % (self.center, self.radius)

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
    ARROW_HEADS = list('<>Vv^oO#')
    
    def __init__(self, text, aspect_ratio=1):
        """Take a ASCII art figure and store it, prepare for ``recognize``"""
        self.aspect_ratio = float(aspect_ratio)
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
        if 0 <= x < self.width and 0 <= y < self.height:
            return self.image[y][x]
        else:
            return ' '

    def tag(self, coordinates, classification):
        """Tag coordinates as used, store classification"""
        for x, y in coordinates:
            self.classification[y][x] = classification

    # Coordinate converison
    def left(self, x):          return x*NOMINAL_SIZE*self.aspect_ratio
    def hcenter(self, x):       return (x+0.5)*NOMINAL_SIZE*self.aspect_ratio
    def right(self, x):         return (x+1)*NOMINAL_SIZE*self.aspect_ratio
    def top(self, y):           return y*NOMINAL_SIZE
    def vcenter(self, y):       return (y+0.5)*NOMINAL_SIZE
    def bottom(self, y):        return (y+1)*NOMINAL_SIZE

    def recognize(self):
        """Try to convert ASCII are to vector graphics."""
        #XXX search for symbols
        #~ #search for long strings
        #~ for y in range(self.height):
            #~ for x in range(self.width):
                #~ character = self.image[y][x]
                #~ if self.classification[y][x] is None:
                    #~ if character.isalnum():
                        #~ self.shapes.extend(self._follow_horizontal_string(x, y, min_length=1))
        #search for standard shapes
        for y in range(self.height):
            for x in range(self.width):
                #if not yet classified, check for a line
                character = self.image[y][x]
                if self.classification[y][x] != CLASS_LINE:
                    if character == '-':
                        self.shapes.extend(self._follow_horizontal_line(x, y))
                    elif character == '|':
                        self.shapes.extend(self._follow_vertical_line(x, y))
                    elif character == '_':
                        self.shapes.extend(self._follow_lower_horizontal_line(x, y))
                    elif character == '=':
                        self.shapes.extend(self._follow_horizontal_line(x, y, thick=True))
                if character == '+':
                    self.shapes.extend(self._plus_joiner(x, y))
                if self.classification[y][x] is None:
                    if character == 'X' and (self.get(x+1,y) == 'X' or self.get(x,y+1) == 'X'):
                        self.shapes.extend(self._follow_filled_rectangle(x, y))
                    #~ elif character.isalnum():
                        #~ self.shapes.extend(self._follow_horizontal_string(x, y))
                    #~ elif character == '.':
                        #~ self.shapes.append(Point(self.hcenter(x),self.vcenter(y))) #XXX
                    elif character == '*':
                        self.shapes.append(Circle(Point(self.hcenter(x),self.vcenter(y)), NOMINAL_SIZE/2.0)) #XXX
                if character in '\\/':
                    self.shapes.extend(self._follow_rounded_edge(x, y))
        #search for short strings too
        for y in range(self.height):
            for x in range(self.width):
                character = self.image[y][x]
                if self.classification[y][x] is None:
                    if character != ' ':
                        self.shapes.extend(self._follow_horizontal_string(x, y, accept_anything=True))

    # - - - - - - - - - helper function for some shapes - - - - - - - - -
    # use complex numbers as 2D vectors as that means easy transformations like
    # scaling, rotation and translation
    
    def _standard_arrow(self, p1, p2):
        """-->
           return a possibly modified starting point and a list of shapes
        """
        direction_vector = p1 - p2
        direction_vector /= abs(direction_vector)
        return p1, [
            Line(p1, p1-direction_vector*1.5+direction_vector*0.5j),
            Line(p1, p1-direction_vector*1.5+direction_vector*-0.5j)
        ]

    def _reversed_arrow(self, p1, p2):
        """--<"""
        direction_vector = p1 - p2
        direction_vector /= abs(direction_vector)
        return p1-direction_vector*1.5, [
            Line(p1-direction_vector*1.5, p1+direction_vector*0.5j),
            Line(p1-direction_vector*1.5, p1+direction_vector*-0.5j)
        ]

    def _circle_head(self, p1, p2, radius=0.5):
        """--o"""
        direction_vector = p1 - p2
        direction_vector /= abs(direction_vector)
        return p1-direction_vector, [Circle(p1-direction_vector, radius)]

    def _large_circle_head(self, p1, p2):
        return self._circle_head(p1, p2, radius=0.9)

    def _rectangular_head(self, p1, p2):
        direction_vector = p1 - p2
        direction_vector /= abs(direction_vector)
        return p1-direction_vector*1.414, [
            Rectangle(p1-direction_vector-direction_vector*(0.707+0.707j),
                      p1-direction_vector+direction_vector*(0.707+0.707j))
        ]
        
    ARROW_TYPES = [
        #chr  dx  dy  arrow type
        ('>',  1,  0, '_standard_arrow'),
        ('<', -1,  0, '_standard_arrow'),
        ('^',  0, -1, '_standard_arrow'),
        ('V',  0,  1, '_standard_arrow'),
        ('v',  0,  1, '_standard_arrow'),
        ('>', -1,  0, '_reversed_arrow'),
        ('<',  1,  0, '_reversed_arrow'),
        ('^',  0,  1, '_reversed_arrow'),
        ('V',  0, -1, '_reversed_arrow'),
        ('v',  0, -1, '_reversed_arrow'),
        ('o',  1,  0, '_circle_head'),
        ('o', -1,  0, '_circle_head'),
        ('o',  0, -1, '_circle_head'),
        ('o',  0,  1, '_circle_head'),
        ('O',  1,  0, '_large_circle_head'),
        ('O', -1,  0, '_large_circle_head'),
        ('O',  0, -1, '_large_circle_head'),
        ('O',  0,  1, '_large_circle_head'),
        ('#',  1,  0, '_rectangular_head'),
        ('#', -1,  0, '_rectangular_head'),
        ('#',  0, -1, '_rectangular_head'),
        ('#',  0,  1, '_rectangular_head'),
    ]

    def get_arrow(self, character, dx, dy):
        for head, ddx, ddy, function_name in self.ARROW_TYPES:
            if character == head and dx == ddx and dy == ddy:
                return getattr(self, function_name)
                
    # - - - - - - - - - helper function for shape recognition - - - - - - - - -
    
    def _follow_vertical_line(self, x, y):
        """find a vertical line with optional arrow heads"""
        #follow line to the bottom
        _, end_y, line_end_style = self._follow_line(x, y, dy=1, line_character='|')
        #follow line to the top
        _, start_y, line_start_style = self._follow_line(x, y, dy=-1, line_character='|')
        #if a '+' follows a line, then the line is streched to hit the '+' center
        start_y_fix = end_y_fix = 0
        if self.get(x, start_y-1) == '+':
            start_y_fix = -0.5
        if self.get(x, end_y+1) == '+':
            end_y_fix = 0.5
        #tag characters as used (not the arrow heads)
        self.tag([(x, y) for y in range(start_y, end_y+1)], CLASS_LINE)
        #return the new shape object with arrows etc
        p1 = complex(self.hcenter(x), self.top(start_y+start_y_fix))
        p2 = complex(self.hcenter(x), self.bottom(end_y+end_y_fix))
        shapes = []
        if line_start_style:
            p1, arrow_shapes = line_start_style(p1, p2)
            shapes.extend(arrow_shapes)
        if line_end_style:
            p2, arrow_shapes = line_end_style(p2, p1)
            shapes.extend(arrow_shapes)
        shapes.append(Line(p1, p2))
        return shapes
    
    def _follow_horizontal_line(self, x, y, thick=False):
        """find a horizontal line with optional arrow heads"""
        if thick:
            line_character = '='
        else:
            line_character = '-'
        #follow line to the right
        end_x, _, line_end_style = self._follow_line(x, y, dx=1, line_character=line_character)
        #follow line to the left
        start_x, _, line_start_style = self._follow_line(x, y, dx=-1, line_character=line_character)
        start_x_fix = end_x_fix = 0
        if self.get(start_x-1, y) == '+':
            start_x_fix = -0.5
        if self.get(end_x+1, y) == '+':
            end_x_fix = 0.5
        self.tag([(x, y) for x in range(start_x, end_x+1)], CLASS_LINE)
        #return the new shape object with arrows etc
        p1 = complex(self.left(start_x+start_x_fix), self.vcenter(y))
        p2 = complex(self.right(end_x+end_x_fix), self.vcenter(y))
        shapes = []
        if line_start_style:
            p1, arrow_shapes = line_start_style(p1, p2)
            shapes.extend(arrow_shapes)
        if line_end_style:
            p2, arrow_shapes = line_end_style(p2, p1)
            shapes.extend(arrow_shapes)
        shapes.append(Line(p1, p2, thick=thick))
        return shapes

    def _follow_lower_horizontal_line(self, x, y):
        """find a horizontal line, the line is aligned to the bottom and a bit
           wider, so that it can be used for shapes like this:
              ___
           __|   |___
        """
        #follow line to the right
        end_x, _, line_end_style = self._follow_line(x, y, dx=1, line_character='_', arrows=False)
        #follow line to the left
        start_x, _, line_start_style = self._follow_line(x, y, dx=-1, line_character='_', arrows=False)
        self.tag([(x, y) for x in range(start_x, end_x+1)], CLASS_LINE)
        #return the new shape object with arrows etc
        p1 = complex(self.hcenter(start_x-1), self.bottom(y))
        p2 = complex(self.hcenter(end_x+1), self.bottom(y))
        return [Line(p1, p2)]

    def _follow_line(self, x, y, dx=0, dy=0, line_character=None, arrows=True):
        """helper function for all the line functions"""
        #follow line in the given direction
        while 0 <= x < self.width and 0<= y < self.height and self.get(x+dx, y+dy) == line_character:
            x += dx
            y += dy
        if arrows:
            #check for arrow head
            following_character = self.get(x+dx, y+dy)
            if following_character in self.ARROW_HEADS:
                line_end_style = self.get_arrow(following_character, dx, dy)
                x += dx
                y += dy
            else:
                line_end_style = None
        else:
            line_end_style = None
        return x, y, line_end_style
    
    def _plus_joiner(self, x, y):
        """adjacent '+' signs are connected with a line from center to center
           required for images like these:
          
              +---+         The box should be closed on all sides
              |   +--->     and the arrow start should touch the box
              +---+
        """
        result = []
        #~ for dx, dy in ((1,0), (-1,0), (0,1), (0,-1)):
        #looking right and down is sufficient as the scan is done from left
        #to right, top to bottom
        for dx, dy in ((1,0), (0,1)):
            if self.get(x+dx, y+dy) == '+':
                result.append(Line(
                    Point(self.hcenter(x), self.vcenter(y)),
                    Point(self.hcenter(x+dx), self.vcenter(y+dy))
                ))
        self.tag([(x,y)], CLASS_JOIN)
        return result


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
        return [Rectangle(
            Point(self.left(start_x), self.top(start_y)),
            Point(self.right(x), self.bottom(y)),
        )]
        
    def _follow_horizontal_string(self, start_x, y, min_length=0, accept_anything=False):
        """find a string. may contain single spaces, but the detection is
           aborted after more than one space.
           
              Text one   Text two
           
           texts are skipped if text is shorter than min_length (used for two pass scan)
           accept_anything means that all non space characters are interpreted as text
        """
        #follow line in the given direction
        x = start_x
        text = []
        text.append(self.get(x, y))
        is_first_space = True
        while 0 <= x+1 < self.width \
          and self.classification[y][x+1] is None \
          and ((accept_anything and (self.get(x+1, y) != ' ' \
                                     or (self.get(x+1, y) == ' ' and is_first_space)))
            or (self.get(x+1, y).isalnum() \
                or (self.get(x+1, y) == ' ' and is_first_space))) \
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
        if len(text) > min_length:
            self.tag([(x, y) for x in range(start_x, x+1)], CLASS_STRING)
            return [Label(
                Point(self.left(start_x), self.bottom(y)),
                ''.join(text)
            )]
        else:
            return []

    def _follow_rounded_edge(self, x, y):
        """check for rounded edges:
            /-    |     -\-    |
            |    -/      |     \-
        """            
        #XXX return Arc shapes, not lines
        result = []
        if self.get(x, y) == '/':
            # rounded rectangles
            if (self.get(x+1, y) == '-' and self.get(x, y+1) == '|'):
                result.append(Line(
                    Point(self.hcenter(x), self.bottom(y)),
                    Point(self.right(x), self.vcenter(y))
                ))
            if (self.get(x-1, y) == '-' and self.get(x, y-1) == '|'):
                result.append(Line(
                    Point(self.hcenter(x), self.top(y)),
                    Point(self.left(x), self.vcenter(y))
                ))
            if not result:
                # if used as diagonal line
                p1 = p2 = None
                if self.get(x+1, y-1) == '|':
                    p1 = Point(self.hcenter(x+1), self.top(y))
                elif self.get(x+1, y-1) == '+':
                    p1 = Point(self.hcenter(x+1), self.vcenter(y-1))
                elif self.get(x+1, y-1) == '-':
                    p1 = Point(self.right(x), self.vcenter(y-1))
                elif self.get(x+1, y-1) == '/':
                    p1 = Point(self.right(x), self.top(y))
                elif self.get(x+1, y) == '|':
                    p1 = Point(self.hcenter(x+1), self.top(y))
                elif self.get(x, y-1) == '-':
                    p1 = Point(self.right(x), self.vcenter(y-1))
                    
                if self.get(x-1, y+1) == '|':
                    p2 = Point(self.hcenter(x-1), self.top(y+1))
                elif self.get(x-1, y+1) == '+':
                    p2 = Point(self.hcenter(x-1), self.vcenter(y+1))
                elif self.get(x-1, y+1) == '-':
                    p2 = Point(self.left(x), self.vcenter(y+1))
                elif self.get(x-1, y+1) == '/':
                    p2 = Point(self.left(x), self.bottom(y))
                elif self.get(x-1, y) == '|':
                    p2 = Point(self.hcenter(x-1), self.bottom(y))
                elif self.get(x, y+1) == '-':
                    p2 = Point(self.left(x), self.vcenter(y+1))
                
                if p1 or p2:
                    if not p1:
                        p1 = Point(self.right(x), self.top(y))
                    if not p2:
                        p2 = Point(self.left(x), self.bottom(y))
                    result.append(Line(p1, p2))
        else: # '\'
            # rounded rectangles
            if (self.get(x-1, y) == '-' and self.get(x, y+1) == '|'):
                result.append(Line(
                    Point(self.hcenter(x), self.bottom(y)),
                    Point(self.left(x), self.vcenter(y))
                ))
            if (self.get(x+1, y) == '-' and self.get(x, y-1) == '|'):
                result.append(Line(
                    Point(self.hcenter(x), self.top(y)),
                    Point(self.right(x), self.vcenter(y))
                ))
            if not result:
                # if used as diagonal line
                p1 = p2 = None
                if self.get(x-1, y-1) == '|':
                    p1 = Point(self.hcenter(x-1), self.top(y))
                elif self.get(x-1, y-1) == '+':
                    p1 = Point(self.hcenter(x-1), self.vcenter(y-1))
                elif self.get(x-1, y-1) == '-':
                    p1 = Point(self.left(x), self.vcenter(y-1))
                elif self.get(x-1, y-1) == '\\':
                    p1 = Point(self.left(x), self.top(y))
                elif self.get(x-1, y) == '|':
                    p1 = Point(self.hcenter(x-1), self.top(y))
                elif self.get(x, y-1) == '-':
                    p1 = Point(self.left(x), self.hcenter(y-1))
                    
                if self.get(x+1, y+1) == '|':
                    p2 = Point(self.hcenter(x+1), self.top(y+1))
                elif self.get(x+1, y+1) == '+':
                    p2 = Point(self.hcenter(x+1), self.vcenter(y+1))
                elif self.get(x+1, y+1) == '-':
                    p2 = Point(self.right(x), self.vcenter(y+1))
                elif self.get(x+1, y+1) == '\\':
                    p2 = Point(self.right(x), self.bottom(y))
                elif self.get(x+1, y) == '|':
                    p2 = Point(self.hcenter(x+1), self.bottom(y))
                elif self.get(x, y+1) == '-':
                    p2 = Point(self.right(x), self.vcenter(y+1))
                
                if p1 or p2:
                    if not p1:
                        p1 = Point(self.left(x), self.top(y))
                    if not p2:
                        p2 = Point(self.right(x), self.bottom(y))
                    result.append(Line(p1, p2))
        if result:
            self.tag([(x,y)], CLASS_JOIN)
        return result

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

