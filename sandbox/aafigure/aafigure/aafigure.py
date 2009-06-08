"""\
ASCII art to image converter.

This is the main module that contains the parser.

See svg.py and aa.py for output modules, that can render the parsed structure.

(C) 2006 Chris Liechti <cliechti@gmx.net>
"""

NOMINAL_SIZE = 2

CLASS_LINE = 'line'
CLASS_STRING = 'str'
CLASS_RECTANGLE = 'rect'
CLASS_JOIN = 'join'
CLASS_FIXED = 'fixed'

DEFAULT_OPTIONS = dict(
    background   = '#ffffff',
    foreground   = '#000000',
    line_width   = 2.0,
    scale        = 1.0,
    aspect       = 1.0,
    format       = 'svg',
    debug        = False,
    textual      = False,
    proportional = False,
)

# - - - - - - - - - - - - - - Shapes - - - - - - - - - - - - - - -
def point(object):
    """return a Point instance.
       - if object is already a Point instance it's returned as is
       - complex numbers are converted to Points
       - a tuple with two elements (x,y)
    """
    if isinstance(object, Point):
        return object
    #~ print type(object), object.__class__
    if type(object) is complex:
        return Point(object.real, object.imag)
    if type(object) is tuple and len(object) == 2:
        return Point(object[0], object[1])
    raise ValueError('can not convert %r to a Point')


def group(list_of_shapes):
    """return a group if the number of shapes is greater than one"""
    if len(list_of_shapes) > 1:
        return [Group(list_of_shapes)]
    else:
        return list_of_shapes


class Point:
    """A single point. This class primary use is to represent coordinates
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
        self.start = point(start)
        self.end = point(end)

    def __repr__(self):
        return 'Line(%r, %r)' % (self.start, self.end)


class Rectangle:
    """Rectangle with two edge coordinates."""
    def __init__(self, p1, p2):
        self.p1 = point(p1)
        self.p2 = point(p2)
    def __repr__(self):
        return 'Rectangle(%r, %r)' % (self.p1, self.p2)


class Circle:
    """Circle with center coordinates and radius."""
    def __init__(self, center, radius):
        self.center = point(center)
        self.radius = radius

    def __repr__(self):
        return 'Circle(%r, %r)' % (self.center, self.radius)


class Label:
    """A text label at a position"""
    def __init__(self, position, text):
        self.position = position
        self.text = text
    def __repr__(self):
        return 'Label(%r, %r)' % (self.position, self.text)


class Group:
    """A group of shapes"""
    def __init__(self, shapes=None):
        if shapes is None: shapes = []
        self.shapes = shapes
    def __repr__(self):
        return 'Group(%r)' % (self.shapes,)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class AsciiArtImage:
    """This class hold a ASCII art figure and has methods to parse it.
       The resulting list of shapes is also stored here.
    """
    QUOTATION_CHARACTERS = list('"\'`')

    def __init__(self, text, aspect_ratio=1, textual=False):
        """Take a ASCII art figure and store it, prepare for ``recognize``"""
        self.aspect_ratio = float(aspect_ratio)
        self.textual = textual
        # XXX TODO tab expansion
        # detect size of input image
        self.image = []
        max_x = 0
        for y, line in enumerate(text.splitlines()):
            max_x = max(max_x, len(line))
            self.image.append(line)
        self.width = max_x
        self.height = y+1
        # make sure it's rectangular
        for y, line in enumerate(self.image):
            if len(line) < max_x:
                self.image[y] = line + ' '*(max_x-len(line))
        # initialize other data structures
        self.classification = [[None]*self.width for y in range(self.height)]
        self.shapes = []
        self.nominal_size = NOMINAL_SIZE

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

    def cls(self, x, y):
        """get tag at coordinate"""
        try:
            return self.classification[y][x]
        except IndexError:
            return 'outside'

    # Coordinate conversion and shifting
    def left(self, x):          return x*NOMINAL_SIZE*self.aspect_ratio
    def hcenter(self, x):       return (x+0.5)*NOMINAL_SIZE*self.aspect_ratio
    def right(self, x):         return (x+1)*NOMINAL_SIZE*self.aspect_ratio
    def top(self, y):           return y*NOMINAL_SIZE
    def vcenter(self, y):       return (y+0.5)*NOMINAL_SIZE
    def bottom(self, y):        return (y+1)*NOMINAL_SIZE

    def recognize(self):
        """Try to convert ASCII art to vector graphics."""
        # XXX search for symbols
        #~ #search for long strings
        #~ for y in range(self.height):
            #~ for x in range(self.width):
                #~ character = self.image[y][x]
                #~ if self.classification[y][x] is None:
                    #~ if character.isalnum():
                        #~ self.shapes.extend(self._follow_horizontal_string(x, y))
        # search for quoted texts
        for y in range(self.height):
            for x in range(self.width):
                #if not yet classified, check for a line
                character = self.image[y][x]
                if character in self.QUOTATION_CHARACTERS:
                    self.shapes.extend(self._follow_horizontal_string(x, y, quoted=True))

        # search for standard shapes
        for y in range(self.height):
            for x in range(self.width):
                #if not yet classified, check for a line
                character = self.image[y][x]
                if self.classification[y][x] is None:
                    if character == '-':
                        self.shapes.extend(self._follow_horizontal_line(x, y))
                    elif character == '|':
                        self.shapes.extend(self._follow_vertical_line(x, y))
                    elif character == '_':
                        self.shapes.extend(self._follow_lower_horizontal_line(x, y))
                    elif character == '~':
                        self.shapes.extend(self._follow_upper_horizontal_line(x, y))
                    elif character == '=':
                        self.shapes.extend(self._follow_horizontal_line(x, y, thick=True))
                    elif character in '\\/':
                        self.shapes.extend(self._follow_rounded_edge(x, y))
                    if character == '+':
                        self.shapes.extend(self._plus_joiner(x, y))
                    #~ if character in self.FILL_CHARACTERS \
                        #~ and ((self.get(x+1,y) == character and self.get(x+2,y) == character) \
                             #~ or self.get(x,y+1) == character):
                    elif character in self.FIXED_CHARACTERS:
                        self.shapes.extend(self.get_fixed_character(character)(x, y))
                        self.tag([(x,y)], CLASS_FIXED)
                    elif character in self.FILL_CHARACTERS:
                        if self.textual:
                            if self.get(x,y+1) == character:
                                self.shapes.extend(self._follow_fill(character, x, y))
                        else:
                            if (self.get(x+1,y) == character or self.get(x,y+1) == character):
                                self.shapes.extend(self._follow_fill(character, x, y))

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

    # - - - - - - - - - arrows - - - - - - - - -
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
        return p1-direction_vector*2, [
            Line(p1-direction_vector*2.0, p1+direction_vector*(-0.5+0.5j)),
            Line(p1-direction_vector*2.0, p1+direction_vector*(-0.5-0.5j))
        ]

    def _circle_head(self, p1, p2, radius=0.5):
        """--o"""
        direction_vector = p1 - p2
        direction_vector /= abs(direction_vector)
        return p1-direction_vector, [Circle(p1-direction_vector, radius)]

    def _large_circle_head(self, p1, p2):
        """--O"""
        return self._circle_head(p1, p2, radius=0.9)

    def _rectangular_head(self, p1, p2):
        """--#"""
        direction_vector = p1 - p2
        direction_vector /= abs(direction_vector)
        #~ return p1-direction_vector*1.414, [
            #~ Rectangle(p1-direction_vector-direction_vector*(0.707+0.707j),
                      #~ p1-direction_vector+direction_vector*(0.707+0.707j))
        #~ ]
        return p1-direction_vector*1.707, [
            Line(p1-direction_vector-direction_vector*(0.707+0.707j),
                 p1-direction_vector-direction_vector*(0.707-0.707j)),
            Line(p1-direction_vector+direction_vector*(0.707+0.707j),
                 p1-direction_vector+direction_vector*(0.707-0.707j)),
            Line(p1-direction_vector-direction_vector*(0.707+0.707j),
                 p1-direction_vector+direction_vector*(0.707-0.707j)),
            Line(p1-direction_vector-direction_vector*(0.707-0.707j),
                 p1-direction_vector+direction_vector*(0.707+0.707j)),
        ]

    # the same character can mean a different thing, depending from where the
    # line is coming. this table maps line direction (dx,dy) and the arrow
    # character to a arrow drawing function
    ARROW_TYPES = [
        #chr  dx  dy  arrow type
        ('>',  1,  0, '_standard_arrow'),
        ('<', -1,  0, '_standard_arrow'),
        ('^',  0, -1, '_standard_arrow'),
        ('A',  0, -1, '_standard_arrow'),
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
    ARROW_HEADS = list('<>AVv^oO#')

    def get_arrow(self, character, dx, dy):
        """return arrow drawing function or None"""
        for head, ddx, ddy, function_name in self.ARROW_TYPES:
            if character == head and dx == ddx and dy == ddy:
                return getattr(self, function_name)

    # - - - - - - - - - fills - - - - - - - - -

    def _hatch_left(self, x, y): return self._n_hatch_diagonal(x,y,1,True)
    def _hatch_right(self, x, y): return self._n_hatch_diagonal(x,y,1,False)
    def _cross_hatch(self, x, y): return self._n_hatch_diagonal(x,y,1,True) + self._n_hatch_diagonal(x,y,1,False)
    def _double_hatch_left(self, x, y): return self._n_hatch_diagonal(x,y,2,True)
    def _double_hatch_right(self, x, y): return self._n_hatch_diagonal(x,y,2,False)
    def _double_cross_hatch(self, x, y): return self._n_hatch_diagonal(x,y,2,True) + self._n_hatch_diagonal(x,y,2,False)
    def _triple_hatch_left(self, x, y): return self._n_hatch_diagonal(x,y,3,True)
    def _triple_hatch_right(self, x, y): return self._n_hatch_diagonal(x,y,3,False)
    def _triple_cross_hatch(self, x, y): return self._n_hatch_diagonal(x,y,3,True) + self._n_hatch_diagonal(x,y,3,False)

    def _n_hatch_diagonal(self, x, y, n, left=False):
        """hatch generator function"""
        d = 1/float(n)
        result = []
        if left:
            for i in range(n):
                result.append(Line(
                    Point(self.left(x), self.top(y+d*i)),
                    Point(self.right(x-d*i), self.bottom(y))
                ))
                if n:
                    result.append(Line(
                        Point(self.right(x-d*i), self.top(y)),
                        Point(self.right(x), self.top(y+d*i))
                    ))
        else:
            for i in range(n):
                result.append(Line(Point(self.left(x), self.top(y+d*i)), Point(self.left(x+d*i), self.top(y))))
                if n:
                    result.append(Line(Point(self.left(x+d*i), self.bottom(y)), Point(self.right(x), self.top(y+d*i))))
        return result

    def _hatch_v(self, x, y): return self._n_hatch_straight(x,y,1,True)
    def _hatch_h(self, x, y): return self._n_hatch_straight(x,y,1,False)
    def _hv_hatch(self, x, y): return self._n_hatch_straight(x,y,1,True) + self._n_hatch_straight(x,y,1,False)
    def _double_hatch_v(self, x, y): return self._n_hatch_straight(x,y,2,True)
    def _double_hatch_h(self, x, y): return self._n_hatch_straight(x,y,2,False)
    def _double_hv_hatch(self, x, y): return self._n_hatch_straight(x,y,2,True) + self._n_hatch_straight(x,y,2,False)
    def _triple_hatch_v(self, x, y): return self._n_hatch_straight(x,y,3,True)
    def _triple_hatch_h(self, x, y): return self._n_hatch_straight(x,y,3,False)
    def _triple_hv_hatch(self, x, y): return self._n_hatch_straight(x,y,3,True) + self._n_hatch_straight(x,y,3,False)

    def _n_hatch_straight(self, x, y, n, vertical=False):
        """hatch generator function"""
        d = 1/float(n)
        offset = 1.0/(n+1)
        result = []
        if vertical:
            for i in range(n):
                i = i + offset
                result.append(Line(
                    Point(self.left(x+d*i), self.top(y)),
                    Point(self.left(x+d*i), self.bottom(y))
                ))
                #~ if n:
                    #~ result.append(Line(Point(self.right(x-d*i), self.top(y)), Point(self.right(x), self.top(y+d*i))))
        else:
            for i in range(n):
                i = i + offset
                result.append(Line(
                    Point(self.left(x), self.top(y+d*i)),
                    Point(self.right(x), self.top(y+d*i))
                ))
                #~ if n:
                    #~ result.append(Line(Point(self.left(x+d*i), self.bottom(y)), Point(self.right(x), self.top(y+d*i))))
        return result

    def _fill_trail(self, x, y):
        return [
            Line(
                Point(self.left(x+0.707), self.top(y)),
                Point(self.right(x), self.bottom(y-0.707))
            ),
            Line(
                Point(self.left(x), self.top(y+0.707)),
                Point(self.right(x-0.707), self.bottom(y))
            )
        ]

    def _fill_foreground(self, x, y):
        return [
            Rectangle(
                Point(self.left(x), self.top(y)),
                Point(self.right(x), self.bottom(y))
            )
        ]

    def _fill_background(self, x, y): return []

    def _fill_small_circle(self, x, y):
        return [
            Circle(Point(self.left(x+0.5), self.top(y+0.5)), 0.2)
        ]
    def _fill_medium_circle(self, x, y):
        return [
            Circle(Point(self.left(x+0.5), self.top(y+0.5)), 0.4)
        ]

    def _fill_large_circle(self, x, y):
        return [
            Circle(Point(self.left(x+0.5), self.top(y+0.5)), 0.9)
        ]

    def _fill_qmark(self, x, y):
        return [
            Label(Point(self.left(x), self.bottom(y)), '?')
        ]

    def _fill_triangles(self, x, y):
        return [
            Line(Point(self.left(x+0.5), self.top(y+0.2)), Point(self.left(x+0.75), self.top(y+0.807))),
            Line(Point(self.left(x+0.7), self.top(y+0.807)), Point(self.left(x+0.25), self.top(y+0.807))),
            Line(Point(self.left(x+0.25), self.top(y+0.807)), Point(self.left(x+0.5), self.top(y+0.2))),
        ]

    FILL_TYPES = [
        ('A', '_hatch_left'),
        ('B', '_hatch_right'),
        ('C', '_cross_hatch'),
        ('D', '_double_hatch_left'),
        ('E', '_double_hatch_right'),
        ('F', '_double_cross_hatch'),
        ('G', '_triple_hatch_left'),
        ('H', '_triple_hatch_right'),
        ('I', '_triple_cross_hatch'),
        ('J', '_hatch_v'),
        ('K', '_hatch_h'),
        ('L', '_hv_hatch'),
        ('M', '_double_hatch_v'),
        ('N', '_double_hatch_h'),
        ('O', '_double_hv_hatch'),
        ('P', '_triple_hatch_v'),
        ('Q', '_triple_hatch_h'),
        ('R', '_triple_hv_hatch'),
        ('S', '_fill_qmark'),
        ('T', '_fill_trail'),
        ('U', '_fill_small_circle'),
        ('V', '_fill_medium_circle'),
        ('W', '_fill_large_circle'),
        ('X', '_fill_foreground'),
        ('Y', '_fill_triangles'),
        ('Z', '_fill_background'),
    ]
    FILL_CHARACTERS = ''.join([t+t.lower() for (t,f) in FILL_TYPES])

    def get_fill(self, character):
        """return fill function"""
        for head, function_name in self.FILL_TYPES:
            if character == head:
                return getattr(self, function_name)
        raise ValueError('no such fill type')

    # - - - - - - - - - fixed characters and their shapes - - - - - - - - -

    def _open_triangle_left(self, x, y):
        return [
            Line(
                Point(self.left(x), self.vcenter(y)),
                Point(self.right(x), self.top(y))
            ),
            Line(
                Point(self.left(x), self.vcenter(y)),
                Point(self.right(x), self.bottom(y))
            )
        ]
    def _open_triangle_right(self, x, y):
        return [
            Line(
                Point(self.right(x), self.vcenter(y)),
                Point(self.left(x), self.top(y))
            ),
            Line(
                Point(self.right(x), self.vcenter(y)),
                Point(self.left(x), self.bottom(y))
            )
        ]

    def _circle(self, x, y):
        return [
            Circle(Point(self.hcenter(x), self.vcenter(y)), NOMINAL_SIZE/2.0)
        ]


    FIXED_TYPES = [
        ('{', '_open_triangle_left'),
        ('}', '_open_triangle_right'),
        ('*', '_circle'),
    ]
    FIXED_CHARACTERS = ''.join([t for (t,f) in FIXED_TYPES])

    def get_fixed_character(self, character):
        """return fill function"""
        for head, function_name in self.FIXED_TYPES:
            if character == head:
                return getattr(self, function_name)
        raise ValueError('no such character')

    # - - - - - - - - - helper function for shape recognition - - - - - - - - -

    def _follow_vertical_line(self, x, y):
        """find a vertical line with optional arrow heads"""
        # follow line to the bottom
        _, end_y, line_end_style = self._follow_line(x, y, dy=1, line_character='|')
        # follow line to the top
        _, start_y, line_start_style = self._follow_line(x, y, dy=-1, line_character='|')
        # if a '+' follows a line, then the line is stretched to hit the '+' center
        start_y_fix = end_y_fix = 0
        if self.get(x, start_y-1) == '+':
            start_y_fix = -0.5
        if self.get(x, end_y+1) == '+':
            end_y_fix = 0.5
        # tag characters as used (not the arrow heads)
        self.tag([(x, y) for y in range(start_y, end_y+1)], CLASS_LINE)
        # return the new shape object with arrows etc.
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
        return group(shapes)

    def _follow_horizontal_line(self, x, y, thick=False):
        """find a horizontal line with optional arrow heads"""
        if thick:
            line_character = '='
        else:
            line_character = '-'
        # follow line to the right
        end_x, _, line_end_style = self._follow_line(x, y, dx=1, line_character=line_character)
        # follow line to the left
        start_x, _, line_start_style = self._follow_line(x, y, dx=-1, line_character=line_character)
        start_x_fix = end_x_fix = 0
        if self.get(start_x-1, y) == '+':
            start_x_fix = -0.5
        if self.get(end_x+1, y) == '+':
            end_x_fix = 0.5
        self.tag([(x, y) for x in range(start_x, end_x+1)], CLASS_LINE)
        # return the new shape object with arrows etc.
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
        return group(shapes)

    def _follow_lower_horizontal_line(self, x, y):
        """find a horizontal line, the line is aligned to the bottom and a bit
           wider, so that it can be used for shapes like this:
              ___
           __|   |___
        """
        # follow line to the right
        end_x, _, line_end_style = self._follow_line(x, y, dx=1, line_character='_', arrows=False)
        # follow line to the left
        start_x, _, line_start_style = self._follow_line(x, y, dx=-1, line_character='_', arrows=False)
        self.tag([(x, y) for x in range(start_x, end_x+1)], CLASS_LINE)
        # return the new shape object with arrows etc.
        p1 = complex(self.hcenter(start_x-1), self.bottom(y))
        p2 = complex(self.hcenter(end_x+1), self.bottom(y))
        return [Line(p1, p2)]

    def _follow_upper_horizontal_line(self, x, y):
        """find a horizontal line, the line is aligned to the bottom and a bit
           wider, so that it can be used for shapes like this:

             |~~~|
           ~~     ~~~
        """
        # follow line to the right
        end_x, _, line_end_style = self._follow_line(x, y, dx=1, line_character='~', arrows=False)
        # follow line to the left
        start_x, _, line_start_style = self._follow_line(x, y, dx=-1, line_character='~', arrows=False)
        self.tag([(x, y) for x in range(start_x, end_x+1)], CLASS_LINE)
        # return the new shape object with arrows etc.
        p1 = complex(self.hcenter(start_x-1), self.top(y))
        p2 = complex(self.hcenter(end_x+1), self.top(y))
        return [Line(p1, p2)]

    def _follow_line(self, x, y, dx=0, dy=0, line_character=None, arrows=True):
        """helper function for all the line functions"""
        # follow line in the given direction
        while 0 <= x < self.width and 0<= y < self.height and self.get(x+dx, y+dy) == line_character:
            x += dx
            y += dy
        if arrows:
            # check for arrow head
            following_character = self.get(x+dx, y+dy)
            if following_character in self.ARROW_HEADS:
                line_end_style = self.get_arrow(following_character, dx, dy)
                if line_end_style:
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
        # looking right and down is sufficient as the scan is done from left to
        # right, top to bottom
        for dx, dy in ((1,0), (0,1)):
            if self.get(x+dx, y+dy) == '+':
                result.append(Line(
                    Point(self.hcenter(x), self.vcenter(y)),
                    Point(self.hcenter(x+dx), self.vcenter(y+dy))
                ))
        self.tag([(x,y)], CLASS_JOIN)
        return result


    def _follow_fill(self, character, start_x, start_y):
        """fill shapes like the ones below with a pattern. when the character is
           upper case, draw a border too.

            XXX  aaa  BB
           XXX    a
        """
        fill = self.get_fill(character.upper())
        border = character.isupper()
        result = []
        # flood fill algorithm, searching for similar characters
        coordinates = []
        to_scan = [(start_x, start_y)]
        while to_scan:
            x,y = to_scan.pop()
            if self.cls(x, y) is None:
                if self.get(x, y) == character:
                    result.extend(fill(x, y))
                    self.tag([(x,y)], CLASS_RECTANGLE)
                if self.get(x+1, y) == character:
                    if self.cls(x+1, y) is None: to_scan.append((x+1, y))
                elif border:
                    result.append(Line(Point(self.right(x), self.top(y)), Point(self.right(x), self.bottom(y))))
                if self.get(x-1, y) == character:
                    if self.cls(x-1, y) is None: to_scan.append((x-1, y))
                elif border:
                    result.append(Line(Point(self.left(x), self.top(y)), Point(self.left(x), self.bottom(y))))
                if self.get(x, y+1) == character:
                    if self.cls(x, y+1) is None: to_scan.append((x, y+1))
                elif border:
                    result.append(Line(Point(self.left(x), self.bottom(y)), Point(self.right(x), self.bottom(y))))
                if self.get(x, y-1) == character:
                    if self.cls(x, y-1) is None: to_scan.append((x, y-1))
                elif border:
                    result.append(Line(Point(self.left(x), self.top(y)), Point(self.right(x), self.top(y))))
        return group(result)
            #~ for x in range(start_x, x+1):
                #~ result.extend(fill(x,i))
        #~ # [Rectangle(
            #~ # Point(self.left(start_x), self.top(start_y)),
            #~ # Point(self.right(x), self.bottom(y)),
        #~ # )]
        #~ for i in range(start_y, y+1):
            #~ self.tag([(x, i) for x in range(start_x, x+1)], CLASS_RECTANGLE)
            #~ for x in range(start_x, x+1):
                #~ result.extend(fill(x,i))
        #~ return result

    def _follow_horizontal_string(self, start_x, y, accept_anything=False, quoted=False):
        """find a string. may contain single spaces, but the detection is
           aborted after more than one space.

              Text one   "Text two"

           accept_anything means that all non space characters are interpreted
           as text.
        """
        # follow line from left to right
        if quoted:
            quotation_character = self.get(start_x, y)
            x = start_x+1
        else:
            quotation_character = None
            x = start_x
        text = []
        if self.get(x, y) != ' ':
            text.append(self.get(x, y))
            self.tag([(x, y)], CLASS_STRING)
            is_first_space = True
            while 0 <= x+1 < self.width and self.cls(x+1, y) is None:
                if not quoted:
                    if self.get(x+1, y) == ' ' and not is_first_space:
                        break
                    if not accept_anything and not self.get(x+1, y).isalnum():
                        break
                x += 1
                character = self.get(x, y)
                if character == quotation_character:
                    break
                text.append(character)
                if character == ' ':
                    is_first_space = False
                else:
                    is_first_space = True
            if text[-1] == ' ':
                del text[-1]
                x -= 1
            self.tag([(x, y) for x in range(start_x, x+1)], CLASS_STRING)
            return [Label(
                Point(self.left(start_x), self.bottom(y)),
                ''.join(text)
            )]
        else:
            return []

    def _follow_rounded_edge(self, x, y):
        """check for rounded edges:
            /-    |     -\-    |   and also \    /  etc.
            |    -/      |     \-            -  |
        """
        result = []
        if self.get(x, y) == '/':
            # rounded rectangles
            # XXX return Arc shapes, not Lines
            if (self.get(x+1, y) == '-' and self.get(x, y+1) == '|'):
                result.append(Line(
                    Point(self.hcenter(x), self.bottom(y)),
                    Point(self.right(x), self.vcenter(y))
                ))
            if self.get(x-1, y) == '-' and self.get(x, y-1) == '|':
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
            if self.get(x-1, y) == '-' and self.get(x, y+1) == '|':
                result.append(Line(
                    Point(self.hcenter(x), self.bottom(y)),
                    Point(self.left(x), self.vcenter(y))
                ))
            if self.get(x+1, y) == '-' and self.get(x, y-1) == '|':
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
        return group(result)

class UnsupportedFormatError(Exception):
    pass

def render(input, output=None, options=None):
    """Render an ASCII art figure.

    If ``input`` is a basestring subclass (str or unicode), the text contained
    in ``input`` is rendered. If ``input is a file-like object, the text to
    render is taken using ``input.read()``. If no ``output`` is specified, the
    resulting rendered image is returned as a string. If output is a basestring
    subclass, a file with the name of ``output`` contents is created and the
    rendered image is saved there. If ``output`` is a file-like object,
    ``output.write()`` is used to save the rendered image.

    This function returns a tuple ``(visitor, output)``, where ``visitor`` is
    visitor instance that rendered the image and ``output`` is the image as
    requested by the ``output`` parameter (a ``str`` if it was ``None``, or
    a file-like object otherwise, which you should ``close()`` if needed).

    ``options`` are... optional. You can provide a dictionary with options.
    Valid keys (and their defaults) are:

    background <str>:
        background color in the form ``#rgb`` or ``#rrggbb``, *not* for SVG
        output (default: ``#000000``).

    foreground <str>:
        foreground color in the form ``#rgb`` or ``#rrggbb`` (default:
        ``#ffffff``).

    fill <str>:
        fill color in the form ``#rgb`` or ``#rrggbb`` (default: same as
        ``foreground`` color).

    line_width <float>:
        change line with, SVG only currently (default: 2.0).

    scale <float>:
        enlarge or shrink image (default: 1.0).

    aspect <float>:
        change aspect ratio. Effectively it is the width of the image that is
        multiplied by this factor. The default setting ``1`` is useful when
        shapes must have the same look when drawn horizontally or vertically.
        However, 0.5 looks more like the original ASCII and even smaller
        factors may be useful for timing diagrams and such. But there is a risk
        that text is cropped or is draw over an object beside it.

        The stretching is done before drawing arrows or circles, so that they
        are still good looking (default: 1.0).

    format <str>:
        choose backend/output format: 'svg', 'pdf', 'png' and all bitmap
        formats that PIL supports can be used but only few make sense. Line
        drawings have a good compression and better quality when saved as PNG
        rather than a JPEG. The best quality will be achieved with SVG, tough
        not all browsers support this vector image format at this time
        (default: 'svg').

    debug <bool>:
        for now, it only prints the original ASCII art figure text (default:
        False).

    textual <bool>:
        disables horizontal fill detection. Fills are only detected when they
        are vertically at least 2 characters high (default: False).

    proportional <bool>:
        use a proportional font. Proportional fonts are general better looking
        than monospace fonts but they can mess the figure if you need them to
        look as similar as possible to the ASCII art (default: False).

    This function can raise an UnsupportedFormatError exception if the
    specified format is not supported.
    """

    def decode_color(color_string):
        if color_string[0] == '#':          # HTML like color syntax
            if len(color_string) == 4:      # #rgb format
                r,g,b = [int(c+c, 16) for c in color_string[1:]]
            elif len(color_string) == 7:      # #rrggbb format
                r,g,b = [int(color_string[n:n+2], 16)
                                for n in range(1, len(color_string), 2)]
            else:
                raise ValueError('not a valid color: %r' % color_string)
        # XXX add a list of named colors
        return r,g,b

    def merge(dst, src):
        for (k, v) in src.items():
            if k not in dst:
                dst[k] = v

    if options is None:
        options = dict()

    merge(options, DEFAULT_OPTIONS)
    if 'fill' not in options or options['fill'] is None:
        options['fill'] = options['foreground']

    if hasattr(input, 'read'):
        input = input.read()

    aaimg = AsciiArtImage(input, options['aspect'], options['textual'])
    if options['debug']:
        sys.stderr.write(str(aaimg) + '\n')
    aaimg.recognize()

    close_output = False
    if output is None:
        import StringIO
        output = StringIO.StringIO()
    if isinstance(output, basestring):
        output = file(output, 'wb')
        close_output = True

    if options['format'].lower() == 'svg':
        import svg
        visitor = svg.SVGOutputVisitor(
            output,
            scale = options['scale']*7,
            line_width = options['line_width'],
            foreground = decode_color(options['foreground']),
            background = decode_color(options['background']),
            fillcolor = decode_color(options['fill']),
            proportional = options['proportional'],
            #~ debug = options['debug'],
        )
        visitor.visit_image(aaimg)
    elif options['format'].lower() == 'pdf':
        try:
            import pdf
        except ImportError:
            if close_output:
                output.close()
            raise UnsupportedFormatError('install reportlab to get PDF support')
        visitor = pdf.PDFOutputVisitor(
            output,
            scale = options['scale'],
            line_width = options['line_width'],
            foreground = decode_color(options['foreground']),
            background = decode_color(options['background']),
            fillcolor = decode_color(options['fill']),
            proportional = options['proportional'],
            #~ debug = options['debug'],
        )
        visitor.visit_image(aaimg)
    elif options['format'].lower() == 'ascii':
        import aa
        visitor = aa.AsciiOutputVisitor(
            scale = options['scale'],
        )
        visitor.visit_image(aaimg)
        output.write(str(visitor))
    else:
        try:
            import pil
        except ImportError:
            if close_output:
                output.close()
            raise UnsupportedFormatError('install PIL to get bitmap formats '
                    'support')
        visitor = pil.PILOutputVisitor(
            output,
            scale = options['scale']*7,
            line_width = options['line_width'],
            foreground = decode_color(options['foreground']),
            background = decode_color(options['background']),
            fillcolor = decode_color(options['fill']),
            proportional = options['proportional'],
            file_type = options['format'],
            #~ debug = options['debug'],
        )
        try:
            visitor.visit_image(aaimg)
        except KeyError:
            if close_output:
                output.close()
            raise UnsupportedFormatError("PIL doesn't support image format %r" %
                    options['format'])

    return (visitor, output)


def main():
    """implent an useful main for use as command line program"""
    import sys
    import optparse

    parser = optparse.OptionParser(
        usage = "%prog [options] [file]"
    )

    parser.add_option("-o", "--output",
        dest = "output",
        metavar = "FILE",
        help = "write output to FILE"
    )

    parser.add_option("-t", "--type",
        dest = "type",
        help = "filetype: png, jpg, svg",
        default = DEFAULT_OPTIONS['format'],
    )

    parser.add_option("-D", "--debug",
        dest = "debug",
        action = "store_true",
        help = "enable debug outputs",
        default = DEFAULT_OPTIONS['debug'],
    )

    parser.add_option("-T", "--textual",
        dest = "textual",
        action = "store_true",
        help = "disable horizontal fill detection",
        default = DEFAULT_OPTIONS['textual'],
    )

    parser.add_option("-s", "--scale",
        dest = "scale",
        action = "store",
        type = 'float',
        help = "set scale",
        default = DEFAULT_OPTIONS['scale'],
    )

    parser.add_option("-a", "--aspect",
        dest = "aspect",
        action = "store",
        type = 'float',
        help = "set aspect ratio",
        default = DEFAULT_OPTIONS['aspect'],
    )

    parser.add_option("-l", "--linewidth",
        dest = "linewidth",
        action = "store",
        type = 'float',
        help = "set width, svg only",
        default = DEFAULT_OPTIONS['line_width'],
    )

    parser.add_option("--proportional",
        dest = "proportional",
        action = "store_true",
        help = "use proportional font instead of fixed width",
        default = DEFAULT_OPTIONS['proportional'],
    )

    parser.add_option("-f", "--foreground",
        dest = "foreground",
        action = "store",
        help = "foreground color default=%default",
        default = DEFAULT_OPTIONS['foreground'],
    )

    parser.add_option("-x", "--fill",
        dest = "fill",
        action = "store",
        help = "foreground color default=foreground",
        default = None,
    )

    parser.add_option("-b", "--background",
        dest = "background",
        action = "store",
        help = "foreground color default=%default",
        default = DEFAULT_OPTIONS['background'],
    )

    class Values:
        def as_dict(self):
            return dict(((k, getattr(self,k)) for k in dir(self)
                        if not k.startswith('__') and k != 'as_dict'))
    options = Values()
    (options, args) = parser.parse_args(values=options)

    if len(args) > 1:
        parser.error("too many arguments")

    if args:
        input = file(args[0])
    else:
        input = sys.stdin

    #~ def render(text, output, options):
        #~ """helper function for tests. scan the given image and create svg output"""
        #~ import pprint
        #~ aaimg = AsciiArtImage(text)
        #~ print text
        #~ aaimg.recognize()
        #~ aav = aa.AsciiOutputVisitor()
        #~ pprint.pprint(aaimg.shapes)
        #~ aav.visit(aaimg)
        #~ print aav
        #~ svgout = svg.SVGOutputVisitor(
            #~ file('aafigure_%x.svg' % (long(hash(text)) & 0xffffffffL,), 'w'),
            #~ scale = 10
        #~ )
        #~ svgout.visit(aaimg)

    #~ aaimg = AsciiArtImage("""
        #~ ---> | ^|   |
        #~ <--- | || --+--
        #~ <--> | |V   |
     #~ __             __
    #~ |  |__  +---+  |__|
            #~ |box|   ..
            #~ +---+  Xenophon
    #~ """)
    #~ print aaimg
    #~ aaimg.recognize()
    #~ print "%r" % aaimg
    #~ aav = aa.AsciiOutputVisitor()
    #~ aav.visit(aaimg)
    #~ print aav

    if hasattr(options, 'output'):
        output = file(options.output, 'wb')
        delattr(options, 'output')
    else:
        output = sys.stdout

    (visitor, output) = render(input, output, options.as_dict())
    output.close()

# when module is run, run the command line tool
if __name__ == '__main__':
    main()
