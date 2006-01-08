"""
SVG renderer.

(C) 2006 Chris Liechti <cliechti@gmx.net>
"""

class SVGOutputVisitor:
    """Render a list of shapes as ASCII art.
    """
    
    def __init__(self, file_like, scale = 1, unit='px', debug=False):
        self.file_like = file_like
        self.scale = scale
        self.unit = unit
        self.debug = debug
    
    def _num(self, number):
        return "%d%s" % (int(number*self.scale), self.unit)
    
    def get_size_attrs(self):
        return 'width="%s" height="%s"' % (
            self._num(self.aa_image.width*self.aa_image.nominal_size),
            self._num(self.aa_image.height*self.aa_image.nominal_size)
        )
    
    def visit(self, aa_image):
        self.aa_image = aa_image        #save fore later XXX not optimal to do it here
        self.file_like.write("""\
<?xml version="1.0" standalone="no" ?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN"
  "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
<svg width="%s" height="%s" xmlns="http://www.w3.org/2000/svg"
  xmlns:xlink="http://www.w3.org/1999/xlink">
""" % (
            self._num(aa_image.width*aa_image.nominal_size),
            self._num(aa_image.height*aa_image.nominal_size)
        ))
  
        #~ if self.debug:
            #~ #draw a rectangle around entire image
            #~ self._rectangle(
                #~ 0,0,
                #~ aa_image.width, aa_image.height,
                #~ style = 'fill:none;',
            #~ )
        
        for shape in aa_image.shapes:
            shape_name = shape.__class__.__name__.lower()
            visitor_name = 'visit_%s' % shape_name
            if hasattr(self, visitor_name):
                getattr(self, visitor_name)(shape)
            else:
                print "don't know how to handle shape %r" % shape
            
        self.file_like.write("""</svg>""")

    def visit_point(self, point):
        self.file_like.write("""\
<cirlce cx="%s" cy="%s" r="%s" 
    style="fill:black; stroke:black; stroke-width:2px;" />
""" % (self._num(point.x), self._num(point.y), self._num(2)))
    
    def _line_c(self, p1, p2):
        self._line(p1.real, p1.imag, p2.real, p2.imag)
        
    def _line(self, x1, y1, x2, y2):
        self.file_like.write("""\
<line x1="%s" y1="%s" x2="%s" y2="%s"
    style="stroke:black; stroke-width:2px;" />
""" % (self._num(x1), self._num(y1), self._num(x2), self._num(y2)))

    def visit_line(self, line):
        self.file_like.write("<!-- line -->")
        x1, x2 = line.start.x, line.end.x
        y1, y2 = line.start.y, line.end.y
        self._line(x1, y1, x2, y2)
        #arrows
        p1 = complex(line.start.x,line.start.y)
        p2 = complex(line.end.x,line.end.y)
        if line.start_style:
            self.file_like.write("<!-- start arrow -->")
            directon_vector = p1 - p2
            directon_vector /= abs(directon_vector)
            self._line_c(p1, p1-directon_vector+directon_vector*0.5j)
            self._line_c(p1, p1-directon_vector+directon_vector*-0.5j)
        if line.end_style:
            self.file_like.write("<!-- end arrow -->")
            directon_vector = p2 - p1
            directon_vector /= abs(directon_vector)
            self._line_c(p2, p2-directon_vector+directon_vector*0.5j)
            self._line_c(p2, p2-directon_vector+directon_vector*-0.5j)

    def _rectangle(self, x1, y1, x2, y2, style=''):
        self.file_like.write("""\
<rect x="%s" y="%s"
  width="%s"
  height="%s"
  style="stroke:black; stroke-width:2px; %s"
/>
""" % (
            self._num(x1), self._num(y1),
            self._num(x2-x1), self._num(y2-y1),
            style
        ))
    
    def visit_rectangle(self, rectangle):
        self._rectangle(
            rectangle.p1.x, rectangle.p1.y,
            rectangle.p2.x, rectangle.p2.y
        )
        
    def visit_label(self, label):
        #  font-weight="bold"

        self.file_like.write("""\
<text x="%s" y="%s"
  font-family="Verdana,sans-serif"
  font-size="%s"
>
  %s
</text>
""" % (
        self._num(label.position.x), self._num(label.position.y),
        self._num(2),
        label.text
        ))

