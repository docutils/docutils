"""
Bitmap renderer with the help of the Python Imaging Library.

(C) 2006 Chris Liechti <cliechti@gmx.net>
"""

import Image, ImageDraw, ImageFont

class PILOutputVisitor:
    """Render a list of shapes as ASCII art.
    """
    
    def __init__(self, file_like, scale = 1, line_width=1, debug=False, file_type='png',
                 foreground=(0,0,0), background=(255,255,255), fillcolor=(0,0,0),
    ):
        self.file_like = file_like
        self.scale = scale
        self.debug = debug
        self.line_width = line_width
        self.file_type = file_type
        self.foreground = foreground
        self.background = background
        self.fillcolor = fillcolor
    
    def visit(self, aa_image):
        """Process the gived ASCIIArtFigure and output the shapes in
           the SVG file
        """
        self.aa_image = aa_image        #save for later XXX not optimal to do it here
        self.width = (aa_image.width+1)*aa_image.nominal_size*aa_image.aspect_ratio
        self.height = (aa_image.height+1)*aa_image.nominal_size
        
        image = Image.new(
            'RGB',
            (int(self.width*self.scale), int(self.height*self.scale)),
            self.background
        )
        self.draw = ImageDraw.Draw(image)
  
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
                print "WARNING: don't know how to handle shape %r" % shape
        
        del self.draw
        image.save(self.file_like, self.file_type)

    # - - - - - - SVG drawing helpers - - - - - - -
    def _line(self, x1, y1, x2, y2):
        """Draw a line, coordinates given as four decimal numbers"""
        self.draw.line((x1, y1, x2, y2), fill=self.foreground) #self.line_width

    def _rectangle(self, x1, y1, x2, y2):
        """Draw a rectange, coordinates given as four decimal numbers.
           ``style`` is inserted in the SVG. It could be e.g. "fill:yellow"
        """
        self.draw.rectangle((x1, y1, x2, y2), fill=self.fillcolor, outline=self.foreground) #self.line_width

    # - - - - - - visitor function for the different shape types - - - - - - -
    
    def visit_point(self, point):
        dotsize = 2
        self.draw.ellipse(
            (
                point.x*self.scale-dotsize, point.y*self.scale-dotsize,
                point.x*self.scale+dotsize, point.y*self.scale+dotsize
            ),
            fill=self.foreground
        )
    
    def visit_line(self, line):
        x1, x2 = line.start.x, line.end.x
        y1, y2 = line.start.y, line.end.y
        self._line(x1*self.scale, y1*self.scale, x2*self.scale, y2*self.scale)
    
    def visit_rectangle(self, rectangle):
        self._rectangle(
            rectangle.p1.x*self.scale, rectangle.p1.y*self.scale,
            rectangle.p2.x*self.scale, rectangle.p2.y*self.scale,
        )
        
    
    def visit_circle(self, circle):
        self.draw.ellipse(
            (
                (circle.center.x-circle.radius)*self.scale, (circle.center.y-circle.radius)*self.scale,
                (circle.center.x+circle.radius)*self.scale, (circle.center.y+circle.radius)*self.scale
            ),
            outline=self.foreground,
            fill=self.fillcolor
        )

    def visit_label(self, label):
        #  font-weight="bold"
        self.draw.text(
            (label.position.x*self.scale, (label.position.y-self.aa_image.nominal_size*1.1)*self.scale),
            label.text,
            fill=self.foreground,
            font=ImageFont.truetype('arial.ttf', int(self.aa_image.nominal_size*self.scale))
        )

