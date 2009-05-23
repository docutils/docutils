"""\
PDF renderer for the aafigure package.

(C) 2008 Chris Liechti <cliechti@gmx.net>
"""

import sys
import reportlab
from reportlab.lib import colors
from reportlab.graphics.shapes import *
from reportlab.graphics import renderPDF

class PDFOutputVisitor:
    """Render a list of shapes as PDF vector image."""

    def __init__(self, file_like, scale = 1, line_width = 1,
                 foreground=  (0, 0, 0), background = (255, 255, 255), fillcolor = (0, 0, 0),
                 proportional = False
        ):
        self.file_like = file_like
        self.scale = 3.33*scale
        self.line_width = 0.4*line_width
        self.foreground = foreground
        self.background = background
        self.fillcolor = fillcolor
        if proportional:
            self.font = 'Helvetica'
        else:
            self.font = 'Courier'

    def _num(self, number):
        """helper to format numbers with scale for PDF output"""
        return number*self.scale

    def _color(self, color):
        r,g,b = color
        return colors.HexColor('#%02x%02x%02x' % (r,g,b))

    def visit_image(self, aa_image):
        """Process the given ASCIIArtFigure and output the shapes in
           the PDF file
        """
        self.aa_image = aa_image        # save for later XXX not optimal to do it here
        self.width = (aa_image.width)*aa_image.nominal_size*aa_image.aspect_ratio
        self.height = (aa_image.height)*aa_image.nominal_size
        self.drawing = Drawing(self._num(self.width), self._num(self.height))
        self.visit_shapes(aa_image.shapes)
        renderPDF.drawToFile(self.drawing, self.file_like, '')

    def visit_shapes(self, shapes):
        for shape in shapes:
            shape_name = shape.__class__.__name__.lower()
            visitor_name = 'visit_%s' % shape_name
            if hasattr(self, visitor_name):
                getattr(self, visitor_name)(shape)
            else:
                sys.stderr.write("WARNING: don't know how to handle shape %r\n"
                    % shape)

    # - - - - - - PDF drawing helpers - - - - - - -
    def _line(self, x1, y1, x2, y2, thick):
        """Draw a line, coordinates given as four decimal numbers"""
        self.drawing.add(Line(
            self._num(x1),  self._num(self.height-y1),
            self._num(x2),  self._num(self.height-y2),
            strokeColor=self._color(self.foreground),
            strokeWidth=self.line_width*(1+0.5*bool(thick))
        ))

    def _rectangle(self, x1, y1, x2, y2, style=''):
        """Draw a rectangle, coordinates given as four decimal numbers."""
        if x1 > x2: x1, x2 = x2, x1
        if y1 > y2: y1, y2 = y2, y1
        self.drawing.add(Rect(
            self._num(x1),  self._num(self.height-y2),
            self._num(x2-x1),  self._num(y2-y1),
            fillColor=self._color(self.fillcolor),
            strokeWidth=self.line_width
        ))

    # - - - - - - visitor function for the different shape types - - - - - - -

    def visit_point(self, point):
        self.drawing.add(Circle(
            self._num(point.x),  self._num(self.height-point.y),
            self._num(0.2),
            fillColor=self._color(self.foreground),
            strokeWidth=self.line_width
        ))

    def visit_line(self, line):
        x1, x2 = line.start.x, line.end.x
        y1, y2 = line.start.y, line.end.y
        self._line(x1, y1, x2, y2, line.thick)

    def visit_rectangle(self, rectangle):
        self._rectangle(
            rectangle.p1.x, rectangle.p1.y,
            rectangle.p2.x, rectangle.p2.y
        )


    def visit_circle(self, circle):
        self.drawing.add(Circle(
            self._num(circle.center.x), self._num(self.height-circle.center.y),
            self._num(circle.radius),
            fillColor=self._color(self.foreground),
            strokeWidth=self.line_width
        ))

    def visit_label(self, label):
        #  font-weight="bold"   style="stroke:%s"
        self.drawing.add(String(
            self._num(label.position.x), self._num(self.height-label.position.y),
            label.text,
            fontSize=self._num(self.aa_image.nominal_size),
            fontName=self.font,
            fillColor=self._color(self.foreground),
        ))

    def visit_group(self, group):
        # XXX could add a group to the PDF file
        self.visit_shapes(group.shapes)


