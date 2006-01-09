"""
implement aafigure directive
"""

import os, sys, tempfile, popen2
import aafigure
import svg
try:
    import pil
except ImportError:
    #no support for bitmaps
    pil = None

from docutils import nodes
from docutils.parsers.rst.directives import register_directive

aafigure_counter = 0

def AAFigureDrective(name, arguments, options, content, lineno,
                  content_offset, block_text, state, state_machine):
    text = '\n'.join(content)
    
    global aafigure_counter
    aafigure_counter += 1
    
    aaimg = aafigure.AsciiArtImage(text)
    #~ print text
    aaimg.recognize()
    #ensure that options are present
    if not options.has_key('scale'): options['scale'] = 1
    if not options.has_key('line_width'): options['line_width'] = 2
    if not options.has_key('format'): options['format'] = 'svg'
    
    if options['format'] == 'svg':
        output_name = 'aafigure-%i.svg' % aafigure_counter
        svgout = svg.SVGOutputVisitor(
            file(output_name, 'w'),
            scale = options['scale']*10,
            line_width = options['line_width'],
            debug = True
        )
        svgout.visit(aaimg)
        attributes = {'format': 'html'}
        result = [nodes.raw('', '<embed src="%s" %s type="image/svg+xml"/>' % (
            output_name,
            svgout.get_size_attrs()
        ), **attributes)]
    elif pil is not None:
        output_name = 'aafigure-%i.%s' % (aafigure_counter, options['format'])
        pilout = pil.PILOutputVisitor(
            file(output_name, 'wb'),
            scale = options['scale']*10,
            line_width = options['line_width'],
            debug = True,
            file_type = options['format']
        )
        try:
            pilout.visit(aaimg)
        except KeyError:
            result = [state_machine.reporter.error(
                'No support for image format %r' % (options['format']),
                nodes.literal_block(block_text, block_text),
                line=lineno
            )]
        else:
            # Return an image directive.
            image_options = {}
            image_options['uri'] = os.path.basename(output_name)
            result = [nodes.image(output_name, **image_options)]
    else:
        result = [state_machine.reporter.error(
            'No support for image format %r (PIL not installed)' % (options['format']),
            nodes.literal_block(block_text, block_text),
            line=lineno
        )]
    
    return result

AAFigureDrective.content = True
#~ AAFigureDrective.arguments = (1, 1, 1)
AAFigureDrective.options = {
    'scale': float,
    'line_width': float,
    'format': str,
}

def register():
    register_directive('aafigure', AAFigureDrective)
