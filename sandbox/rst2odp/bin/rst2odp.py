#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright 2008-2009 Matt Harrison
# Licensed under Apache License, Version 2.0 (current)
import sys
import os

import docutils
from docutils import io, writers, nodes
from docutils.readers import standalone
from docutils.core import Publisher, default_description, \
    default_usage
from docutils.parsers import rst

import odplib.preso as preso
  
  
S5_COLORS = dict(
    black='#000000',
    gray='#545454',
    silver='#c0c0c0',
    white='#ffffff',
    maroon='#b03060',
    red='#ff0000',
    magenta='#ff00ff',
    fuchsia='#ff00ff', # FIX
    pink='#ff1493',
    orange='#ffa500',
    yellow='#ffff00',
    lime='#32cd32',
    green='#00ff00',
    olive='#6b8e23',
    teal='#008080',
    cyan='#00ffff',
    aqua='#00ffff', # FIX
    blue='#0000ff',
    navy='#000080',
    purple='#a020f0'
)  

S5_SIZES = dict(
    huge='66pt',
    big='44pt',
    normal='28pt',
    small='22pt',
    tiny='18pt'
)

class SyntaxHighlightCodeBlock(rst.Directive):
    required_arguments = 1
    optional_arguments = 0
    has_content = True
    #
    # See visit_literal_block for code that processes the node 
    #   created here.
    def run(self):
        language = self.arguments[0]
        code_block = nodes.literal_block(classes=["code-block", language],
            language=language)
        lines = self.content
        content = '\n'.join(lines)
        text_node = nodes.Text(content)
        code_block.append(text_node)
        # Mark this node for high-lighting so that visit_literal_block
        #   will be able to hight-light those produced here and
        #   *not* high-light regular literal blocks (:: in reST).
        code_block['hilight'] = True
        return [code_block]

rst.directives.register_directive('code-block', SyntaxHighlightCodeBlock)

class ImportNode(nodes.General, nodes.Inline, nodes.Element): pass

class ImportSlideBlock(rst.Directive):
    required_arguments = 2
    optional_arguments = 0
    has_content = False
    node_class = ImportNode
    #
    # See visit_literal_block for code that processes the node 
    #   created here.
    def run(self):
        odp_path = self.arguments[0]
        page_num = self.arguments[1]
        node = ImportNode(odp_path=odp_path, page_num=page_num)
        return [node]

rst.directives.register_directive('importslide', ImportSlideBlock)

class Writer(writers.Writer):
    settings_spec = (
        'ODP Specific Options', # option group title
        None, # Description
        ( # options (help string, list of options, dictions of OptionParser.add_option dicts)
            ('Specify a template (.otp) to use for styling',
             ['--template-file'],
             {'action': 'store',
              'dest': 'template_file'}),
            ('Specify a monospace font to use ("Courier New" default)',
             ['--mono-font'],
             {'action': 'store',
              'dest': 'mono_font'}),
            ('Specify a normal font to use ("Arial" default)',
             ['--font'],
             {'action': 'store',
              'dest': 'font'}),
            ('Specify pages to export (2,3,9-10)',
             ['--pages-to-output'],
             {'action': 'store',
              'dest': 'pages_to_output'})
            )
        )
    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = ODPTranslator

    def translate(self):
        self.visitor = self.translator_class(self.document)
        self.document.walkabout(self.visitor)
        self.parts['whole'] = self.visitor.get_whole()
        self.output = self.parts['whole']
        self.parts['encoding'] = self.document.settings.output_encoding
        self.parts['version'] = docutils.__version__
        
class ODPTranslator(nodes.GenericNodeVisitor):
    def __init__(self, document):
        nodes.GenericNodeVisitor.__init__(self, document)
        self.settings = document.settings
        self.preso = preso.Preso()
        
        if self.settings.pages_to_output:
            self.preso.limit_pages = num_string_to_list(self.settings.pages_to_output)

        if self.settings.mono_font:
            preso.MONO_FONT = self.settings.mono_font
            
        if self.settings.font:
            preso.NORMAL_FONT = self.settings.font

        self.in_node = {} # map of tagname to True if we are in/under this
        self._reset()

    def _reset(self):
        # state we keep track of
        self.cur_slide = None
        self.bullet_list = None
        self.bullet_depth = 0
        self.footer = None

    def _init_slide(self, force=False):
        if force or self.cur_slide is None:
            self._reset()
            self.cur_slide = self.preso.add_slide()

    def at(self, nodename):
        """
        shortcut for at/under this node
        """
        return self.in_node.get(nodename, False)

    def get_whole(self):
        return self.preso.get_data(self.settings.template_file)

    def dispatch_visit(self, node):
        # Easier just to throw nodes I'm in in a dict, than keeping
        # state for each one
        count = self.in_node.setdefault(node.tagname, 0)
        self.in_node[node.tagname] += 1
        nodes.GenericNodeVisitor.dispatch_visit(self, node)

    def dispatch_departure(self, node):
        self.in_node[node.tagname] -= 1
        nodes.GenericNodeVisitor.dispatch_departure(self, node)

    def default_visit(self, node):
        if self.settings.report_level >= 3:
            print "ERR! NODE", node, node.tagname
        raise NotImplementedError('node is %r, tag is %s' % (node, node.tagname))


    def default_departure(self, node):
        if self.settings.report_level >= 3:
            print "NODE", node, node.tagname
        raise NotImplementedError


    def _dumb_visit(self, node):
        pass
    _dumb_depart = _dumb_visit


    def visit_document(self, node):
        if self.settings.report_level >= 3:
            print "DOC", node
    
    depart_document = _dumb_depart

    def visit_title(self, node):
        if self.at('section') < 2:
            self._init_slide()
        if self.at('topic'):
            return
        elif self.at('sidebar'):
            return
        elif self.at('section') < 2:
            self.cur_slide.add_title_frame()
    
    def depart_title(self, node):
        if self.at('topic'):
            return
        elif self.at('sidebar'):
            return
        elif self.at('section') < 2:
            # not in a title element anymore
            self.cur_slide.cur_element = None
        else:
            pass
        
            
    def visit_Text(self, node):
        if self.bullet_list and not self.at('handout'): # !!!need to deal w/ bullets in handout
            self.bullet_list.write(node.astext())
        elif self.at('footer'):
            self.footer.write(node.astext())
        elif self.at('comment'):
            pass
        elif self.at('topic'):
            pass
        elif self.at('literal_block'):
            pass
        elif self.at('reference'):
            # FirstClown - if we have link text, we need to make sure the text
            # doesn't get any styles applied or it sometimes doesn't show.
            self.cur_slide.write(node.astext(), add_p_style=False, add_t_style=False)
        elif self.at('doctest_block'):
            pass
        elif self.at('field_name'):
            pass
        else:
            self.cur_slide.write(node.astext())
    depart_Text = _dumb_depart

    def _push_handout(self, classes):
        if 'handout' in classes:
            self.in_node['handout'] = True
            self.cur_slide.push_element()
            if not self.cur_slide.notes_frame:
                self.cur_slide.add_notes_frame()
            else:
                self.cur_slide.cur_element = self.cur_slide.notes_frame
                self.cur_slide.insert_line_break += 1
                self.cur_slide.insert_line_breaks()


    def visit_paragraph(self, node):
        classes =  node.attributes.get('classes', [])
        self._push_handout(classes)

        ## if 'center' in classes:
        ##     attribs = {'fo:text-align':'center', 
        ##                'fo:margin-left':'1.2cm',
        ##                'fo:margin-right':'-.9cm',
        ##                }
        ##     style = preso.ParagraphStyle(**attribs)
        ##     self.cur_slide.push_style(style)

        ## elif 'left' in classes:
        ##     pass # default
        ## elif 'right' in classes:
        ##     attribs = {'fo:text-align':'end',
        ##                }
        ##     style = preso.ParagraphStyle(**attribs)
        ##     self.cur_slide.push_style(style)
        p_attribs = self._get_para_attribs(node)
        if p_attribs:
            self.cur_slide.push_style(preso.ParagraphStyle(**p_attribs))
            
        # text styles
        attribs = self._get_text_attribs(node)
        if attribs:
            style = preso.TextStyle(**attribs)
            self.cur_slide.push_style(style)
            
        if self.bullet_list:
            pass
        elif self.at('topic'):
            return
        elif self.at('block_quote'):
            return # block quote adds paragraph style
        elif self.at('doctest_block'):
            pass
        

    def depart_paragraph(self, node):
        # add newline
        if not self.at('list_item'):
            self.depart_line(node)

        classes = node.attributes.get('classes', [])
        if 'center' in classes or 'right' in classes:
            self.cur_slide.pop_node()
        if 'handout' in classes:
            self.in_node['handout'] = False
            self.cur_slide.pop_element()
            
        if self._get_text_attribs(node):
            # pop off text:span
            self.cur_slide.pop_node()
            self.cur_slide.pop_style()

        elif self.at('topic'):
            return
        elif self.at('block_quote'):
            return # block quote adds paragraph style
        else:
            self.cur_slide.parent_of('text:p')

    visit_definition = _dumb_visit
    depart_definition = _dumb_depart

    def visit_bullet_list(self, node):
        if self.at('topic'):
            return
        p_attribs = self._get_para_attribs(node)
        if p_attribs:
            self.cur_slide.push_style(preso.ParagraphStyle(**p_attribs))

        attribs = self._get_text_attribs(node)
        if attribs:
            style = preso.TextStyle(**attribs)
            self.cur_slide.push_style(style)

        classes =  node.attributes.get('classes', [])
        if 'handout' in classes:
            self._push_handout(classes)

        self.bullet_depth += 1
        if not self.bullet_list:
            # start a new list
            self.bullet_list = preso.OutlineList(self.cur_slide)
            self.cur_slide.add_list(self.bullet_list)
        else:
            # indent one more
            self.bullet_list.indent()
        if 'incremental' in node.attributes.get('classes', []):
            self.in_node['incremental'] = True 

    def depart_bullet_list(self, node):
        if self.at('topic'):
            return

        classes = node.attributes.get('classes', [])
        if 'handout' in classes:
            self.in_node['handout'] = False
            self.cur_slide.pop_element()
            
        self.bullet_depth -= 1
        if self.bullet_depth == 0:
            # done with list
            self.bullet_list = None
            self.cur_slide.pop_element()
            self.cur_slide.insert_line_break += 1
        else:
            self.bullet_list.dedent()            
        if 'incremental' in node.attributes.get('classes', []):
            self.in_node['incremental'] = False

    visit_definition_list = visit_bullet_list
    depart_definition_list = depart_bullet_list


    def visit_list_item(self, node):
        if self.at('topic'):
            return
        if self.at('incremental'):
            self.cur_slide.start_animation(preso.Animation())
        self.bullet_list.new_item()

    def depart_list_item(self, node):
        if self.at('topic'):
            return
        if self.at('incremental'):
            self.cur_slide.end_animation()

    visit_definition_list_item = visit_list_item
    depart_definition_list_item = depart_list_item


    visit_decoration = _dumb_visit
    depart_decoration = _dumb_depart

    def visit_footer(self, node):
        self.footer = preso.Footer(self.cur_slide)

    def depart_footer(self, node):
        self.preso.add_footer(self.footer)
        self.footer = None
        
    visit_docinfo = _dumb_visit
    depart_docinfo = _dumb_depart

    # bibliographic elements
    def visit_author(self, node):
        self.visit_attribution(node)

    def depart_author(self, node):
        self.depart_line(node) # add new-line
        self.depart_attribution(node)

    visit_copyright = visit_author
    depart_copyright = depart_author

    def visit_date(self, node):
        self.visit_attribution(node)

    def depart_date(self, node):
        self.depart_line(node) # add new-line
        self.depart_attribution(node)

    def visit_field(self, node):
        pass

    def depart_field(self, node):
        pass

    def visit_field_name(self, node):
        pass # maybe put this somewhere

    def depart_field_name(self, node):
        pass

    def visit_field_body(self, node):
        self.visit_attribution(node)

    def depart_field_body(self, node):
        self.depart_attribution(node)
        
    def visit_comment(self, node):
        pass
    def depart_comment(self, node):
        pass

    visit_topic = _dumb_visit
    depart_topic = _dumb_depart
 
    def visit_reference(self, node):
        """
        <draw:text-box>
          <text:p text:style-name="P4">
            <text:a xlink:href="http://www.yahoo.com/">Yahoo corp</text:a>
          </text:p>
        </draw:text-box>
        """
        if node.has_key('refid'):
            return
        elif self.at('topic'):
            return
        elif self.at('field_body'):
            self.visit_attribution(node)
            self.cur_slide.push_pending_node('text:a', {'xlink:href': '%s' % node['refuri'],
                                                        'xlink:type': 'simple'})
            self.cur_slide.write(node.astext()) 
        else:
            # needs to be in a p
            # need to hack a bit, since .write usually inserts text:p and text:span
            if not self.cur_slide.cur_element or not self.cur_slide.cur_element._in_p():
                #self.cur_slide.add_node('text:p', {})
                # pyohio code
                # we write an empty string since write() creates the paragraph
                # we need, with the style needed to reset from a possible Title
                # P0 style. This was most apparent when a link was first word
                # in a section after a title.
                self.cur_slide.write("")

            self.cur_slide.add_node('text:a', attrib={'xlink:href': '%s' % node['refuri'],
                                                      'xlink:type': 'simple'})
            
    def depart_reference(self, node):
        if node.has_key('refid'):
            return
        elif self.at('topic'):
            return
        elif self.at('field_body'):
            self.depart_attribution(node)
            self.cur_slide.parent_of('text:a')
        else:
            self.cur_slide.parent_of('text:a')

    def visit_target(self,node):
        # Skip the target element since the <reference> of the target is
        # responsible for writing out the content
        pass
    
    def depart_target(self, node):
        pass

    def visit_container(self, node):
        classes = node.attributes.get('classes', [])
        self._push_handout(classes)

    def depart_container(self, node):
        if self.in_node.get('handout', False):
            self.cur_slide.pop_element()
            self.in_node['handout'] = False

    visit_substitution_definition = _dumb_visit
    depart_substitution_definition = _dumb_depart

    def visit_section(self, node):

        # first page has no section
        if self.at('section') < 2:
            # don't create slide for subsections
            self._init_slide(force=True)

    def depart_section(self, node):
        if self.at('section') < 1:
            self._reset()

    def visit_transition(self, node):
        # hack to have titleless slides (transitions come in between sections)
        self._reset()
        self._init_slide(force=True)
    
    depart_transition = _dumb_depart

    def visit_literal(self, node):
        style = preso.TextStyle(**{
                'fo:font-family':preso.MONO_FONT,
                'style:font-family-generic':"swiss",
                'style:font-pitch':"fixed"})
        self.cur_slide.push_style(style)

    def depart_literal(self, node):
        # pop off the text:span
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()

    def visit_inline(self,node):
        attribs = self._get_text_attribs(node)
        if attribs:
            style = preso.TextStyle(**attribs)       

            self.cur_slide.push_style(style)
        if 'incremental' in node.attributes.get('classes', []):
            self.cur_slide.start_animation(preso.Animation())

    def depart_inline(self, node):
        # pop off the text:span
        attribs = self._get_text_attribs(node)
        if attribs:
            self.cur_slide.pop_style()
            self.cur_slide.pop_node()
        if 'incremental' in node.attributes.get('classes', []):
            self.cur_slide.end_animation()

    def visit_emphasis(self, node):
        attribs = {'fo:font-style':'italic'}
        style = preso.TextStyle(**attribs)       
        self.cur_slide.push_style(style)

    def depart_emphasis(self, node):
        # pop off the text:span
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()

    visit_title_reference = visit_emphasis
    depart_title_reference = depart_emphasis


    def visit_strong(self, node):
        attribs = {'fo:font-weight':'bold'}
        style = preso.TextStyle(**attribs)       
        self.cur_slide.push_style(style)
       
    def depart_strong(self, node):
        # pop off the text:span
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()

    visit_term = visit_strong
    depart_term = depart_strong

    def visit_superscript(self, node):
        attribs = {'style:text-position':'super 58%'}
        style = preso.TextStyle(**attribs)
        self.cur_slide.push_style(style)

    def depart_superscript(self, node):
        # pop off the text:span
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()
        
    def visit_subscript(self, node):
        attribs = {'style:text-position':'sub 58%'}
        style = preso.TextStyle(**attribs)
        self.cur_slide.push_style(style)

    def depart_subscript(self, node):
        # pop off the text:span
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()
        
    def visit_block_quote(self, node):
        attribs = {'fo:text-align':'start', 
                   'fo:margin-left':'1.2cm',
                   'fo:margin-right':'-.9cm',
                   }
        style = preso.ParagraphStyle(**attribs)
        self.cur_slide.push_style(style)

    def depart_block_quote(self, node):
        # pop off the text:p
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()
        
    def visit_attribution(self, node):
        # right justify
        attribs = {'fo:text-align':'end',
                   'fo:margin-right':'.9cm'
                   }
        style = preso.ParagraphStyle(**attribs)
        self.cur_slide.push_style(style)

        # italics
        style = preso.TextStyle(**{'fo:font-size':S5_SIZES['small'],
                                   'fo:font-style':'italic'})
        self.cur_slide.push_style(style)

    def depart_attribution(self, node):
        # pop off the text:p and text:span
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()

    def visit_line_block(self, node):
        #jump out of current paragraph
        self.cur_slide.parent_of('text:p')

        p_attribs = self._get_para_attribs(node)
        if p_attribs:
            self.cur_slide.push_style(preso.ParagraphStyle(**p_attribs))
        else:    
            # right justify
            P_attribs = {'fo:text-align':'end',
                       'fo:margin-right':'.9cm'
                       }
            style = preso.ParagraphStyle(**p_attribs)
            self.cur_slide.push_style(preso.ParagraphStyle(**p_attribs))
        #self.cur_slide.push_style(style)
        attribs = self._get_text_attribs(node)
        if attribs:
            style = preso.TextStyle(**attribs)       
            self.cur_slide.push_style(style)

    def depart_line_block(self, node):
        attribs = self._get_text_attribs(node)
        if attribs:
            #self.cur_slide.pop_style()
            self.cur_slide.pop_node()
        # pop off text:p
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()
        

    def visit_line(self, node):
        pass

    def depart_line(self, node):
        self.cur_slide.insert_line_break += 1
        self.cur_slide.insert_line_breaks()
        
    @preso.cwd_decorator
    def visit_image(self, node):
        classes = node.attributes.get('classes', [])

        source = node.attributes['uri']
        p = preso.Picture(os.path.abspath(source), **node.attributes)
        self.cur_slide.add_picture(p)

    def depart_image(self, node):
        pass

    visit_figure = _dumb_visit
    depart_figure = _dumb_depart

    def visit_caption(self, node):
        #!!! fix
        pass

    def depart_caption(self, node):
        pass

    def visit_literal_block(self, node):
        attributes = node.attributes
        # left align
        style = preso.ParagraphStyle(**{'fo:text-align':'start'})
        self.cur_slide.push_style(style)
        # text styles
        attribs = self._get_text_attribs(node)
        if attribs:
            style = preso.TextStyle(**attribs)       
            self.cur_slide.push_style(style)

        if attributes['classes'] and 'code-block' in attributes['classes']:
            node_input = node.astext()
            language = node.attributes['language']
            self.cur_slide.add_code(node_input, language)
            # insert a new line after
            self.cur_slide.insert_line_break += 1

        else:
            style = preso.TextStyle(**{
                    'fo:font-family':preso.MONO_FONT,
                    'style:font-family-generic':"swiss",
                    'style:font-pitch':"fixed"})
            self.cur_slide.push_style(style)
            node_input = node.astext()
            chunks = node_input.split('\n')
            for chunk in chunks:
                self.cur_slide.write(chunk)
                self.cur_slide.insert_line_break += 1
                self.cur_slide.insert_line_breaks()



    def depart_literal_block(self, node):
        # pop text-align
        self.cur_slide.pop_style()
        self.cur_slide.pop_node()
        attributes = node.attributes
        if attributes['classes'] and 'code-block' in attributes['classes']:
            pass
        else:
            self.cur_slide.pop_style()
            self.cur_slide.pop_node()

    def visit_footnote(self, node):
        # shift to bottom of page?
        pass

    def depart_footnote(self, node):
        pass

    def visit_footnote_reference(self, node):
        self.visit_superscript(node)
        self.cur_slide.write('[')

    def depart_footnote_reference(self, node):
        self.cur_slide.write(']')
        self.depart_superscript(node)

    
    def visit_label(self, node):
        # part of footnote
        if self.at('footnote'):
            self.cur_slide.write('[')

    def depart_label(self, node):
        if self.at('footnote'):
            self.cur_slide.write('] ')


    def visit_enumerated_list(self, node):
        if self.at('topic'):
            return
        self.bullet_depth += 1
        if not self.bullet_list:
            self.bullet_list = preso.NumberList(self.cur_slide)
            self.cur_slide.add_list(self.bullet_list)
        else:
            # indent one more
            self.bullet_list.indent()
        if 'incremental' in node.attributes.get('classes', []):
            self.in_node['incremental'] = True 


    def depart_enumerated_list(self, node):
        self.depart_bullet_list(node)

    def visit_doctest_block(self, node):
        node_input = node.astext()
        language = 'pycon'
        self.cur_slide.add_code(node_input, language)
        # insert a new line after
        self.cur_slide.insert_line_break += 1


    def depart_doctest_block(self, node):
        pass

    def visit_importslide(self, node):
        pass


    def visit_table(self, node):
        table = preso.TableFrame(self.cur_slide)
        self.cur_slide.add_table(table)

    def depart_table(self, node):
        self.cur_slide.pop_element()

    def visit_row(self, node):
        self.cur_slide.cur_element.add_row()

    def depart_row(self, node):
        pass

    def visit_entry(self, node):
        self.cur_slide.cur_element.add_cell()

    def depart_entry(self, node):
        pass
    
    visit_tgroup = _dumb_visit
    depart_tgroup = _dumb_depart

    visit_colspec = _dumb_visit
    depart_colspec = _dumb_depart

    visit_thead = _dumb_visit
    depart_thead = _dumb_depart

    visit_tbody = _dumb_visit
    depart_tbody = _dumb_depart

    def visit_hint(self, node):
        return self._visit_hint(node, 'Hint')
    
    
    def _visit_hint(self, node, name):
        if self.cur_slide.text_frames:
            # should adjust width of other frame
            node = self.cur_slide.text_frames[-1].get_node()
            node.attrib['svg:width'] = '12.296cm'
        else:
            self.cur_slide.add_text_frame()
            
        self.cur_slide.push_element()
        #put the hint on the right side
        attrib = {
            'presentation:style-name':'pr2',
            'draw:layer':'layout',
            'svg:width':'12.296cm',
            'svg:height':'13.86cm',
            'svg:x':'14.311cm',
            'svg:y':'4.577cm',
            'presentation:class':'subtitle'
        }
        self.cur_slide.add_text_frame(attrib)
        self.cur_slide.write(name)
        self.cur_slide.insert_line_break = 2
        self.cur_slide.insert_line_breaks()

    def depart_hint(self, node):
        self.cur_slide.pop_element()

    def visit_sidebar(self, node):
        return self._visit_hint(node, 'Sidebar')

    depart_sidebar = depart_hint
        
        
    def _get_para_attribs(self, node):
        classes = node.attributes.get('classes', [])
        attribs = {}
        for c in classes:
            if c == 'center':
                attribs.update({'fo:text-align':'center', 
                                'fo:margin-left':'1.2cm',
                                'fo:margin-right':'.9cm',
                                })
            elif c == 'right':
                attribs.update({'fo:text-align':'end',
                                'fo:margin-right':'.9cm'
                                })
            elif c == 'left':
                attribs.update({'fo:text-align':'start', 
                                'fo:margin-left':'1.2cm',
                                'fo:margin-right':'5.9cm',
                                })
                #pass # default
        return attribs
        

    def _get_text_attribs(self, node):
        classes = node.attributes.get('classes', [])
        attribs = {}
        for c in classes:
            if c in S5_COLORS:
                attribs['fo:color'] = S5_COLORS[c]
            elif c in S5_SIZES:
                attribs['fo:font-size'] = S5_SIZES[c]
        return attribs

def num_string_to_list(numstr):
    """
    >>> num_string_to_list('2,5-7')
    [2, 5, 6, 7]
    >>> num_string_to_list('1')
    [1]

    """
    nums = []
    if ',' in numstr:
        comma_delim = numstr.split(',')
        for part in comma_delim:
            if '-' in part:
                start, end = [int(x) for x in part.split('-')]
                for num in range(start, end+1):
                    nums.append(num)
            else:
                nums.append(int(part))
    elif '-' in numstr:
        start, end = [int(x) for x in numstr.split('-')]
        for num in range(start, end+1):
            nums.append(num)
    else:
        nums.append(int(numstr))
    return nums
            

class BinaryFileOutput(io.FileOutput):
    """
    A version of docutils.io.FileOutput which writes to a binary file.
    """
    def open(self):
        try:
            self.destination = open(self.destination_path, 'wb')
            
        except IOError, error:
            if not self.handle_io_errors:
                raise
            print >>sys.stderr, '%s: %s' % (error.__class__.__name__,
                                            error)
            print >>sys.stderr, ('Unable to open destination file for writing '
                                 '(%r).  Exiting.' % self.destination_path)
            sys.exit(1)
        self.opened = 1


def main(prog_args):
    argv = None
    reader = standalone.Reader()
    reader_name = 'standalone'
    writer = Writer()
    writer_name = 'pseudoxml'
    parser = None
    parser_name = 'restructuredtext'
    settings = None
    settings_spec = None
    settings_overrides = None
    config_section = None
    enable_exit_status = 1
    usage = default_usage
    publisher = Publisher(reader, parser, writer, settings,
                          destination_class=BinaryFileOutput)
    publisher.set_components(reader_name, parser_name, writer_name)
    description = ('Generates OpenDocument/OpenOffice/ODF slides from '
                   'standalone reStructuredText sources.  ' + default_description)

    output = publisher.publish(argv, usage, description,
                               settings_spec, settings_overrides,
                               config_section=config_section,
                               enable_exit_status=enable_exit_status)


def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    if '--doctest' in sys.argv:
        _test()
    else:
        sys.exit(main(sys.argv) or 0)
