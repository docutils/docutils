# Author: Aleksey Gurtovoy
# Contact: agurtovoy@meta-comm.com
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

from docutils import writers
from docutils.writers import html4css1
import docutils.nodes

import urlparse
import os.path
import os
import re


class Writer(writers.Writer):

    settings_spec = html4css1.Writer.settings_spec + (
        'HTML/Frames-Specific Options',
        """The HTML --stylesheet option's default is set to """
        '"frames.css".',
        ()
        )

    settings_default_overrides = { 'stylesheet': 'frames.css' }
    relative_path_settings = ('stylesheet_path',)
    config_section = 'html4frames writer'
    config_section_dependencies = ('writers',)

    def __init__(self):
        self.__super = writers.Writer
        self.__super.__init__(self)
        self.translator = frame_pages_translator

    def translate(self, document, page_files_dir, extension):
        visitor = self.translator(document, page_files_dir, extension)
        document.walkabout(visitor)
        self.output = None # needed by writers.Writer
        return visitor.result()

    def write(self, document, destination):
        self.language = docutils.languages.get_language(
            document.settings.language_code)

        index_file_path = destination.destination_path
        index_file_name = os.path.basename(index_file_path)
        (page_files_dir, extension) = os.path.splitext(index_file_name)
        chunk_files_path = os.path.join(
              os.path.dirname(index_file_path)
            , page_files_dir
            )
        
        (index_text, pages) = self.translate(document, page_files_dir, extension)
                
        if len(pages) > 0 and not os.path.exists(chunk_files_path):
            os.mkdir(chunk_files_path)
        
        destination.write(index_text)
        
        for c in pages.keys():
            chunk_file = self.make_chunk_output(
                  destination
                , os.path.join(chunk_files_path,c + extension)
                )
            
            chunk_file.write(pages[c])

    def make_chunk_output(self, destination, destination_path):
        return docutils.io.FileOutput(
              destination_path=destination_path
            , encoding=destination.encoding
            , error_handler=destination.error_handler
            )


class frame_pages_translator(docutils.nodes.NodeVisitor):
    
    page_translator = html4css1.HTMLTranslator
    tocframe = 'tocframe'
    docframe = 'docframe'
    tocframe_width = 30
    nav_bar_separator = '<hr class="navigation-bar-separator">\n'
    re_href = re.compile('(href=")(.*?)(")')
        
    def __init__(self, document, page_files_dir, extension):
        self.__super = docutils.nodes.NodeVisitor
        self.__super.__init__(self, document)
        
        self.section_level = 0
        self.max_chunk_level = 0
        self.settings = document.settings
        self.reporter = document.reporter
        self.visitors = [self.__page_translator(document, self.docframe)]
        self.toc_nav_builder = None
        self.page_files_dir = page_files_dir
        self.extension = extension
        self.pages = {}
        self.page_subtoc = {}
        self.tocframe_page = _toc_entry(id='%s_toc' % self.page_files_dir)
        self.home_page = _toc_entry(id='%s_home' % self.page_files_dir)
        self.toc = self.home_page
        self.in_home_page = 0

    def active_visitor(self):
        return self.visitors[-1]

    def result(self):
        self.pages[self.home_page.id] = self.visitors[0].astext()
        html_spec = self.visitors[0]
        index_page = ''.join( 
                  html_spec.head_prefix + html_spec.head + html_spec.stylesheet
                + ['</head>\n<frameset cols="%d%%,%d%%">\n' 
                      % (self.tocframe_width, 100 - self.tocframe_width)]
                + ['<frame name="%s" src="%s" scrolling="auto">\n' 
                    % (self.tocframe, self.__chunk_ref(self.tocframe_page.id, 1))]
                + ['<frame name="%s" src="%s" scrolling="auto">\n' 
                    % (self.docframe, self.__chunk_ref(self.home_page.id, 1))]
                + ['</frameset>\n</html>\n']
                )

        return (index_page, self.pages)


    def visit_section(self, node):
        self.section_level += 1
        if self.section_level <= self.max_chunk_level:
            section_id = node.get('id')
            if self.page_subtoc.has_key(section_id):
                node.append(self.page_subtoc[section_id])
            
            self.visitors.append(self.__page_translator(
                  self.__node_to_document(node)
                , self.docframe
                ))

            self.toc = self.toc.next
            self.active_visitor().nav_bar = self.__nav_bar(self.toc)
            self.active_visitor().body.append(self.active_visitor().nav_bar)
            self.active_visitor().body.append(self.nav_bar_separator)

        self.active_visitor().visit_section(node)

    def depart_section(self, node):
        self.active_visitor().depart_section(node)
        if self.section_level <= self.max_chunk_level:
            self.active_visitor().body.append(self.nav_bar_separator)
            self.active_visitor().body.append(self.active_visitor().nav_bar)
            visitor = self.visitors.pop()
            self.pages[node.get('id')] = visitor.astext()

        self.section_level -= 1


    def visit_topic(self, node):
        if node.get('class') == 'contents' and not self.in_home_page:
            self.toc_nav_builder = _toc_nav_builder(home=self.home_page)
            self.visitors.append(self.__page_translator(
                  self.__node_to_document(node)
                , self.tocframe
                ))
        else:
            self.active_visitor().visit_topic(node)

    def depart_topic(self, node):
        if node.get('class') == 'contents' and not self.in_home_page:
            self.toc_nav_builder = None
            tocframe_visitor = self.visitors.pop()
            self.pages[self.tocframe_page.id] = tocframe_visitor.astext()
            self.in_home_page = 1
            home_page_toc = self.page_subtoc[self.home_page.id]
            home_page_toc.walkabout(self)
            self.in_home_page = 0
        else:
            self.active_visitor().depart_topic(node)


    def visit_bullet_list(self, node):
        if self.__is_in_main_toc():
            section_id = self.toc_nav_builder.last_visited.id
            self.page_subtoc[section_id] = self.__subtoc(node)
            self.toc_nav_builder.subsection()
            self.section_level += 1
            if self.section_level > self.max_chunk_level:
                self.max_chunk_level = self.section_level

        self.active_visitor().visit_bullet_list(node)

    def depart_bullet_list(self, node):
        self.active_visitor().depart_bullet_list(node)
        if self.__is_in_main_toc():
            self.toc_nav_builder.up()
            self.section_level -= 1


    def visit_reference(self, node):
        self.__adjust_node_uri(node, 'refuri')
        self.active_visitor().visit_reference(node)
        if node.has_key('refuri'):
            self.active_visitor().body[-1] = self.__add_target_attr(
                  self.active_visitor().body[-1]
                , '_top'
                )

        if node.has_key('refid'):
            section_id = node.get('refid')
            self.active_visitor().body[-1] = self.__replace_href(
                  self.__chunk_ref(section_id)
                , self.active_visitor().body[-1]
                )

            if self.__is_in_main_toc():
                self.toc_nav_builder.along(section_id)
    

    def depart_reference(self, node):
        self.active_visitor().depart_reference(node)


    def visit_image(self, node):
        self.__adjust_node_uri(node, 'uri')
        self.active_visitor().visit_image(node)

    def depart_image(self, node):
        self.active_visitor().depart_image(node)


    def visit_title(self, node):
        self.active_visitor().visit_title(node)
        if node.has_key('refid'):
            self.active_visitor().body[-1] = self.__replace_href(
                  '%s#%s' % ( self.__chunk_ref(self.toc.up.id), node.get('refid') )
                , self.active_visitor().body[-1]
                )

    def depart_title(self, node):
        self.active_visitor().depart_title(node)

    
    def __page_translator(self, document, frame):
        result = self.page_translator(document)
        result.body_prefix = ['</head>\n<body class="%s">\n' % frame]
        return result
        
    def __subtoc(self, node):
        def _auto_toc_filter(node,root=node):
            return  node != root \
                and isinstance(node, docutils.nodes.bullet_list)

        node['id'] = 'outline'
        if node.get('class', '') == '': node['class'] = 'toc'
        return _filter_tree(
              self.__node_to_document(node)
            , _auto_toc_filter
            )


    def __add_target_attr(self, href, target):        
        return self.re_href.sub(r'\1\2\3 target="%s"' % target, href)

    def __replace_href(self, new, old):
        if not self.__is_in_main_toc():
            return self.re_href.sub(r'\1%s\3' % new, old)
        else:
            return self.re_href.sub(
                  r'\1%s\3 target="%s"' % (new, self.docframe)
                , old
                )

    def __nav_bar(self, toc_node):
        return '<span class="navigation-bar">%s</span>\n' \
            % '<span class="navigation-group-separator">&nbsp;|&nbsp;</span>'.join([
                  self.__nav_group(['Prev', 'Next'],  [toc_node.prev, toc_node.next])
                , self.__nav_group(['Back', 'Along'], [toc_node.back, toc_node.along])
                , self.__nav_group(['Up',   'Home'],  [toc_node.up, self.home_page])
                ])

    def __nav_group(self, labels, nodes):
        return '<span class="navigation-group">%s</span>' % '&nbsp;'.join(
              map(lambda l,n: self.__toc_node_link(l, n), labels, nodes)
            )
    
    
    def __node_to_document(self, node):
        node.settings = self.settings
        node.reporter = self.reporter
        return node

    def __adjust_node_uri(self, node, attr):
        if node.has_key(attr):
            src_uri = node[attr]
            if _is_uri_relative(src_uri):
                node[attr] = '../%s' % src_uri    
    
    def __toc_node_link(self, name, toc_node):
        if not toc_node: return name
        return '<a href="%s" class="navigation-link">%s</a>' % ( 
              self.__chunk_ref(toc_node.id)
            , name
            )
    

    def __chunk_ref(self, chunk_name, from_index=0):
        if from_index:
            return './%s/%s%s' % (self.page_files_dir, chunk_name, self.extension)
        else:
            return './%s%s' % (chunk_name, self.extension)

    def __is_in_main_toc(self):
        return self.toc_nav_builder


def _setup_forwarding(visitor):
    for name in docutils.nodes.node_class_names:
        if not getattr(visitor, 'visit_' + name, None):
            def forward_visit(self, node, name=name):
                getattr(self.active_visitor(), 'visit_' + name)(node)
            def forward_depart(self, node, name=name):
                getattr(self.active_visitor(), 'depart_' + name)(node)
            
            setattr(visitor, 'visit_' + name, forward_visit)
            setattr(visitor, 'depart_' + name, forward_depart)

_setup_forwarding(frame_pages_translator)


def _filter_tree(document, filter):

    class filter_tree_copy_visitor(docutils.nodes.TreeCopyVisitor):

        def __init__(self, document, filter):
            self.__super = docutils.nodes.TreeCopyVisitor
            self.__super.__init__(self, document)
            self.filter = filter

        def default_visit(self, node):
            if self.filter(node):
                raise docutils.nodes.SkipNode()
            
            self.__super.default_visit(self, node)

    visitor = filter_tree_copy_visitor(document, filter)
    document.walkabout(visitor)
    return visitor.get_tree_copy()


def _is_uri_relative(uri):
    (scheme, location, path, query, fragment) = urlparse.urlsplit(uri)
    return len(scheme) == 0 and len(location) == 0


class _toc_nav_builder:

    def __init__(self, home):
        self.last_visited = self.last_sibling = home

    def subsection(self):
        parent = self.last_visited
        self.last_visited = _toc_entry(id=None)
        self.last_visited.up = self.last_visited.prev = parent
        parent.next = self.last_visited
        self.last_sibling = self.last_visited

    def along(self, section_id):
        last = self.last_visited
        if last.id:
            self.last_visited = _toc_entry(id=section_id)
            self.last_visited.prev = last
            self.last_visited.back = self.last_sibling
            self.last_visited.up = self.last_sibling.up
            last.next = self.last_sibling.along = self.last_visited
            self.last_sibling = self.last_visited
        else:
            self.last_visited.id = section_id

    def up(self):
        self.last_sibling = self.last_sibling.up


class _toc_entry:
    def __init__(self, id):
        self.id = id
        self.prev = self.next = self.back = self.along = self.up = None
