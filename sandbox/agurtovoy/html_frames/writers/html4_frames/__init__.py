# Author: Aleksey Gurtovoy
# Contact: agurtovoy@meta-comm.com
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

from docutils import frontend, writers, utils
from docutils.writers import html4css1
import docutils.nodes

import urlparse
import shutil
import os.path
import os
import re

from docutils.transforms import writer_aux


class Writer(writers.Writer):

    default_stylesheet = 'style.css'
    default_stylesheet_path = utils.relative_path(
        os.path.join(os.getcwd(), 'dummy'),
        os.path.join(os.path.dirname(__file__), default_stylesheet))

    settings_spec = html4css1.Writer.settings_spec + (
        'HTML/Frames-Specific Options'
      , ''
      , (
            ('Chunked output without frames. Default: disabled.'
                , ['--no-frames']
                , { 'default': 0, 'action': 'store_true',
                    'validator': frontend.validate_boolean }
                )
          , ('Do not copy the stylesheet to the chunks output directory. '
             'Default: disabled if \'--embed-stylesheet\' is not specified, '
             'enabled otherwise.'
                , ['--dont-copy-stylesheet']
                , { 'default': 0, 'action': 'store_true',
                    'validator': frontend.validate_boolean }
                )
          , ('Specify the chunks output directory. Default is a base name '
             'of the output document (without extension).'
                , ['--chunk-dir-name']
                , { 'default': None, 'metavar': '<name>' }
                )
          , ('Specify the TOC frame\'s width. Default is "25%".', ['--toc_frame_width']
                , { 'default': '25%', 'metavar': '<name>' }
                )
        )
      )

    settings_default_overrides = {
          'stylesheet_path': default_stylesheet_path
        , 'embed_stylesheet': False
        }

    config_section = 'html4_frames writer'
    config_section_dependencies = ('writers',)

    def __init__(self):
        self.__super = writers.Writer
        self.__super.__init__(self)
        self.translator = frame_pages_translator

    def get_transforms(self):
        return writers.Writer.get_transforms(self) + [writer_aux.Admonitions]

    def translate(self, document, index_file, page_files_dir, extension):
        visitor = self.translator(document, index_file, page_files_dir, extension)
        document.walkabout(visitor)
        self.output = None # needed by writers.Writer
        return visitor.result()

    def write(self, document, destination):
        self.document = document
        self.language = docutils.languages.get_language(
            document.settings.language_code)
        self.destination = destination

        index_file_path = destination.destination_path
        index_file_name = os.path.basename(index_file_path)
        (index_file, extension) = os.path.splitext(index_file_name)
        
        if document.settings.chunk_dir_name:
            page_files_dir = document.settings.chunk_dir_name
        else:
            page_files_dir = index_file
        
        chunk_files_path = os.path.join(
              os.path.dirname(index_file_path)
            , page_files_dir
            )

        output_stylesheet = None
        if document.settings.embed_stylesheet:
            document.settings.dont_copy_stylesheet = 1
        
        if not document.settings.dont_copy_stylesheet:
            stylesheet_path = utils.get_stylesheet_reference(document.settings)
            if stylesheet_path:
                output_stylesheet = os.path.basename(stylesheet_path)
                document.settings.stylesheet = '%s/%s' % ( page_files_dir, output_stylesheet )
                document.settings.stylesheet_path = None

            
        (index_text, pages) = self.translate(document, index_file, page_files_dir, extension)
                
        if len(pages) > 0 and not os.path.exists(chunk_files_path):
            os.mkdir(chunk_files_path)

        if output_stylesheet:
            shutil.copy(
                  stylesheet_path
                , os.path.join(chunk_files_path, output_stylesheet)
                )
        
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
    re_href = re.compile('(href=")(.*?)(")')

    def __init__(self, document, index_file, page_files_dir, extension):
        self.__super = docutils.nodes.NodeVisitor
        self.__super.__init__(self, document)
        
        self.section_level = 0
        self.max_chunk_level = 0
        self.settings = document.settings                
        self.reporter = document.reporter
        self.toc_nav_builder = None
        self.page_files_dir = page_files_dir
        self.index_file = index_file
        self.extension = extension
        self.document_title = None
        self.pages = {}
        self.page_subtoc = {}

        if self.settings.no_frames:
            self.visitors = [self._page_translator(document, self.docframe)]
            self.home_page = _toc_entry( self._chunk_id( document ), 'Front Page' )
            self.tocframe = self.docframe
        else:
            self.visitors = [self._page_translator(self._node_to_document(document), self.docframe)]
            self.home_page = _toc_entry( self._chunk_id( document ), 'Front Page' )

        self.full_toc_page = _toc_entry('%s_toc' % self.page_files_dir, 'Full TOC')
        self.full_toc_page.up = self.home_page
        
        self.toc = self.home_page
        self.in_home_page = 0


    def active_visitor(self):
        return self.visitors[-1]


    def result(self):
        if self.settings.no_frames:
            return (self.visitors[0].astext(), self.pages)        
        else:
            self.pages[self.home_page.id] = self.visitors[0].astext()
            html_spec = self.visitors[0]
            index_page = ''.join( 
                    html_spec.head_prefix + html_spec.head
                    + ['</head>\n<frameset cols="%s,*">\n' 
                        % self.settings.toc_frame_width]
                    + ['<frame name="%s" src="%s" scrolling="auto">\n' 
                        % (self.tocframe, self._chunk_ref(None, self.full_toc_page.id))]
                    + ['<frame name="%s" src="%s" scrolling="auto">\n' 
                        % (self.docframe, self._chunk_ref(None, self.home_page.id))]
                    + ['</frameset>\n</html>\n']
                    )

            return (index_page, self.pages)


    def visit_raw(self, node):
        if node.get('format') == 'html/pre_docinfo':
            self.active_visitor().body_pre_docinfo.insert(0, node.astext())
            raise docutils.nodes.SkipNode

        self.active_visitor().visit_raw(node)
        

    def visit_section(self, node):
        self.section_level += 1
        if self.section_level <= self.max_chunk_level:
            chunk_id = self._chunk_id( node )
            if self.page_subtoc.has_key( chunk_id ):
                node.append( self.page_subtoc[chunk_id] )            

            self.toc = self.toc.next
            self.visitors.append(self._page_translator(
                  self._node_to_document(node)
                , self.docframe
                , self._page_title(self.toc)
                ))

            self.active_visitor().nav_bar = self._nav_bar(self.toc)
            self.active_visitor().body.append(self._header_start())
            self.active_visitor().body.append(self.active_visitor().nav_bar)
            self.active_visitor().body.append(self._page_location(self.toc))
            self.active_visitor().body.append(self._header_end())

        self.active_visitor().visit_section(node)

    def depart_section(self, node):
        self.active_visitor().depart_section(node)
        if self.section_level <= self.max_chunk_level:
            self.active_visitor().body.append(self._footer_start())
            self.active_visitor().body.append(self.active_visitor().nav_bar)
            self.active_visitor().body.append(self._footer_content())
            self.active_visitor().body.append(self._footer_end())
            visitor = self.visitors.pop()
            self.pages[ self._chunk_id( node ) ] = visitor.astext()

        self.section_level -= 1


    def visit_topic(self, node):
        if self._is_toc_node( node ) and not self.in_home_page:
            self.toc_nav_builder = _toc_nav_builder(home=self.home_page)
            self.visitors.append(self._page_translator(
                  self._node_to_document(node)
                , self.tocframe
                , self._page_title(self.full_toc_page)
                ))
            
            if self.settings.no_frames:
                self.active_visitor().nav_bar = self._nav_bar(self.toc)
                self.active_visitor().body.append(self._header_start())
                self.active_visitor().body.append(self.active_visitor().nav_bar)
                self.active_visitor().body.append(self._header_end())
        else:
            self.active_visitor().visit_topic(node)

    def depart_topic(self, node):
        if self._is_toc_node( node ) and not self.in_home_page:
            self.toc_nav_builder = None
            if self.settings.no_frames:
                self.active_visitor().body.append(self._footer_start())
                self.active_visitor().body.append(self.active_visitor().nav_bar)
                self.active_visitor().body.append(self._footer_end())

            tocframe_visitor = self.visitors.pop()
            self.pages[self.full_toc_page.id] = self._toc_as_text( tocframe_visitor )
            self.in_home_page = 1            
                        
            home_page_toc = self.page_subtoc[self.home_page.id]
            if self.settings.no_frames:
                home_page_toc.append(self._build_full_toc_entry())

            home_page_toc.walkabout(self)
            
            self.in_home_page = 0
        else:
            self.active_visitor().depart_topic(node)


    def visit_bullet_list(self, node):
        if self._is_in_main_toc():
            chunk_id = self.toc_nav_builder.last_visited.id
            self.page_subtoc[chunk_id] = self._subtoc(node)
            self.toc_nav_builder.subsection()
            self.section_level += 1
            if self.section_level > self.max_chunk_level:
                self.max_chunk_level = self.section_level

        self.active_visitor().visit_bullet_list(node)

    def depart_bullet_list(self, node):
        self.active_visitor().depart_bullet_list(node)
        if self._is_in_main_toc():
            self.toc_nav_builder.up()
            self.section_level -= 1


    def visit_reference(self, node):
        self._adjust_node_uri(node, 'refuri')
        self.active_visitor().visit_reference(node)

        # formatting fix, temporary here        
        if not isinstance(node.parent, docutils.nodes.TextElement):
            a = self.active_visitor().body[-1]
            self.active_visitor().body.pop()
            self.active_visitor().body.pop()
            self.active_visitor().context[-1] = ''
            self.active_visitor().body.append(a)
        
        if node.has_key('refuri'):
            self.active_visitor().body[-1] = self._add_target_attr(
                  self.active_visitor().body[-1]
                , '_top'
                )

        if node.has_key('refid'):
            anchor = None
            section_id = node.get('refid')

            if self.document.ids.has_key(section_id):
                n = self.document.ids[section_id]
                sections = []
                while n:
                    if isinstance(n, docutils.nodes.section):
                        sections.insert(0, n)
                    n = n.parent
                
                if len(sections) > self.max_chunk_level:
                    anchor = section_id
                    section_id = self._node_id( sections[self.max_chunk_level - 1] )
                elif len(sections):
                    anchor = section_id
                    section_id = self._node_id( sections[-1] )
                
                # if it's a page, don't link to the page's title instead of just the page itself
                if anchor == section_id or anchor.find('id') == 0: # or -> temporary hack!
                    anchor = None
            
            chunk_id = self._make_chunk_id( section_id )
            self.active_visitor().body[-1] = self._replace_href(
                  self._chunk_ref( self._active_chunk_id(), chunk_id, anchor )
                , self.active_visitor().body[-1]
                )

            if self._is_in_main_toc():
                name = self._node_name(node)
                if not name: name = node.astext()
                self.toc_nav_builder.along( chunk_id, name )
    

    def depart_reference(self, node):
        self.active_visitor().depart_reference(node)


    def visit_image(self, node):
        self._adjust_node_uri(node, 'uri')
        self.active_visitor().visit_image(node)
                

    def depart_image(self, node):
        self.active_visitor().depart_image(node)


    def visit_title(self, node):
        if self.section_level == 0 and not self._is_in_main_toc():
            self.document_title = self.encode(node.astext())
        
        self.active_visitor().visit_title(node)
        if node.has_key('refid') and self.toc.up:
            self.active_visitor().body[-1] = self._replace_href(
                  '%s#%s' % ( 
                      self._chunk_ref(self._active_chunk_id(), self.toc.up.id)
                    , node.get('refid')
                    )
                , self.active_visitor().body[-1]
                )

    def depart_title(self, node):
        self.active_visitor().depart_title(node)

    
    def encode(self, text):
        return self.active_visitor().encode(text)
    
    
    def _page_translator(self, document, frame, title=None):
        result = self.page_translator(document)
        result.body_prefix = ['</head>\n<body class="%s">\n' % frame]
        if title:
            result.head.append('<title>%s</title>\n' % title)

        return result
        
    def _subtoc(self, node):
        def _auto_toc_filter(node,root=node):
            return  node != root \
                and isinstance(node, docutils.nodes.bullet_list)

        self._set_node_id(node, 'outline')

        if self._node_class( node ) is None:
            self._set_node_class( node, 'toc' )

        return _filter_tree(
              self._node_to_document(node)
            , _auto_toc_filter
            )


    def _add_target_attr(self, href, target):        
        return self.re_href.sub(r'\1\2\3 target="%s"' % target, href)

    def _replace_href(self, new, old):
        if not self._is_in_main_toc() or self.settings.no_frames:
            return self.re_href.sub(r'\1%s\3' % new, old)
        else:
            return self.re_href.sub(
                  r'\1%s\3 target="%s"' % (new, self.docframe)
                , old
                )

    def _header_start(self):
        return '<table class="header"><tr class="header">'
       
    def _header_end(self):
        return '</tr></table><div class="header-separator"></div>\n'


    def _footer_start(self):
        return '\n<div class="footer-separator"></div>\n<table class="footer"><tr class="footer">'
       
    def _footer_content(self):
        return '' 

    def _footer_end(self):
        return '</tr></table>'


    def _nav_bar(self, toc_node):
        nav_bar = [
              self._nav_group(['Prev', 'Next'],  [toc_node.prev, toc_node.next])
            , self._nav_group(['Back', 'Along'], [toc_node.back, toc_node.along])
            , self._nav_group(['Up',   'Home'],  [toc_node.up, self.home_page])
            ]
        
        if self.settings.no_frames:
            nav_bar.append(self._nav_group([self.full_toc_page.name], [self.full_toc_page]))   
        
        return '<td class="header-group navigation-bar">%s</td>\n' \
            % '<span class="navigation-group-separator">&nbsp;|&nbsp;</span>'.join(nav_bar)

    
    def _page_location(self, toc_node):
        result = ''
        while toc_node:
            page_link = self._toc_node_link(toc_node.name, toc_node)
            if result:  result = '%s / %s' % (page_link, result)
            else:       result = page_link
            toc_node = toc_node.up
            
        return '<td class="header-group page-location">%s</td>\n' % result
    

    def _page_title(self, toc_node):
        result = toc_node.name
        level = 0
        while toc_node.up:
            toc_node = toc_node.up
            level +=1 

        if level > 0: result = '%s: %s' % (self.document_title, result)        
        return result
    
    
    def _nav_group(self, labels, nodes):
        return '<span class="navigation-group">%s</span>' % '&nbsp;'.join(
              map(lambda l,n: self._toc_node_link(l, n), labels, nodes)
            )
    
    
    def _node_to_document(self, node):
        node.settings = self.settings.copy()
        if not self.settings.embed_stylesheet and _is_uri_relative( self.settings.stylesheet_path ):
            node.settings.stylesheet_path = '../%s' % self.settings.stylesheet_path
        node.reporter = self.reporter
        return node

    def _adjust_node_uri(self, node, attr):
        if node.has_key(attr):
            src_uri = node[attr]
            if _is_uri_relative(src_uri):
                node[attr] = '../%s' % src_uri    
    
    def _toc_node_link(self, name, toc_node):
        if not toc_node: return name
        return '<a href="%s" class="navigation-link">%s</a>' % ( 
              self._chunk_ref(self._active_chunk_id(), toc_node.id)
            , name
            )
    
    def _build_full_toc_entry(self):
        reference = docutils.nodes.reference('', self.full_toc_page.name, refid=self.full_toc_page.id)
        entry = docutils.nodes.paragraph('', '', reference)
        item = docutils.nodes.list_item('', entry)
        return item


    def _chunk_id( self, node ):
        return self._make_chunk_id( self._node_id( node ) )


    def _chunk_ref(self, source_id, target_id, anchor=None):
        prefix = './'
        if self.settings.no_frames:
            if target_id == self.home_page.id and source_id != target_id:
                prefix = '../'
            elif source_id == self.home_page.id:
                prefix = './%s/' % self.page_files_dir

            if target_id == self.home_page.id:
                target_id = self.index_file

        elif source_id is None:
            prefix = './%s/' % self.page_files_dir
        
        if not anchor:
            return '%s%s%s' % (prefix, target_id, self.extension)
        else:
            return '%s%s%s#%s' % (prefix, target_id, self.extension, anchor)


    def _active_chunk_id(self):
        return self._chunk_id( self.active_visitor().document )


    def _make_chunk_id( self, node_id ):
        return node_id


    def _is_in_main_toc(self):
        return self.toc_nav_builder


    def _is_toc_node( self, node ):
        return self._node_class( node ) == 'contents'    

    def _toc_as_text( self, visitor ):
        return visitor.astext() 


    def _node_id( self, node ):
        return node['ids'][0]

    def _set_node_id( self, node, id ):
        node['ids'] = [ id ]

    def _node_class( self, node ):
        if len( node['classes'] ):
            return node['classes'][0]
        return None

    def _set_node_class( self, node, class_ ):
        node['classes'] = [ class_ ]

    def _node_name( self, node ):
        if len( node['names'] ):
            return node['names'][0]
        return None


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
        self.last_visited = _toc_entry(id=None, name=None)
        self.last_visited.up = self.last_visited.prev = parent
        parent.next = self.last_visited
        self.last_sibling = self.last_visited

    def along( self, chunk_id, name ):
        last = self.last_visited
        if last.id:
            self.last_visited = _toc_entry( chunk_id, name )
            self.last_visited.prev = last
            self.last_visited.back = self.last_sibling
            self.last_visited.up = self.last_sibling.up
            last.next = self.last_sibling.along = self.last_visited
            self.last_sibling = self.last_visited
        else:
            self.last_visited.id = chunk_id
            self.last_visited.name = name

    def up(self):
        self.last_sibling = self.last_sibling.up


class _toc_entry:
    def __init__(self, id, name):
        self.id = id
        self.name = name
        self.prev = self.next = self.back = self.along = self.up = None
