# 
# $Id$
#
''' ReStructuredText Product for Zope

This Product stores two texts - a "source" text in ReStructureText format,
and a HTML "formatted" version of that text.

'''
from docutils import core, io

from Globals import InitializeClass, DTMLFile
from OFS.SimpleItem import Item
from OFS.PropertyManager import PropertyManager
from Acquisition import Implicit
from Persistence import Persistent
from AccessControl import ClassSecurityInfo
from AccessControl import ModuleSecurityInfo
from DateTime.DateTime import DateTime
modulesecurity = ModuleSecurityInfo()

modulesecurity.declareProtected('View management screens',
    'manage_addZReSTForm')
manage_addZReSTForm = DTMLFile('dtml/manage_addZReSTForm', globals())

modulesecurity.declareProtected('Add Z Roundups', 'manage_addZReST')
def manage_addZReST(self, id, title='', file='', REQUEST=None):
    """Add a ZReST product """
    # validate the instance_home
    self._setObject(id, ZReST(id, title))
    self._getOb(id).manage_upload(file)
    if REQUEST:
        return self.manage_main(self, REQUEST)

class ZReST(Item, PropertyManager, Implicit, Persistent):
    '''An instance of this class provides an interface between Zope and
       ReStructuredText for one text.
    '''
    meta_type =  'ReStructuredText Document'
    security = ClassSecurityInfo()

    def __init__(self, id, title):
        self.id = id
        self.title = title
        self.source = self.formatted = ''

    # define the tabs for the management interface
    manage_options= ( {'label': 'Edit', 'action':'manage_main'},
                      {'label': 'View', 'action':'index_html'},
                      {'label': 'Source', 'action':'source_txt'},
                    ) + Item.manage_options

    # access to the source text and formatted text
    security.declareProtected('View', 'index_html')
    def index_html(self, REQUEST=None):
        ''' Getting the formatted text
        '''
        REQUEST.RESPONSE.setHeader('content-type', 'text/html')
        return self.formatted
    security.declareProtected('View', 'source_txt')
    def source_txt(self, REQUEST=None):
        ''' Getting the source text
        '''
        REQUEST.RESPONSE.setHeader('content-type', 'text/plain')
        return self.source

    # edit form, which is also the primary interface
    security.declareProtected('Edit ReStructuredText', 'manage_editForm')
    manage_main = DTMLFile('dtml/manage_editForm', globals())

    # edit action
    security.declareProtected('Edit ReStructuredText', 'manage_edit')
    def manage_edit(self, data, title, SUBMIT='Change',dtpref_cols='50',
                    dtpref_rows='20', REQUEST=None):
        '''Alias index_html to roundup's index
        '''
        self.title=str(title)
        if self._size_changes.has_key(SUBMIT):
            return self._er(data,title,SUBMIT,dtpref_cols,dtpref_rows,REQUEST)
        if data != self.source:
            self.source = data
            self.render()

        if REQUEST:
            message="Saved changes."
            return self.manage_main(self, REQUEST, manage_tabs_message=message)

    # handle edit window size changes
    _size_changes = {
        'Bigger': (5,5),
        'Smaller': (-5,-5),
        'Narrower': (0,-5),
        'Wider': (0,5),
        'Taller': (5,0),
        'Shorter': (-5,0),
    }
    def _er(self,data,title,SUBMIT,dtpref_cols,dtpref_rows,REQUEST):
        dr,dc = self._size_changes[SUBMIT]
        rows=max(1,int(dtpref_rows)+dr)
        cols=max(40,int(dtpref_cols)+dc)
        e=(DateTime('GMT') + 365).rfc822()
        resp=REQUEST['RESPONSE']
        resp.setCookie('dtpref_rows',str(rows),path='/',expires=e)
        resp.setCookie('dtpref_cols',str(cols),path='/',expires=e)
        return self.manage_main(
            self,REQUEST,title=title,__str__=self.quotedHTML(data),
            dtpref_cols=cols,dtpref_rows=rows)
    security.declarePrivate('quotedHTML')
    def quotedHTML(self,
                   text=None,
                   character_entities=(
                       (('&'), '&amp;'),
                       (("<"), '&lt;' ),
                       ((">"), '&gt;' ),
                       (('"'), '&quot;'))): #"
        if text is None: text=self.read_raw()
        for re,name in character_entities:
            if text.find(re) >= 0: text=name.join(text.split(re))
        return text


    # handle uploads too
    security.declareProtected('Edit ReStructuredText', 'manage_upload')
    def manage_upload(self, file='', REQUEST=None):
        ''' Replaces the current source with the upload.
        '''
        if isinstance(file, type('')):
            self.source = file
        else:
            self.source = file.read()

        if REQUEST:
            message="Saved changes."
            return self.manage_main(self, REQUEST, manage_tabs_message=message)

    security.declarePrivate('render')
    def render(self):
        ''' Render the source to HTML
        '''
        # format with strings
        pub = core.Publisher()
        pub.set_reader('standalone', None, 'restructuredtext')
        pub.set_writer('html')

        # go with the defaults
        pub.set_options()

        # this is needed, but doesn't seem to do anything
        pub.options._destination = ''

        # input
        pub.source = io.StringInput(pub.options)
        pub.source.source = self.source

        # output - not that it's needed
        pub.destination = io.StringOutput(pub.options)

        # parse!
        document = pub.reader.read(pub.source, pub.parser, pub.options)

        # do the format
        self.formatted = pub.writer.write(document, pub.destination)

    def __str__(self):
        ''' Stringfy .. return the source
        '''
        return self.quotedHTML(self.source)


InitializeClass(ZReST)
modulesecurity.apply(globals())


#
# $Log$
# Revision 1.1  2002/08/14 05:15:37  richard
# Zope ReStructuredText Product
#
#
#
# vim: set filetype=python ts=4 sw=4 et si
