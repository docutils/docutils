"""
The Wiki module primarily exports the `WikiPage` class:
"""

import os, re, time
from docutils import core, io
from docutils import readers

__all__ = ['WikiPage', 'allPages', 'recentPages',
           'searchTitles', 'search', 'css']

## All the Wiki pages will be kept in this directory:
pageDir = '/usr/home/ianb/w/pypaper/pages/'

class WikiPage(object):
    """
    WikiPage is a class to represent one page in a WikiWikiWeb [#]_.
    The page may or may not yet exist -- that is, it may not yet
    have content.

    .. [#] http://c2.com/cgi-bin/wiki

    It has the following properties and methods:

        `html`:
            A read-only property giving the HTML for the page.
            If the page does not yet have content the text
            ``"This page has not yet been created"`` is returned.
        `text`:
            The text for the page.  To save new text, simply
            assign to this property.
        `title`:
            The title of the page.
        `name`:
            The name of the page -- a canonical identifier.
            Related to the title, but not necessarily the
            same.
        .. ignore: html
        .. ignore: text
        .. ignore: setText
        .. ignore: title

    """

    def __init__(self, pageName):
        """
        Each page has a name, which is a unique identifier, for example
        ``"FrontPage"``, which identifies the page in the URL and
        for linking.
        """
        self.name = pageName

    def basePath(self):
        """
        :Ignore: yes
        Returns the base path (sans extension) for this page
        """
        return _basePath(self.name)

    basePath = property(basePath)

    def exists(self):
        """Does this page have content yet?"""
        return _exists(self.name)

    def html(self):
        """Returns text of HTML for page (HTML fragment only)"""
        if self.exists():
            html = open(self.basePath + ".html").read()
            html = self._subWikiLinks(html)
            return html
        else:
            return 'This page has not yet been created.'

    html = property(html)

    def preview(self, text):
        """Returns an HTML preview of the text"""
        return self._subWikiLinks(self._convertText(text))

    _wikiLinkRE = re.compile(r'(<a [^>]* href=")!(.*?)("[^>]*>)(.*?)(</a>)',
                             re.I+re.S)
    
    def _subWikiLinks(self, text):
        return self._wikiLinkRE.sub(self._subLink, ' %s ' % text)

    def _subLink(self, match):
        if _exists(match.group(2)):
            return match.group(1) + match.group(2) + match.group(3) + match.group(4) + match.group(5)
        else:
            return '<span class="nowiki">%s%s%s%s?%s</span>' \
                   % (match.group(4), match.group(1), match.group(2),
                      match.group(3), match.group(5))

    def text(self):
        """
        The text of the page.  ReStructuredText is used, though the
        parsing is internal to the module.  You can assign to this
        property to save new text for the page.
        """
        if self.exists():
            return open(self.basePath + ".txt").read()
        else:
            return ''

    def setText(self, text):
        """Sets the text for the page (and updates cached HTML at the
        same time)"""
        f = open(self.basePath + ".txt", 'w')
        f.write(text)
        f.close()
        f = open(self.basePath + ".html", 'w')
        f.write(self._convertText(text))
        f.close()

    def _convertText(self, text):
        return self._cleanHTML(core.publish_string(
            source=text,
            reader=Reader(),
            parser_name='restructuredtext',
            writer_name='html'))

    def _cleanHTML(self, html):
        return html[html.find('<body>'):html.find('</body>')]

    text = property(text, setText)

    def searchMatches(self, text):
        """
        :Ignore: yes
        """
        return self.searchTitleMatches(text) \
               or self.text().lower().find(text.lower()) != -1

    def searchTitleMatches(self, text):
        """
        :Ignore: yes
        """
        return self.title().lower().find(text.lower()) != -1

    def modifiedDate(self):
        """Date modified (integer timestamp)"""
        return os.stat(self.basePath + ".txt").st_mtime

    modifiedDate = property(modifiedDate)

    def modifiedDateText(self):
        """Text representation of modified date"""
        return time.strftime("%a %m/%d/%y", time.gmtime(self.modifiedDate()))

    modifiedDateText = property(modifiedDateText)

    def title(self):
        """Page title"""
        return self.name
        
    title = property(title)

"""
Methods for searching the wiki pages:
"""

def allPages():
    """All pages with content in the system"""
    return [WikiPage(page[:-4])
            for page in os.listdir(pageDir)
            if page.endswith('.txt')]
    
def recentPages():
    """All pages, sorted by date modified, most recent first"""
    pages = allPages()
    pages.sort(lambda a, b: cmp(b.modifiedDate(), a.modifiedDate()))
    return pages

def searchTitles(text):
    """Search page titles for ``text``, returning list of pages"""
    return [page for page in allPages()
            if page.searchTitleMatches(text)]

def search(text):
    """Search titles and bodies of pages for ``text``, returning list
    of pages"""
    return [page for page in allPages()
            if page.searchMatches(text)]


def _basePath(name):
    return os.path.join(pageDir, name)

def _exists(name):
    return os.path.exists(_basePath(name) + ".html")

"""
There is one module global to be printed at the top of
every Wiki page:

    `css`:
        The HTML to put the proper CSS at the top of the page.  This
        should be put in the ``<head>`` section of the page.
"""

try:
    f = open('default.css')
except IOError:
    css = ""
else:
    css = '<style type="text/css">\n%s</style>\n' % f.read()
    f.close()

########################################
## reST-specific stuff
########################################


from docutils import nodes
from docutils.readers import standalone
from docutils.transforms import Transform

class WikiLinkResolver(nodes.SparseNodeVisitor):
    ":Ignore: yes"

    def visit_reference(self, node):
        if node.resolved or not node.hasattr('refname'):
            return
        refname = node['refname']
        node.resolved = 1
        node['class'] = 'wiki'
        # I put a ! here to distinguish Wiki links from other
        # links -- Wiki links have to be fixed up at view time,
        # to distinguish between dangling and resolved Wiki
        # links.
        node['refuri'] = '!' + refname
        del node['refname']

class WikiLink(Transform):
    ":Ignore: yes"

    default_priority = 800

    def apply(self):
        visitor = WikiLinkResolver(self.document)
        self.document.walk(visitor)
        
class Reader(standalone.Reader):
    ":Ignore: yes"

    supported = standalone.Reader.supported + ('wiki',)

    def get_transforms(self):
        return standalone.Reader.get_transforms(self) + [WikiLink]
