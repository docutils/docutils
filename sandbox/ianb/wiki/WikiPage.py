import os, re, stat, time
from docutils import core, io
from cgi import escape
from docutils import readers
import sys

## All the Wiki pages will be kept in this directory:
pageDir = '/usr/home/ianb/prog/WebwareWiki/pages'

class WikiPage:

    def __init__(self, pageName):
        self.name = pageName

    def basePath(self, name=None):
        """Returns the base path (sans extension) for this page,
        or the page named by ``name``"""
        if name is None: name = self.name
        return os.path.join(pageDir, name)

    def exists(self, name=None):
        """Does this page (or page named by ``name``) exist?"""
        return os.path.exists(self.basePath(name) + ".html")

    def html(self):
        """Returns text of HTML for page (HTML fragment only)"""
        if self.exists():
            html = open(self.basePath() + ".html").read()
            html = self._subWikiLinks(html)
            return html
        else:
            return 'This page has not yet been created.'

    def previewHTML(self, text):
        """Returns an HTML preview of the text"""
        return self._subWikiLinks(self._convertText(text))

    _wikiLinkRE = re.compile(r'(<a [^>]* href=")!(.*?)("[^>]*>)(.*?)(</a>)',
                             re.I+re.S)
    def _subWikiLinks(self, text):
        return self._wikiLinkRE.sub(self._subLink, ' %s ' % text)

    def _subLink(self, match):
        if self.exists(match.group(2)):
            return match.group(1) + match.group(2) + match.group(3) + match.group(4) + match.group(5)
        else:
            return '<span class="nowiki">%s%s%s%s?%s</span>' \
                   % (match.group(4), match.group(1), match.group(2),
                      match.group(3), match.group(5))

    def text(self):
        """Returns (reST) text for page"""
        if self.exists():
            return open(self.basePath() + ".txt").read()
        else:
            return ''

    def searchMatches(self, text):
        return self.searchTitleMatches(text) \
               or self.text().lower().find(text.lower()) != -1

    def searchTitleMatches(self, text):
        return self.title().lower().find(text.lower()) != -1

    def setText(self, text):
        """Sets the text for the page (and updates cached HTML at the
        same time)"""
        f = open(self.basePath() + ".txt", 'w')
        f.write(text)
        f.close()
        f = open(self.basePath() + ".html", 'w')
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

    def modifiedDate(self):
        """Date modified (integer timestamp)"""
        return os.stat(self.basePath() + ".txt")[stat.ST_MTIME]

    def modifiedDateText(self):
        """Text representation of modified date"""
        return time.strftime("%a %m/%d/%y", time.gmtime(self.modifiedDate()))

    def title(self):
        return self.name


def allPages():
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



########################################
## reST-specific stuff
########################################


from docutils import nodes
from docutils.readers import standalone
from docutils.transforms import Transform

class WikiLinkResolver(nodes.SparseNodeVisitor):

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

    default_priority = 800

    def apply(self):
        visitor = WikiLinkResolver(self.document)
        self.document.walk(visitor)
        
class Reader(standalone.Reader):

    supported = standalone.Reader.supported + ('wiki',)

    default_transforms = standalone.Reader.default_transforms \
                         + (WikiLink,)

