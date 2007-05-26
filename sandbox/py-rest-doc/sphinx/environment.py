# -*- coding: utf-8 -*-
"""
    sphinx.environment
    ~~~~~~~~~~~~~~~~~~

    Global creation environment.

    :copyright: 2007 by Georg Brandl.
    :license: Python license.
"""

import heapq
import difflib
import itertools
import cPickle as pickle
from os import path
from string import uppercase

from docutils import nodes
from docutils.io import FileInput
from docutils.core import publish_doctree
from docutils.readers import standalone
from docutils.transforms import Transform
from docutils.transforms.parts import ContentsFilter

from . import addnodes

default_settings = {
    'embed_stylesheet': False,
    'cloak_email_addresses': True,
    'pep_base_url': 'http://www.python.org/dev/peps/',
    'input_encoding': 'utf-8',
    'doctitle_xform': False,
    'sectsubtitle_xform': False,
}

# This is increased every time a new environment attribute is added
# to properly invalidate pickle files.
ENV_VERSION = 6


def walk_depth(node, depth, maxdepth):
    """Utility: Cut a TOC at a specified depth."""
    for subnode in node.children[:]:
        if isinstance(subnode, (addnodes.compact_paragraph, nodes.list_item)):
            walk_depth(subnode, depth, maxdepth)
        elif isinstance(subnode, nodes.bullet_list):
            if depth > maxdepth:
                subnode.parent.replace(subnode, [])
            else:
                walk_depth(subnode, depth+1, maxdepth)


default_substitutions = set([
    'version',
    'release',
    'today',
])


class DefaultSubstitutions(Transform):
    """
    Replace some substitutions if they aren't defined in the document.
    """
    # run before the default Substitutions
    default_priority = 210

    def apply(self):
        # only handle those not otherwise defined in the document
        to_handle = default_substitutions - set(self.document.substitution_defs)
        for ref in self.document.traverse(nodes.substitution_reference):
            refname = ref['refname']
            if refname in to_handle:
                text = self.document.settings.env.builder.config.get(refname, '')
                ref.replace_self(nodes.Text(text, text))


class MoveModuleTargets(Transform):
    """
    Move module targets to their nearest enclosing section title.
    """
    default_priority = 210

    def apply(self):
        for node in self.document.traverse(nodes.target):
            if not node['ids']:
                continue
            if node['ids'][0].startswith('module-') and \
                   node.parent.__class__ is nodes.section:
                node.parent['ids'] = node['ids']
                node.parent.remove(node)


class MyStandaloneReader(standalone.Reader):
    """
    Add our own Substitutions transform.
    """
    def get_transforms(self):
        tf = standalone.Reader.get_transforms(self)
        return tf + [DefaultSubstitutions, MoveModuleTargets]


class MyContentsFilter(ContentsFilter):
    """
    Used with BuildEnvironment.add_toc_from() to discard cross-file links
    within table-of-contents link nodes.
    """
    def visit_pending_xref(self, node):
        self.parent.append(nodes.literal(node['reftarget'], node['reftarget']))
        raise nodes.SkipNode


class BuildEnvironment:
    """
    The environment in which the ReST files are translated.
    Stores an inventory of cross-file targets and provides doctree
    transformations to resolve links to them.

    Not all doctrees are stored in the environment, only those of files
    containing a "toctree" directive, because they have to change if sections
    are edited in other files. This keeps the environment size moderate.
    """

    @staticmethod
    def frompickle(filename, builder):
        picklefile = open(filename, 'rb')
        env = pickle.load(picklefile)
        picklefile.close()

        if env.version != ENV_VERSION:
            raise IOError('env version not current')

        env.builder = builder
        return env

    def topickle(self, filename):
        # remove unpicklable attributes
        builder = self.builder
        self.builder = None
        wstream = self.warning_stream
        self.set_warning_stream(None)
        picklefile = open(filename, 'wb')
        pickle.dump(self, picklefile, pickle.HIGHEST_PROTOCOL)
        picklefile.close()
        # reset stream
        self.set_warning_stream(wstream)
        self.builder = builder

    def __init__(self, builder):
        self.builder = builder
        self.settings = default_settings.copy()
        self.settings['env'] = self
        self.warning_stream = None

        # this is to invalidate old pickles
        self.version = ENV_VERSION

        # Build times -- to determine changed files
        # Also use this as an inventory of all existing and built filenames.
        self.mtimes = {}            # filename -> mtime at the time of build

        # File metadata
        self.metadata = {}          # filename -> dict of metadata items

        # TOC inventory
        self.titles = {}            # filename -> title node
        self.tocs = {}              # filename -> table of contents nodetree
        self.toc_num_entries = {}   # filename -> number of real entries
                                    # used to determine when to show the TOC in a sidebar
                                    # (don't show if it's only one item)
        self.toctree_children = {}  # filename -> included filenames in a toctree
        self.toctree_relations = {} # filename -> ["parent", "previous", "next"] filename
                                    # for navigating in the toctree
        self.toctree_doctrees = {}  # filenames with toctrees -> doctree of the file
        self.files_to_rebuild = {}  # filename -> list of files (containing its TOCs)
                                    # to rebuild too

        # x-ref target inventory
        self.descrefs = {}          # fullname -> filename, desctype
        self.filemodules = {}       # filename -> [modules]
        self.modules = {}           # modname -> filename, synopsis, platform
        self.tokens = {}            # tokenname -> filename
        self.labels = {}            # labelname -> filename, labelid

        self.indexentries = {}      # filename -> list of
                                    # (type, string, target, aliasname)
        self.versionchanges = {}    # version -> list of
                                    # (type, filename, module, descname, content)

        # These are set while parsing a file
        self.filename = None        # current file name
        self.currmodule = None      # current module name
        self.currclass = None       # current class name
        self.currdesc = None        # current descref name
        self.index_num = 0          # autonumber for index targets

    def set_warning_stream(self, stream):
        self.warning_stream = stream
        self.settings['warning_stream'] = stream

    def clear_file(self, filename):
        """Remove all traces of a source file in the inventory."""
        if filename in self.mtimes:
            self.mtimes.pop(filename, None)
            self.titles.pop(filename, None)
            self.tocs.pop(filename, None)
            self.toctree_children.pop(filename, None)
            self.toctree_doctrees.pop(filename, None)
            self.files_to_rebuild.pop(filename, None)

            for fullname, (fn, _) in self.descrefs.items():
                if fn == filename:
                    del self.descrefs[fullname]
            for modname, (fn, _, _) in self.modules.items():
                if fn == filename:
                    del self.modules[modname]
            self.filemodules.pop(filename, None)
            for tokenname, fn in self.tokens.items():
                if fn == filename:
                    del self.tokens[tokenname]
            for labelname, (fn, _, _) in self.labels.items():
                if fn == filename:
                    del self.labels[labelname]
            self.indexentries.pop(filename, None)
            for version, changes in self.versionchanges.items():
                new = [change for change in changes if change[1] != filename]
                changes[:] = new

    # --------- SINGLE FILE BUILDING -------------------------------------------

    def update_file(self, filename):
        """Parse a file and add/update inventory entries for the doctree."""
        self.clear_file(filename)
        self.filename = filename
        filesystem_filename = path.join(self.builder.srcdir, filename)
        doctree = publish_doctree(None, filesystem_filename, FileInput,
                                  settings_overrides=self.settings,
                                  reader=MyStandaloneReader())
        self.remove_system_messages(doctree)
        self.process_metadata(filename, doctree)
        self.create_title_from(filename, doctree)
        self.note_labels_from(filename, doctree)
        self.build_toc_from(filename, doctree)
        if filename in self.toctree_doctrees:
            tree = doctree.deepcopy()
            # make it picklable
            tree.reporter = None
            tree.transformer = None
            tree.settings.warning_stream = None
            self.toctree_doctrees[filename] = tree
        self.mtimes[filename] = path.getmtime(filesystem_filename)
        # cleanup
        self.filename = None
        self.currmodule = None
        self.currclass = None
        return doctree

    def process_metadata(self, filename, doctree):
        """
        Process the docinfo part of the doctree as metadata.
        """
        self.metadata[filename] = md = {}
        docinfo = doctree[0]
        if docinfo.__class__ is not nodes.docinfo:
            # nothing to see here
            return
        for node in docinfo:
            if node.__class__ is nodes.author:
                # handled specially by docutils
                md['author'] = node.astext()
            elif node.__class__ is nodes.field:
                name, body = node
                md[name.astext()] = body.astext()
        print md
        del doctree[0]

    def remove_system_messages(self, doctree):
        """
        Remove info-level system messages from the doctree.
        This is necessary because the transform doing this otherwise
        is not active for our setup.
        """
        for node in doctree.traverse(nodes.system_message):
            if node['level'] < 2:
                node.parent.remove(node)

    def create_title_from(self, filename, document):
        """Add a title node to the document (just copy the first section title)."""
        for node in document.traverse(nodes.section):
            titlenode = nodes.title()
            visitor = MyContentsFilter(document)
            node[0].walkabout(visitor)
            titlenode += visitor.get_entry_text()
            self.titles[filename] = titlenode
            return

    def note_labels_from(self, filename, document):
        for name, explicit in document.nametypes.iteritems():
            if not explicit:
                continue
            labelid = document.nameids[name]
            node = document.ids[labelid]
            if not isinstance(node, nodes.section):
                # e.g. desc-signatures
                continue
            sectname = node[0].astext() # node[0] == title node
            if name in self.labels:
                print >>self.warning_stream, \
                      ('WARNING: duplicate label %s, ' % name +
                       'in %s and %s' % (self.labels[name][0], filename))
            self.labels[name] = filename, labelid, sectname

    def note_toctree(self, filename, toctreenode):
        """Note a TOC tree directive in a document and gather information about
           file relations from it."""
        includefiles = toctreenode['includefiles']
        includefiles_len = len(includefiles)
        for i, includefile in enumerate(includefiles):
            # the "previous" file for the first toctree item is the parent
            previous = includefiles[i-1] if i > 0 else filename
            # the "next" file for the last toctree item is the parent again
            next = includefiles[i+1] if i < includefiles_len-1 else filename
            self.toctree_relations[includefile] = [filename, previous, next]

        self.toctree_children.setdefault(filename, []).extend(includefiles)

    def build_toc_from(self, filename, document):
        """Build a TOC from the doctree and store it in the inventory."""
        numentries = [0] # nonlocal again...

        def build_toc(node):
            entries = []
            for subnode in node:
                if isinstance(subnode, addnodes.toctree):
                    # just copy the toctree node which is then resolved
                    # in self.resolve_toctrees
                    item = subnode.copy()
                    entries.append(item)
                    # do the inventory stuff
                    self.note_toctree(filename, subnode)
                    continue
                if not isinstance(subnode, nodes.section):
                    continue
                title = subnode[0]
                # copy the contents of the section title, but without references
                # and unnecessary stuff
                visitor = MyContentsFilter(document)
                title.walkabout(visitor)
                nodetext = visitor.get_entry_text()
                if not numentries[0]:
                    # for the very first toc entry, don't add an anchor
                    # as it is the file's title anyway
                    anchorname = ''
                else:
                    anchorname = '#' + subnode['ids'][0]
                reference = nodes.reference('', '', refuri=filename,
                                            anchorname=anchorname,
                                            *nodetext)
                para = addnodes.compact_paragraph('', '', reference)
                item = nodes.list_item('', para)
                item += build_toc(subnode)
                numentries[0] += 1
                entries.append(item)
            if entries:
                return nodes.bullet_list('', *entries)
            return []
        toc = build_toc(document)
        if toc:
            self.tocs[filename] = toc
        else:
            self.tocs[filename] = nodes.bullet_list('')
        self.toc_num_entries[filename] = numentries[0]

    def get_toc_for(self, filename):
        """Return a TOC nodetree -- for use on the same page only!"""
        toc = self.tocs[filename].deepcopy()
        for node in toc.traverse(nodes.reference):
            node['refuri'] = node['anchorname']
        return toc

    # -------
    # these are called from docutils directives and therefore use self.filename
    #
    def note_descref(self, fullname, desctype):
        if fullname in self.descrefs:
            print >>self.warning_stream, \
                  ('WARNING: duplicate canonical description name %s, ' % fullname +
                   'in %s and %s' % (self.descrefs[fullname][0], self.filename))
        self.descrefs[fullname] = (self.filename, desctype)

    def note_module(self, modname, synopsis, platform):
        self.modules[modname] = (self.filename, synopsis, platform)
        self.filemodules.setdefault(self.filename, []).append(modname)

    def note_token(self, tokenname):
        self.tokens[tokenname] = self.filename

    def note_toctree_file(self):
        self.toctree_doctrees[self.filename] = None

    def note_index_entry(self, type, string, targetid, aliasname):
        self.indexentries.setdefault(self.filename, []).append(
            (type, string, targetid, aliasname))

    def note_versionchange(self, type, version, node):
        self.versionchanges.setdefault(version, []).append(
            (type, self.filename, self.currmodule, self.currdesc, node.deepcopy()))
    # -------

    # --------- GLOBAL BUILDING ------------------------------------------------

    def resolve_references(self, doctree, docfilename):
        for node in doctree.traverse(addnodes.pending_xref):
            contnode = node[0].deepcopy()
            newnode = None

            typ = node['reftype']
            target = node['reftarget']
            modname = node['modname']
            clsname = node['classname']

            if typ == 'ref':
                filename, labelid, sectname = self.labels.get(target, ('','',''))
                if not filename:
                    newnode = doctree.reporter.system_message(
                        2, 'undefined label: %s' % target)
                    print >>self.warning_stream, \
                          '%s: undefined label: %s' % (docfilename, target)
                else:
                    newnode = nodes.reference('', '')
                    if filename == docfilename:
                        newnode['refid'] = labelid
                    else:
                        newnode['refuri'] = self.builder.get_relative_uri(
                            docfilename, filename) + '#' + labelid
                    newnode.append(nodes.emphasis(sectname, sectname))
            elif typ == 'token':
                filename = self.tokens.get(target, '')
                if not filename:
                    newnode = contnode
                else:
                    newnode = nodes.reference('', '')
                    if filename == docfilename:
                        newnode['refid'] = 'grammar-token-' + target
                    else:
                        newnode['refuri'] = self.builder.get_relative_uri(
                            docfilename, filename) + '#grammar-token-' + target
                    newnode.append(contnode)
            elif typ == 'mod':
                filename, synopsis, platform = self.modules.get(target, ('','',''))
                # just link to an anchor if there are multiple modules in one file
                # because the anchor is generally below the heading which is ugly
                # but can't be helped easily
                anchor = ''
                if not filename or filename == docfilename:
                    # don't link to self
                    newnode = contnode
                else:
                    if len(self.filemodules[filename]) > 1:
                        anchor = '#' + 'module-' + target
                    newnode = nodes.reference('', '')
                    newnode['refuri'] = (
                        self.builder.get_relative_uri(docfilename, filename) + anchor)
                    newnode.append(contnode)
            else:
                name, desc = self.find_desc(modname, clsname, target, typ)
                if not desc:
                    newnode = contnode
                else:
                    newnode = nodes.reference('', '')
                    if desc[0] == docfilename:
                        newnode['refid'] = name
                    else:
                        newnode['refuri'] = (
                            self.builder.get_relative_uri(docfilename, desc[0])
                            + '#' + name)
                    newnode.append(contnode)

            if newnode:
                node.replace_self(newnode)

    def resolve_toctrees(self, documents):
        # determine which files (containing a toc) must be rebuilt for each
        # target file (basically all "parent" files)
        for filename in self.tocs:
            relation = self.toctree_relations.get(filename)
            while relation:
                self.files_to_rebuild.setdefault(filename, set()).add(relation[0])
                relation = self.toctree_relations.get(relation[0])

        # resolve all toctree nodes from TOCs
        def entries_from_toctree(toctreenode):
            """Return TOC entries for a toctree node."""
            includefiles = map(str, toctreenode['includefiles'])

            entries = []
            for includefile in includefiles:
                toc = self.tocs[includefile].deepcopy()
                for toctreenode in toc.traverse(addnodes.toctree):
                    toctreenode.parent.replace_self(
                        entries_from_toctree(toctreenode))
                entries.append(toc)
            if entries:
                return addnodes.compact_paragraph('', '', *entries)
            return []

        for filename in self.toctree_doctrees:
            if filename in documents:
                document = documents[filename]
            else:  # cached
                document = self.toctree_doctrees[filename]

            for toctreenode in document.traverse(addnodes.toctree):
                maxdepth = toctreenode.get('maxdepth', -1)
                newnode = entries_from_toctree(toctreenode)
                # prune the tree to maxdepth
                if maxdepth > 0:
                    walk_depth(newnode, 1, maxdepth)
                toctreenode.replace_self(newnode)

            # set the target paths in the toctrees (they are not known
            # at TOC generation time)
            for node in document.traverse(nodes.reference):
                if node.has_key('anchorname'):
                    # a TOC reference
                    node['refuri'] = self.builder.get_relative_uri(
                        filename, node['refuri']) + node['anchorname']

    def create_index(self):
        new = {}

        def add_entry(word, subword, dic=new):
            entry = dic.get(word)
            if not entry:
                dic[word] = entry = [[], {}]
            if subword:
                add_entry(subword, '', dic=entry[1])
            else:
                entry[0].append(self.builder.get_relative_uri('genindex.rst', fn)
                                + '#' + tid)

        for fn, entries in self.indexentries.iteritems():
            for type, string, tid, alias in entries:
                if type in ('single', 'ssingle'):
                    entry, _, subentry = string.partition('!')
                    add_entry(entry, subentry)
                elif type == 'pair':
                    first, second = map(lambda x: x.strip(), string.split(';', 1))
                    add_entry(first, second)
                    add_entry(second, first)
                elif type == 'triple':
                    first, second, third = map(lambda x: x.strip(), string.split(';', 2))
                    add_entry(first, second+' '+third)
                    add_entry(second, third+', '+first)
                    add_entry(third, first+' '+second)
                elif type == 'quadruple':
                    first, second, third, fourth = \
                           map(lambda x: x.strip(), string.split(';', 3))
                    add_entry(first, '%s %s %s' % (second, third, fourth))
                    add_entry(second, '%s %s, %s' % (third, fourth, first))
                    add_entry(third, '%s, %s %s' % (fourth, first, second))
                    add_entry(fourth, '%s %s %s' % (first, second, third))
                elif type in ('module', 'keyword', 'operator', 'object',
                              'exception', 'statement'):
                    add_entry(string, type)
                    add_entry(type, string)
                elif type == 'builtin':
                    add_entry(string, 'built-in function')
                    add_entry('built-in function', string)

        newlist = new.items()
        newlist.sort(key=lambda t: t[0].lower())
        def keyfunc((k, v), ltrs=uppercase+'_'):
            # hack: mutate the subitems dicts to a list
            v[1] = list(sorted((si, se) for (si, (se, void)) in v[1].iteritems()))
            # now calculate the key
            letter = k[0].upper()
            if letter in ltrs:
                return letter
            else:
                # get all other symbols under one heading
                return 'Symbols'
        self.index = [(key, list(group)) for (key, group) in
                      itertools.groupby(newlist, keyfunc)]

    def check_consistency(self):
        """Do consistency checks."""

        for filename in self.mtimes:
            if filename not in self.toctree_relations:
                if filename == 'contents.rst':
                    # the master file is not included anywhere ;)
                    continue
                self.warning_stream.write(
                    'WARNING: %s isn\'t included in any toctree\n' % filename)

    # --------- QUERYING -------------------------------------------------------

    def find_desc(self, modname, classname, name, type):
        """Find a description node matching "name", perhaps using
           the given module and/or classname."""
        # skip parens
        if name[-2:] == '()':
            name = name[:-2]

        # don't add module and class names for C things
        if type[0] == 'c' and type not in ('class', 'const'):
            # skip trailing star and whitespace
            name = name.rstrip(' *')
            if name in self.descrefs and self.descrefs[name][1][0] == 'c':
                return name, self.descrefs[name]
            return None, None

        if name in self.descrefs:
            newname = name
        elif modname and modname + '.' + name in self.descrefs:
            newname = modname + '.' + name
        elif modname and classname and \
                 modname + '.' + classname + '.' + name in self.descrefs:
            newname = modname + '.' + classname + '.' + name
        # special case: builtin exceptions have module "exceptions" set
        elif type == 'exc' and '.' not in name and \
             'exceptions.' + name in self.descrefs:
            newname = 'exceptions.' + name
        else:
            return None, None
        return newname, self.descrefs[newname]

    def find_keyword(self, keyword, avoid_fuzzy=False, cutoff=0.6, n=20):
        """
        Find keyword matches for a keyword. If there's an exact match, just return
        it, else return a list of fuzzy matches if avoid_fuzzy isn't True.

        Keywords searched are: first modules, then descrefs.

        Returns: None if nothing found
                 (type, filename, anchorname) if exact match found
                 list of (quality, type, filename, anchorname, description) if fuzzy
        """

        if keyword in self.modules:
            filename, title, system = self.modules[keyword]
            return 'module', filename, 'module-' + keyword
        if keyword in self.descrefs:
            filename, ref_type = self.descrefs[keyword]
            return ref_type, filename, keyword

        if avoid_fuzzy:
            return

        # find fuzzy matches
        s = difflib.SequenceMatcher()
        s.set_seq2(keyword.lower())

        def possibilities():
            for title, (fn, desc, _) in self.modules.iteritems():
                yield ('module', fn, 'module-'+title, desc)
            for title, (fn, desctype) in self.descrefs.iteritems():
                yield (desctype, fn, title, '')

        def dotsearch(string):
            parts = string.lower().split('.')
            for idx in xrange(0, len(parts)):
                yield '.'.join(parts[idx:])

        result = []
        for type, filename, title, desc in possibilities():
            best_res = 0
            for part in dotsearch(title):
                s.set_seq1(part)
                if s.real_quick_ratio() >= cutoff and \
                   s.quick_ratio() >= cutoff and \
                   s.ratio() >= cutoff and \
                   s.ratio() > best_res:
                    best_res = s.ratio()
            if best_res:
                result.append((best_res, type, filename, title, desc))

        return heapq.nlargest(n, result)

    def get_real_filename(self, filename):
        """
        Pass this function a filename without .rst extension to
        get the real filename. This also resolves the special
        `index.rst` files. If the file does not exist the return
        value will be `None`.
        """
        for rstname in filename + '.rst', filename + path.sep + 'index.rst':
            if rstname in self.mtimes:
                return rstname
