# -*- coding: utf-8 -*-
"""
    sphinx.builder
    ~~~~~~~~~~~~~~

    Builder classes for different output formats.

    :copyright: 2007 by Georg Brandl.
    :license: BSD license.
"""
from __future__ import with_statement

import os
import sys
import time
import types
import codecs
import shutil
import cPickle as pickle
import cStringIO as StringIO
from os import path

from docutils.io import StringOutput, DocTreeInput
from docutils.core import publish_parts
from docutils.utils import new_document
from docutils.readers import doctree
from docutils.frontend import OptionParser

from .util import (get_matching_files, attrdict, status_iterator,
                   ensuredir, get_category, relative_uri)
from .writer import HTMLWriter
from .console import bold, purple, green
from .environment import BuildEnvironment
from .highlighting import get_stylesheet

# side effect: registers roles and directives
from . import roles
from . import directives

ENV_PICKLE_FILENAME = 'environment.pickle'

# Helper objects

class relpath_to(object):
    def __init__(self, builder, filename):
        self.baseuri = builder.get_target_uri(filename)
        self.builder = builder
    def __call__(self, otheruri, resource=False):
        if not resource:
            otheruri = self.builder.get_target_uri(otheruri)
        return relative_uri(self.baseuri, otheruri)


class collect_env_warnings(object):
    def __init__(self, builder):
        self.builder = builder
    def __enter__(self):
        self.stream = StringIO.StringIO()
        self.builder.env.set_warning_stream(self.stream)
    def __exit__(self, *args):
        self.builder.env.set_warning_stream(self.builder.warning_stream)
        warnings = self.stream.getvalue()
        if warnings:
            print >>self.builder.warning_stream, warnings


class Builder(object):
    """
    Builds target formats from the reST sources.
    """

    option_spec = {
        'freshenv': 'Don\'t use a pickled environment',
    }

    def __init__(self, srcdirname, outdirname, options, env=None,
                 status_stream=None, warning_stream=None):
        self.srcdir = srcdirname
        self.outdir = outdirname

        self.options = attrdict(options)
        self.validate_options()

        # probably set in load_env()
        self.env = env

        self.config = {}
        execfile(path.join(srcdirname, 'conf.py'), self.config)
        # remove potentially pickling-problematic values
        del self.config['__builtins__']
        for key, val in self.config.items():
            if isinstance(val, types.ModuleType):
                del self.config[key]

        self.status_stream = status_stream or sys.stdout
        self.warning_stream = warning_stream or sys.stderr

        self.init()

    # helper methods

    def validate_options(self):
        for option in self.options:
            if option not in self.option_spec:
                raise ValueError('Got unexpected option %s' % option)
        for option in self.option_spec:
            if option not in self.options:
                self.options[option] = False

    def msg(self, message='', nonl=False, nobold=False):
        if not nobold: message = bold(message)
        if nonl:
            print >>self.status_stream, message,
        else:
            print >>self.status_stream, message
        self.status_stream.flush()

    def init(self):
        """Load necessary templates and perform initialization."""
        raise NotImplementedError

    def get_target_uri(self, source_filename):
        """Return the target URI for a source filename."""
        raise NotImplementedError

    def get_relative_uri(self, from_, to):
        """Return a relative URI between two source filenames."""
        return relative_uri(self.get_target_uri(from_),
                            self.get_target_uri(to))

    def get_outdated_files(self):
        """Return a list of output files that are outdated."""
        raise NotImplementedError

    # build methods

    def load_env(self):
        """Set up the build environment. Return True if a pickled file could be
           successfully loaded, False if a new environment had to be created."""
        if self.env:
            return
        if not self.options.freshenv:
            try:
                self.msg('trying to load pickled env...', nonl=True)
                self.env = BuildEnvironment.frompickle(
                    path.join(self.outdir, ENV_PICKLE_FILENAME))
                self.msg('done', nobold=True)
            except Exception, err:
                self.msg('failed: %s' % err, nobold=True)
                self.env = BuildEnvironment(self.srcdir)
        else:
            self.env = BuildEnvironment(self.srcdir)

    def build_all(self):
        """Build all source files."""
        self.load_env()
        self.build(None, summary='all source files')

    def build_specific(self, source_filenames):
        """Only rebuild as much as needed for changes in the source_filenames."""
        # bring the filenames to the canonical format, that is,
        # relative to the source directory.
        dirlen = len(self.srcdir) + 1
        to_write = [path.abspath(filename)[dirlen:] for filename in source_filenames]
        self.load_env()
        self.build(to_write,
                   summary='%d source files given on command line' % len(to_write))

    def build_update(self):
        """Only rebuild files changed or added since last build."""
        self.load_env()
        to_build = list(self.get_outdated_files())
        if not to_build:
            self.msg('no files are out of date, exiting.')
            return
        self.build(to_build,
                   summary='%d source files that are out of date' % len(to_build))

    def build(self, filenames, summary=None):
        if summary:
            self.msg('building [%s]:' % self.name, nonl=1)
            self.msg(summary, nobold=1)

        # while reading, collect all warnings from docutils
        with collect_env_warnings(self):
            self.msg('reading, updating environment...')
            for filename in self.env.update(self.config):
                self.msg(purple(filename), nonl=1, nobold=1)
            self.msg()

        # save the environment
        self.msg('pickling the env...', nonl=True)
        self.env.topickle(path.join(self.outdir, ENV_PICKLE_FILENAME))
        self.msg('done', nobold=True)

        # global actions
        self.msg('checking consistency...')
        self.env.check_consistency()
        self.msg('creating index...')
        self.env.create_index(self)

        self.prepare_writing()

        if filenames:
            # add all TOC files that may have changed
            filenames_set = set(filenames)
            for filename in filenames:
                for tocfilename in self.env.files_to_rebuild.get(filename, []):
                    filenames_set.add(tocfilename)
        else:
            # build all
            filenames_set = set(self.env.all_files)

        # write target files
        with collect_env_warnings(self):
            self.msg('writing output...')
            for filename in status_iterator(sorted(filenames_set), green,
                                            stream=self.status_stream):
                doctree = self.env.get_and_resolve_doctree(filename, self)
                self.write_file(filename, doctree)

        # finish (write style files etc.)
        self.msg('finishing...')
        self.finish()
        self.msg('done!')

    def prepare_writing(self):
        raise NotImplementedError

    def write_file(self, filename, doctree):
        raise NotImplementedError

    def finish(self):
        raise NotImplementedError


class StandaloneHTMLBuilder(Builder):
    """
    Builds standalone HTML docs.
    """
    name = 'html'

    option_spec = Builder.option_spec
    option_spec.update({
        'nostyle': 'Don\'t copy style and script files',
        'searchindex': 'Create a JSON search index for offline search',
    })

    def init(self):
        """Load templates."""
        # lazily import this, maybe other builders won't need it
        from jinja import Environment, FileSystemLoader

        # load templates
        self.templates = {}
        templates_path = path.join(path.dirname(__file__), 'templates')
        jinja_env = Environment(loader=FileSystemLoader(templates_path),
                                # disable traceback, more likely that something in the
                                # application is broken than in the templates
                                friendly_traceback=False)
        for fname in os.listdir(templates_path):
            if fname.endswith('.html'):
                self.templates[fname[:-5]] = jinja_env.get_template(fname)

        # this one is used for all reST pages
        self.page_template = self.templates.pop('page')
        # this one is just included by the others
        self.templates.pop('layout')

    def render_partial(self, node):
        """Utility: Render a lone doctree node."""
        doc = new_document('foo')
        doc.append(node)
        return publish_parts(
            doc,
            source_class=DocTreeInput,
            reader=doctree.Reader(),
            writer=HTMLWriter(self.config),
            settings_overrides={'output_encoding': 'unicode'}
        )

    def get_target_uri(self, source_filename):
        return source_filename[:-4] + '.html'

    def prepare_writing(self):
        if self.options.searchindex:
            from .search import IndexBuilder
            self.indexer = IndexBuilder()
        else:
            self.indexer = None
        self.docwriter = HTMLWriter(self.config)
        self.docsettings = OptionParser(
            defaults=self.env.settings,
            components=(self.docwriter,)).get_default_values()

        # format the "last updated on" string, only once is enough since it
        # typically doesn't include the time of day
        lufmt = self.config.get('last_updated_format')
        if lufmt:
            self.last_updated = time.strftime(lufmt)
        else:
            self.last_updated = None

    def write_file(self, filename, doctree):
        destination = StringOutput(encoding='utf-8')
        doctree.settings = self.docsettings

        output = self.docwriter.write(doctree, destination)
        self.docwriter.assemble_parts()

        prev = next = None
        parents = []
        related = self.env.toctree_relations.get(filename)
        if related:
            prev = {'link': self.get_relative_uri(filename, related[1]),
                    'title': self.render_partial(self.env.titles[related[1]])['title']}
            next = {'link': self.get_relative_uri(filename, related[2]),
                    'title': self.render_partial(self.env.titles[related[2]])['title']}
        while related:
            parents.append(
                {'link': self.get_relative_uri(filename, related[0]),
                 'title': self.render_partial(self.env.titles[related[0]])['title']})
            related = self.env.toctree_relations.get(related[0])
        if parents:
            parents.pop() # remove link to "contents.rst"; we have a generic
                          # "back to index" link already
        parents.reverse()

        title = self.env.titles.get(filename)
        if title:
            title = self.render_partial(title)['title']
        else:
            title = ''
        sourcename = filename[:-4] + '.txt'
        context = dict(
            title = title,
            pathto = relpath_to(self, self.get_target_uri(filename)),
            body = self.docwriter.parts['fragment'],
            toc = self.render_partial(self.env.get_toc_for(filename))['fragment'],
            # only display a TOC if there's more than one item to show
            display_toc = (self.env.toc_num_entries[filename] > 1),
            parents = parents,
            prev = prev,
            next = next,
            sourcename = sourcename,
            last_updated = self.last_updated,
            builder = self.name,
            release = self.config['version'],
        )

        self.index_file(filename, doctree, title)
        self.handle_file(filename, context)

    def finish(self):
        self.msg('writing additional files...')
        specialcontext = dict(
            # used to create links to supporting files like stylesheets
            pathto = relpath_to(self, self.get_target_uri('special.rst')),
            parents = [],
            len = len,
            last_updated = self.last_updated,
            builder = self.name,
            release = self.config['version'],
        )

        # the global general index

        # the total count of lines for each index letter, used to distribute
        # the entries into two columns
        indexcounts = []
        for key, entries in self.env.index:
            indexcounts.append(sum(1 + len(subitems) for _, (_, subitems) in entries))

        genindexcontext = specialcontext.copy()
        genindexcontext['genindexentries'] = self.env.index
        genindexcontext['genindexcounts'] = indexcounts
        genindexcontext['current_page_name'] = 'genindex'
        self.handle_file('genindex.rst', genindexcontext)

        # the global module index

        # the sorted list of all modules, for the global module index
        modules = list(sorted(
            ((mn, (self.get_relative_uri('modindex.rst', fn) +
                   '#module-' + mn, sy, pl))
             for (mn, (fn, sy, pl)) in self.env.modules.iteritems()),
            key=lambda x: x[0].lower()))

        modindexcontext = specialcontext.copy()
        modindexcontext['modindexentries'] = modules
        modindexcontext['current_page_name'] = 'modindex'
        self.handle_file('modindex.rst', modindexcontext)

        # the index page
        specialcontext['current_page_name'] = 'index'
        self.handle_file('index.rst', specialcontext)

        # the search page
        specialcontext['current_page_name'] = 'search'
        self.handle_file('search.rst', specialcontext)

        # dump the search index
        self.dump_index()

        if not self.options.nostyle:
            self.msg('copying style files...')
            # copy style files
            styledirname = path.join(path.dirname(__file__), 'style')
            ensuredir(path.join(self.outdir, 'style'))
            for filename in os.listdir(styledirname):
                if not filename.startswith('.'):
                    shutil.copyfile(path.join(styledirname, filename),
                                    path.join(self.outdir, 'style', filename))
            # add pygments style file
            f = open(path.join(self.outdir, 'style', 'pygments.css'), 'w')
            f.write(get_stylesheet())
            f.close()

    # --------- these are overwritten by the Web builder

    def get_outdated_files(self):
        for filename in get_matching_files(
            self.srcdir, '*.rst', exclude=set(self.config.get('unused_files', ()))):
            try:
                targetmtime = path.getmtime(path.join(self.outdir,
                                                      filename[:-4] + '.html'))
            except:
                targetmtime = 0
            if path.getmtime(path.join(self.srcdir, filename)) > targetmtime:
                yield filename

    def index_file(self, filename, doctree, title):
        # only index pages with title
        if self.indexer is not None and title:
            category = get_category(filename)
            if category is not None:
                self.indexer.feed(self.get_target_uri(filename)[:-5], # strip '.html'
                                  category, title, doctree)

    def handle_file(self, filename, context):
        output = self.page_template.render(context)
        outfilename = path.join(self.outdir, filename[:-4] + '.html')
        ensuredir(path.dirname(outfilename)) # normally different from self.outdir
        try:
            fp = None
            fp = codecs.open(outfilename, 'w', 'utf-8')
            fp.write(output)
        except (IOError, OSError), err:
            print >>self.warning_stream, "Error writing file %s: %s" % (outfilename, err)
        finally:
            if fp:
                fp.close()
        if 'sourcename' in context:
            # copy the source file for the "show source" link
            shutil.copyfile(path.join(self.srcdir, filename),
                            path.join(self.outdir, context['sourcename']))

    def dump_index(self):
        if self.indexer is not None:
            self.msg('dumping search index...')
            f = open(path.join(self.outdir, 'searchindex.json'), 'w')
            self.indexer.dump(f, 'json')
            f.close()


class WebHTMLBuilder(StandaloneHTMLBuilder):
    """
    Builds HTML docs usable with the web-based doc server.
    """
    name = 'web'

    # doesn't use the standalone specific options
    option_spec = Builder.option_spec
    option_spec.update({
        'nostyle': 'Don\'t copy style and script files',
        'searchindex': 'Create a search index for the online search',
    })

    def init(self):
        # Nothing to do here.
        pass

    def get_outdated_files(self):
        for filename in get_matching_files(
            self.srcdir, '*.rst', exclude=set(self.config.get('unused_files', ()))):
            try:
                targetmtime = path.getmtime(path.join(self.outdir,
                                                      filename[:-4] + '.fpickle'))
            except:
                targetmtime = 0
            if path.getmtime(path.join(self.srcdir, filename)) > targetmtime:
                yield filename

    def get_target_uri(self, source_filename):
        if source_filename == 'index.rst':
            return ''
        if source_filename.endswith('/index.rst'):
            return source_filename[:-9] # up to /
        return source_filename[:-4] + '/'

    def index_file(self, filename, doctree, title):
        # only index pages with title and category
        if self.indexer is not None and title:
            category = get_category(filename)
            if category is not None:
                self.indexer.feed(filename, category, title, doctree)

    def handle_file(self, filename, context):
        outfilename = path.join(self.outdir, filename[:-4] + '.fpickle')
        ensuredir(path.dirname(outfilename))
        fp = open(outfilename, 'wb')
        context.pop('pathto', None) # can't be pickled
        pickle.dump(context, fp, 2)
        fp.close()

        # if there is a source file, copy the source file for the "show source" link
        if 'sourcename' in context:
            source_name = path.join(self.outdir, 'sources', context['sourcename'])
            ensuredir(path.dirname(source_name))
            shutil.copyfile(path.join(self.srcdir, filename), source_name)

    def dump_index(self):
        if self.indexer is not None:
            self.msg('dumping search index...')
            f = open(path.join(self.outdir, 'searchindex.pickle'), 'w')
            self.indexer.dump(f, 'pickle')
            f.close()


builders = {
    'html': StandaloneHTMLBuilder,
    'web': WebHTMLBuilder,
#    'latex': LatexBuilder,
}
