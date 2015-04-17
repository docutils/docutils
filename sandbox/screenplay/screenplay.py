# $Id$
# Authors: Jeffrey C. Jacobs
# Copyright: This module has been placed in the public domain.

"""
Directives for screenplays.
"""

__docformat__ = 'reStructuredText'

from docutils import nodes, languages
from docutils.transforms import parts
from docutils.parsers.rst import Directive
from docutils.parsers.rst import directives


class Screenplay(Directive):

    """
    Use Screenplay formatting.

    The screenplay format is a post-processing transform to change
    certain document nodes and classes to be compatible for styling with
    the various output writers.  During the initial parse, a 'pending'
    element is generated which acts as a placeholder, storing any
    options internally.  At a later stage in the processing, the
    'pending' element is removed and the entire doctree is scanned for
    any elements which need to be transformed.
    """
    # TODO: Replace with code as described by class documentation

    backlinks_values = ('top', 'entry', 'none')

    def backlinks(arg):
        value = directives.choice(arg, Contents.backlinks_values)
        if value == 'none':
            return None
        else:
            return value

    optional_arguments = 1
    final_argument_whitespace = True
    option_spec = {'depth': directives.nonnegative_int,
                   'local': directives.flag,
                   'backlinks': backlinks,
                   'class': directives.class_option}
    
    def run(self):
        if not (self.state_machine.match_titles
                or isinstance(self.state_machine.node, nodes.sidebar)):
            raise self.error('The "%s" directive may not be used within '
                             'topics or body elements.' % self.name)
        document = self.state_machine.document
        language = languages.get_language(document.settings.language_code,
                                          document.reporter)
        if self.arguments:
            title_text = self.arguments[0]
            text_nodes, messages = self.state.inline_text(title_text,
                                                          self.lineno)
            title = nodes.title(title_text, '', *text_nodes)
        else:
            messages = []
            if 'local' in self.options:
                title = None
            else:
                title = nodes.title('', language.labels['contents'])
        topic = nodes.topic(classes=['contents'])
        topic['classes'] += self.options.get('class', [])
        # the latex2e writer needs source and line for a warning:
        topic.source, topic.line = self.state_machine.get_source_and_line()
        topic.line -= 1
        if 'local' in self.options:
            topic['classes'].append('local')
        if title:
            name = title.astext()
            topic += title
        else:
            name = language.labels['contents']
        name = nodes.fully_normalize_name(name)
        if not document.has_name(name):
            topic['names'].append(name)
        document.note_implicit_target(topic)
        pending = nodes.pending(parts.Contents, rawsource=self.block_text)
        pending.details.update(self.options)
        document.note_pending(pending)
        topic += pending
        return [topic] + messages

