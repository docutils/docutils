"""
Python based conversion.
"""

###############################################################################
###############################################################################
# Import

import re
import docutils
import docutils.parsers.rst.states

__docformat__ = 'reStructuredText'

###############################################################################
###############################################################################
# Classes

class Inliner(docutils.parsers.rst.states.Inliner):
    """
    Recognizer for inline markup. Derive this from the original inline
    markup parser for best results.
    """

    def quoteInline(self, text):
        """
        `text`: ``str``
          Return `text` with inline markup quoted.
        """
        # Method inspired by `docutils.parsers.rst.states.Inliner.parse`
        self.document = docutils.utils.new_document("<string>")
        self.document.settings.trim_footnote_reference_space = False
        self.reporter = self.document.reporter
        self.reporter.stream = None
        self.language = None
        self.parent = self.document
        remaining = docutils.utils.escape2null(text)
        checked = ""
        processed = []
        unprocessed = []
        messages = []
        while remaining:
            original = remaining
            match = self.patterns.initial.search(remaining)
            if match:
                groups = match.groupdict()
                method = self.dispatch[groups['start'] or groups['backquote']
                                       or groups['refend'] or groups['fnend']]
                ( before, inlines, remaining, sysmessages,
                  ) = method(self, match, 0)
                checked += before
                if inlines:
                    assert len(inlines) == 1, "More than one inline found"
                    inline = original[len(before)
                                      :len(original) - len(remaining)]
                    rolePfx = re.search("^:" + self.simplename + ":(?=`)",
                                        inline)
                    refSfx = re.search("_+$", inline)
                    if rolePfx:
                        # Prefixed roles need to be quoted in the middle
                        checked += (inline[:rolePfx.end()] + "\\"
                                    + inline[rolePfx.end():])
                    elif refSfx and not re.search("^`", inline):
                        # Pure reference markup needs to be quoted at the end
                        checked += (inline[:refSfx.start()] + "\\"
                                    + inline[refSfx.start():])
                    else:
                        # Quote other inlines by prefixing
                        checked += "\\" + inline
            else:
                checked += remaining
                break
        # Quote all original backslashes
        checked = re.sub('\x00', "\\\x00", checked)
        return docutils.utils.unescape(checked, 1)

###############################################################################

class Text():
    """
    Functions for computing valid reStructuredText plain text.
    """

    inliner = Inliner()

    @staticmethod
    def plain(text, indent, literal):
        """
        Return a plain text preventing further interpretation by
        reStructuredText. Text may contain linefeeds.

        `text`: ``str``
          The string to turn into output text.

        `indent`: ``str``
          The string to use for indent in case of internal linefeeds.

        `literal`: ``bool``
          Output literal instead of quoting.
        """
        sep = "\n" + indent
        if literal:
            quoted = text
        else:
            quoted = Text.inliner.quoteInline(text)
        return sep.join(quoted.split("\n"))

#print(Text.plain("Some \\ back\slashes", ""))

# indent
# directive
# field_names
# substitution
# inline markup
# token
# label
# start_delimiter
# end_delimiter
# target_definition
