#! /usr/bin/env python3

# $Id$
# Author: reggie dugard <reggie@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test `core.publish_parts()`__ with the html4css1 writer.

__ https://docutils.sourceforge.io/docs/api/publisher.html#publish-parts
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

import docutils
import docutils.core
from docutils.writers import html4css1

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = Path(__file__).parents[1]
DATA_ROOT = TEST_ROOT / 'data'
ROOT_PREFIX = (TEST_ROOT / 'functional/input').as_posix()


# Parts returned by `publish_parts()` for the HTML5 writer by default:
#   * empty input string
#   * default configuration settings.
# See format_parts() below for the substitution of unresolved format markers.
default_parts = {
    'body': '{fragment}',
    'body_pre_docinfo': '',
    'body_prefix': '</head>\n<body>\n{header}<div class="document">\n',
    'body_suffix': '</div>\n{footer}</body>\n</html>\n',
    'docinfo': '',
    'encoding': 'utf-8',
    'errors': 'xmlcharrefreplace',
    'footer': '',
    'fragment': '',
    'head': '{meta}<title>{metatitle}</title>\n',
    'head_prefix':
        '<?xml version="1.0" encoding="utf-8"?>\n'
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
        ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n'
        '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">\n'
        '<head>\n',
    'header': '',
    'html_body': '{header}<div class="document">\n{fragment}</div>\n{footer}',
    'html_head': '{meta}<title>{metatitle}</title>\n',
    'html_prolog':
        '<?xml version="1.0" encoding="%s"?>\n'
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
        ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n',
    'html_subtitle': '',
    'html_title': '',
    'meta': '<meta http-equiv="Content-Type" content="text/html; charset=%s" />\n'
            f'<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />\n',
    'stylesheet': '',
    'subtitle': '',
    'title': '',
    'version': f'{docutils.__version__}',
    'whole':
        '{head_prefix}\n'
        '{head}\n'
        '{stylesheet}\n'
        '{body_prefix}\n'
        '{body_pre_docinfo}\n'
        '{docinfo}\n'
        '{body}\n'
        '{body_suffix}\n',
    }


def format_parts(parts):
    # fill in part values that depend on other parts
    # https://docutils.sourceforge.io/docs/api/publisher.html#html4-writer

    # metadata title: either the visible title or document['source']
    metatitle = parts['title'] or '&lt;string&gt;'

    # "html_head" leaves the encoding unresolved, as "%s":
    parts['html_head'] = parts['html_head'].format(metatitle=metatitle,
                                                   **parts)
    # now resolve encoding:
    try:
        parts['meta'] = parts['meta'] % parts['encoding']
    except TypeError:  # charset-<meta> missing if encoding is 'unicode'
        pass
    parts['head'] = parts['head'].format(metatitle=metatitle, **parts)
    parts['body_prefix'] = parts['body_prefix'].format(**parts)
    parts['body'] = parts['body'].format(**parts)
    parts['body_suffix'] = parts['body_suffix'].format(**parts)
    parts['html_body'] = parts['html_body'].format(**parts)
    # newlines are stripped when parts are used to expand the template file
    parts['whole'] = parts['whole'].format(**{k: v.rstrip('\n')
                                              for k, v in parts.items()})
    return parts


class Html5WriterPublishPartsTestCase(unittest.TestCase):
    """Test HTML5 writer `publish_parts()` interface."""

    maxDiff = None

    def test_publish_parts(self):
        for name, (settings_overrides, cases) in totest.items():
            for casenum, (case_input, expected_parts) in enumerate(cases):
                _stgns = {'_disable_config': True,
                          'strict_visitor': True,
                          'stylesheet_path': '',
                          'section_self_link': True,
                          **settings_overrides,
                          }
                parts = docutils.core.publish_parts(source=case_input,
                                                    writer=html4css1.Writer(),
                                                    settings_overrides=_stgns,
                                                    )
                expected = format_parts(default_parts | expected_parts)
                for key in parts.keys():
                    with self.subTest(id=f'totest[{name!r}][{casenum}][{key}]'):
                        self.assertEqual(f'{expected[key]}',
                                         f'{parts[key]}')


totest = {}

totest['standard'] = ({}, [
    ['',  # empty input string
     {}   # results in default parts
     ],
    ['Simple String with *markup*',
     {'fragment': '<p>Simple String with <em>markup</em></p>\n'}
     ],
    ['.. header:: custom document header\n\n'
     'A paragraph.',
     {'header': '<div class="header">\ncustom document header\n'
                '<hr class="header"/>\n</div>\n',
      'body_prefix': '</head>\n'
                     '<body>\n'
                     '<div class="header">\n'
                     'custom document header\n'
                     '<hr class="header"/>\n'
                     '</div>\n'
                     '<div class="document">\n',
      'fragment': '<p>A paragraph.</p>\n',
      }
     ],
    ['.. footer:: custom document footer\n\n'
     'A paragraph.',
     {'footer': '<div class="footer">\n'
                '<hr class="footer" />\n'
                'custom document footer\n</div>\n',
      'fragment': '<p>A paragraph.</p>\n',
      }
     ],
    ["""\
+++++
Title
+++++

Subtitle
========

Some stuff

Section
-------

Some more stuff

Another Section
...............

And even more stuff
""",
    {'body_pre_docinfo': '<h1 class="title">Title</h1>\n'
                         '<h2 class="subtitle" id="subtitle">Subtitle</h2>\n',
     'body_prefix': '</head>\n<body>\n<div class="document" id="title">\n',
     'fragment': """\
<p>Some stuff</p>
<div class="section" id="section">
<h1>Section</h1>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h2>Another Section</h2>
<p>And even more stuff</p>
</div>
</div>
""",
     'html_body': """\
<div class="document" id="title">
<h1 class="title">Title</h1>
<h2 class="subtitle" id="subtitle">Subtitle</h2>
<p>Some stuff</p>
<div class="section" id="section">
<h1>Section</h1>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h2>Another Section</h2>
<p>And even more stuff</p>
</div>
</div>
</div>
""",
     'html_subtitle': '<h2 class="subtitle" id="subtitle">Subtitle</h2>\n',
     'html_title': '<h1 class="title">Title</h1>\n',
     'subtitle': 'Subtitle',
     'title': 'Title'
     }],
    ["""\
+++++
Title
+++++

:author: me

Some stuff
""",
    {'body_pre_docinfo': '<h1 class="title">Title</h1>\n',
     'body_prefix': '</head>\n<body>\n<div class="document" id="title">\n',
     'docinfo': """\
<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>me</td></tr>
</tbody>
</table>
""",
     'fragment': '<p>Some stuff</p>\n',
     'html_body': """\
<div class="document" id="title">
<h1 class="title">Title</h1>
<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>me</td></tr>
</tbody>
</table>
<p>Some stuff</p>
</div>
""",
     'html_title': '<h1 class="title">Title</h1>\n',
     'meta': default_parts['meta'] + '<meta name="author" content="me" />\n',
     'title': 'Title'
     }],
    ])

totest['no_title_promotion'] = ({'doctitle_xform': False}, [
    ["""\
+++++
Title
+++++

Not A Subtitle
==============

Some stuff

Section
-------

Some more stuff

Another Section
...............

And even more stuff
""",
     {'fragment': """\
<div class="section" id="title">
<h1>Title</h1>
<div class="section" id="not-a-subtitle">
<h2>Not A Subtitle</h2>
<p>Some stuff</p>
<div class="section" id="section">
<h3>Section</h3>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h4>Another Section</h4>
<p>And even more stuff</p>
</div>
</div>
</div>
</div>
"""},
     ],
    ])

totest['unknown-encoding'] = ({'output_encoding': 'unicode'}, [
    ['Simple String\n',
     {'encoding': 'unicode',
      'fragment': '<p>Simple String</p>\n',
      'head_prefix':
          '<?xml version="1.0"?>\n'
          '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
          ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n'
          '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">\n'
          '<head>\n',
      'html_head': f'{default_parts["meta"]}<title>{{metatitle}}</title>\n',
      'meta': f'<meta name="generator" content="Docutils {docutils.__version__}: https://docutils.sourceforge.io/" />\n',
      }],
    ])


if __name__ == '__main__':
    unittest.main()
