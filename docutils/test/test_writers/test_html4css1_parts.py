#! /usr/bin/env python3

# $Id$
# Author: reggie dugard <reggie@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test for fragment code in HTML writer.

Note: the 'body' and 'whole' entries have been removed from the parts
dictionaries (redundant), along with 'meta' and 'stylesheet' entries with
standard values, and any entries with empty values.
"""

from test import DocutilsTestSupport

import docutils
import docutils.core

WriterPublishTestCase = DocutilsTestSupport.WriterPublishTestCase


class HtmlWriterPublishPartsTestCase(WriterPublishTestCase):

    """
    Test case for HTML writer via the publish_parts interface.
    """

    writer_name = 'html'

    settings_default_overrides = \
        WriterPublishTestCase.settings_default_overrides.copy()
    settings_default_overrides['stylesheet'] = ''

    def test_publish(self):
        parts = docutils.core.publish_parts(
            source=self.input,
            reader_name='standalone',
            parser_name='restructuredtext',
            writer_name=self.writer_name,
            settings_spec=self,
            settings_overrides=self.suite_settings)
        self.assertEqual(self.format_output(parts), self.expected)

    standard_content_type_template = ('<meta http-equiv="Content-Type"'
                                      ' content="text/html; charset=%s" />\n')
    standard_generator_template = (
        '<meta name="generator"'
        f' content="Docutils {docutils.__version__}: '
        f'https://docutils.sourceforge.io/" />\n')
    standard_html_meta_value = (
        standard_content_type_template
        + standard_generator_template)
    standard_meta_value = standard_html_meta_value % 'utf-8'
    standard_html_prolog = (
        '<?xml version="1.0" encoding="%s" ?>\n'
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" '
        '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n')

    def format_output(self, parts):
        """Minimize & standardize the output."""
        # remove redundant parts & uninteresting parts:
        del parts['whole']
        assert parts['body'] == parts['fragment']
        del parts['body']
        del parts['body_pre_docinfo']
        del parts['body_prefix']
        del parts['body_suffix']
        del parts['head']
        del parts['head_prefix']
        del parts['encoding']
        del parts['version']
        # remove standard portions:
        parts['meta'] = parts['meta'].replace(self.standard_meta_value, '')
        parts['html_head'] = parts['html_head'].replace(
            self.standard_html_meta_value, '...')
        parts['html_prolog'] = parts['html_prolog'].replace(
            self.standard_html_prolog, '')
        return {k: v for k, v in parts.items() if v}


class HtmlPublishPartsTestSuite(DocutilsTestSupport.CustomTestSuite):
    def generateTests(self, dict):
        for name, (settings_overrides, cases) in dict.items():
            original_settings = self.suite_settings.copy()
            self.suite_settings.update(settings_overrides)
            for casenum, (case_input, case_expected) in enumerate(cases):
                self.addTestCase(
                    HtmlWriterPublishPartsTestCase, 'test_publish',
                    input=case_input, expected=case_expected,
                    id=f'totest[{name!r}][{casenum}]')
            self.suite_settings = original_settings


def suite():
    s = HtmlPublishPartsTestSuite()
    s.generateTests(totest)
    return s


totest = {}

totest['title_promotion'] = ({'stylesheet_path': '',
                              'embed_stylesheet': 0}, [
["""\
Simple String
""",
{'fragment': '''<p>Simple String</p>\n''',
 'html_body': '''<div class="document">
<p>Simple String</p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
Simple String with *markup*
""",
{'fragment': '''<p>Simple String with <em>markup</em></p>\n''',
 'html_body': '''<div class="document">
<p>Simple String with <em>markup</em></p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
Simple String with an even simpler ``inline literal``
""",
{'fragment': '''<p>Simple String with an even simpler <tt class="docutils literal">inline literal</tt></p>\n''',
 'html_body': '''<div class="document">
<p>Simple String with an even simpler <tt class="docutils literal">inline literal</tt></p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
Simple ``inline\xA0literal`` with NBSP
""",
{'fragment': '''<p>Simple <tt class="docutils literal">inline&nbsp;literal</tt> with NBSP</p>\n''',
 'html_body': '''<div class="document">
<p>Simple <tt class="docutils literal">inline&nbsp;literal</tt> with NBSP</p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
{'fragment': '''<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>\n''',
 'html_body': '''<div class="document">
<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
One paragraph.

Two paragraphs.
""",
{'fragment': '''<p>One paragraph.</p>
<p>Two paragraphs.</p>\n''',
 'html_body': '''<div class="document">
<p>One paragraph.</p>
<p>Two paragraphs.</p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
{'fragment': '''<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>\n''',
 'html_body': '''<div class="document">
<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
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
{'fragment': '''<p>Some stuff</p>
<div class="section" id="section">
<h1>Section</h1>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h2>Another Section</h2>
<p>And even more stuff</p>
</div>
</div>\n''',
 'html_body': '''<div class="document" id="title">
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
</div>\n''',
 'html_head': '''...<title>Title</title>\n''',
 'html_subtitle': '''<h2 class="subtitle" id="subtitle">Subtitle</h2>\n''',
 'html_title': '''<h1 class="title">Title</h1>\n''',
 'subtitle': '''Subtitle''',
 'title': '''Title'''
}],
["""\
+++++
Title
+++++

:author: me

Some stuff
""",
{'docinfo': '''<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>me</td></tr>
</tbody>
</table>\n''',
 'fragment': '''<p>Some stuff</p>\n''',
 'html_body': '''<div class="document" id="title">
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
</div>\n''',
 'html_head': '''...<title>Title</title>
<meta name="author" content="me" />\n''',
 'html_title': '''<h1 class="title">Title</h1>\n''',
 'meta': '''<meta name="author" content="me" />\n''',
 'title': '''Title'''
}]
])

totest['no_title_promotion'] = ({'doctitle_xform': 0,
                                 'stylesheet_path': '',
                                 'embed_stylesheet': 0}, [
["""\
Simple String
""",
{'fragment': '''<p>Simple String</p>\n''',
 'html_body': '''<div class="document">
<p>Simple String</p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
Simple String with *markup*
""",
{'fragment': '''<p>Simple String with <em>markup</em></p>\n''',
 'html_body': '''<div class="document">
<p>Simple String with <em>markup</em></p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
Simple String with an even simpler ``inline literal``
""",
{'fragment': '''<p>Simple String with an even simpler <tt class="docutils literal">inline literal</tt></p>\n''',
 'html_body': '''<div class="document">
<p>Simple String with an even simpler <tt class="docutils literal">inline literal</tt></p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
{'fragment': '''<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>\n''',
 'html_body': '''<div class="document">
<p>A simple <a class="reference external" href="http://www.test.com/test_url">anonymous reference</a></p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
{'fragment': '''<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>\n''',
 'html_body': '''<div class="document">
<p>A simple <a class="reference external" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
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
{'fragment': '''<div class="section" id="title">
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
</div>\n''',
 'html_body': '''<div class="document">
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
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
* bullet
* list
""",
{'fragment': '''<ul class="simple">
<li>bullet</li>
<li>list</li>
</ul>\n''',
 'html_body': '''<div class="document">
<ul class="simple">
<li>bullet</li>
<li>list</li>
</ul>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
.. table::
   :align: right

   +-----+-----+
   |  1  |  2  |
   +-----+-----+
   |  3  |  4  |
   +-----+-----+
""",
{'fragment': '''<table border="1" class="docutils align-right">
<colgroup>
<col width="50%" />
<col width="50%" />
</colgroup>
<tbody valign="top">
<tr><td>1</td>
<td>2</td>
</tr>
<tr><td>3</td>
<td>4</td>
</tr>
</tbody>
</table>\n''',
 'html_body': '''<div class="document">
<table border="1" class="docutils align-right">
<colgroup>
<col width="50%" />
<col width="50%" />
</colgroup>
<tbody valign="top">
<tr><td>1</td>
<td>2</td>
</tr>
<tr><td>3</td>
<td>4</td>
</tr>
</tbody>
</table>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
Not a docinfo.

:This: .. _target:

       is
:a:
:simple:
:field: list
""",
{'fragment': '''<p>Not a docinfo.</p>
<table class="docutils field-list" frame="void" rules="none">
<col class="field-name" />
<col class="field-body" />
<tbody valign="top">
<tr class="field"><th class="field-name">This:</th><td class="field-body"><p class="first last" id="target">is</p>
</td>
</tr>
<tr class="field"><th class="field-name">a:</th><td class="field-body"></td>
</tr>
<tr class="field"><th class="field-name">simple:</th><td class="field-body"></td>
</tr>
<tr class="field"><th class="field-name">field:</th><td class="field-body">list</td>
</tr>
</tbody>
</table>\n''',
 'html_body': '''<div class="document">
<p>Not a docinfo.</p>
<table class="docutils field-list" frame="void" rules="none">
<col class="field-name" />
<col class="field-body" />
<tbody valign="top">
<tr class="field"><th class="field-name">This:</th><td class="field-body"><p class="first last" id="target">is</p>
</td>
</tr>
<tr class="field"><th class="field-name">a:</th><td class="field-body"></td>
</tr>
<tr class="field"><th class="field-name">simple:</th><td class="field-body"></td>
</tr>
<tr class="field"><th class="field-name">field:</th><td class="field-body">list</td>
</tr>
</tbody>
</table>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
["""\
Not a docinfo.

:This is: a
:simple field list with loooong field: names
""",
{'fragment': '''<p>Not a docinfo.</p>
<table class="docutils field-list" frame="void" rules="none">
<col class="field-name" />
<col class="field-body" />
<tbody valign="top">
<tr class="field"><th class="field-name">This is:</th><td class="field-body">a</td>
</tr>
<tr class="field"><th class="field-name" colspan="2">simple field list with loooong field:</th></tr>
<tr class="field"><td>&nbsp;</td><td class="field-body">names</td>
</tr>
</tbody>
</table>\n''',
 'html_body': '''<div class="document">
<p>Not a docinfo.</p>
<table class="docutils field-list" frame="void" rules="none">
<col class="field-name" />
<col class="field-body" />
<tbody valign="top">
<tr class="field"><th class="field-name">This is:</th><td class="field-body">a</td>
</tr>
<tr class="field"><th class="field-name" colspan="2">simple field list with loooong field:</th></tr>
<tr class="field"><td>&nbsp;</td><td class="field-body">names</td>
</tr>
</tbody>
</table>
</div>\n''',
 'html_head': '''...<title>&lt;string&gt;</title>\n'''
}],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
