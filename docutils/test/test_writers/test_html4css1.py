#! /usr/bin/env python

# Author: reggie dugard
# Contact: reggie@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Test for fragment code in HTML writer.

Note: the 'body' and 'whole' entries have been removed from the parts
dictionaries (redundant), along with 'meta' and 'stylesheet' entries with
standard values, and any entries with empty values.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.HtmlPublishPartsTestSuite()
    s.generateTests(totest)
    return s


totest = {}

totest['Title promotion'] = ({}, [
["""\
Simple String
""",
"""\
{'fragment': '''<p>Simple String</p>\\n''',
 'html_body': '''<div class="document">
<p>Simple String</p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
Simple String with *markup*
""",
"""\
{'fragment': '''<p>Simple String with <em>markup</em></p>\\n''',
 'html_body': '''<div class="document">
<p>Simple String with <em>markup</em></p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
Simple String with an even simpler ``inline literal``
""",
"""\
{'fragment': '''<p>Simple String with an even simpler <tt class="docutils literal"><span class="pre">inline</span> <span class="pre">literal</span></tt></p>\\n''',
 'html_body': '''<div class="document">
<p>Simple String with an even simpler <tt class="docutils literal"><span class="pre">inline</span> <span class="pre">literal</span></tt></p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
"""\
{'fragment': '''<p>A simple <a class="reference" href="http://www.test.com/test_url">anonymous reference</a></p>\\n''',
 'html_body': '''<div class="document">
<p>A simple <a class="reference" href="http://www.test.com/test_url">anonymous reference</a></p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
One paragraph.

Two paragraphs.
""",
"""\
{'fragment': '''<p>One paragraph.</p>
<p>Two paragraphs.</p>\\n''',
 'html_body': '''<div class="document">
<p>One paragraph.</p>
<p>Two paragraphs.</p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
"""\
{'fragment': '''<p>A simple <a class="reference" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>\\n''',
 'html_body': '''<div class="document">
<p>A simple <a class="reference" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
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
"""\
{'fragment': '''<p>Some stuff</p>
<div class="section" id="section">
<h1><a name="section">Section</a></h1>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h2><a name="another-section">Another Section</a></h2>
<p>And even more stuff</p>
</div>
</div>\\n''',
 'html_body': '''<div class="document" id="title">
<h1 class="title">Title</h1>
<h2 class="subtitle" id="subtitle">Subtitle</h2>
<p>Some stuff</p>
<div class="section" id="section">
<h1><a name="section">Section</a></h1>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h2><a name="another-section">Another Section</a></h2>
<p>And even more stuff</p>
</div>
</div>
</div>\\n''',
 'html_head': '''...<title>Title</title>\\n''',
 'html_subtitle': '''<h2 class="subtitle" id="subtitle">Subtitle</h2>\\n''',
 'html_title': '''<h1 class="title">Title</h1>\\n''',
 'subtitle': '''Subtitle''',
 'title': '''Title'''}
"""],
["""\
+++++
Title
+++++

:author: me

Some stuff
""",
"""\
{'docinfo': '''<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>me</td></tr>
</tbody>
</table>\\n''',
 'fragment': '''<p>Some stuff</p>\\n''',
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
</div>\\n''',
 'html_head': '''...<title>Title</title>
<meta name="author" content="me" />\\n''',
 'html_title': '''<h1 class="title">Title</h1>\\n''',
 'meta': '''<meta name="author" content="me" />\\n''',
 'title': '''Title'''}
"""]
])

totest['No title promotion'] = ({'doctitle_xform' : 0}, [
["""\
Simple String
""",
"""\
{'fragment': '''<p>Simple String</p>\\n''',
 'html_body': '''<div class="document">
<p>Simple String</p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
Simple String with *markup*
""",
"""\
{'fragment': '''<p>Simple String with <em>markup</em></p>\\n''',
 'html_body': '''<div class="document">
<p>Simple String with <em>markup</em></p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
Simple String with an even simpler ``inline literal``
""",
"""\
{'fragment': '''<p>Simple String with an even simpler <tt class="docutils literal"><span class="pre">inline</span> <span class="pre">literal</span></tt></p>\\n''',
 'html_body': '''<div class="document">
<p>Simple String with an even simpler <tt class="docutils literal"><span class="pre">inline</span> <span class="pre">literal</span></tt></p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
"""\
{'fragment': '''<p>A simple <a class="reference" href="http://www.test.com/test_url">anonymous reference</a></p>\\n''',
 'html_body': '''<div class="document">
<p>A simple <a class="reference" href="http://www.test.com/test_url">anonymous reference</a></p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
"""\
{'fragment': '''<p>A simple <a class="reference" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>\\n''',
 'html_body': '''<div class="document">
<p>A simple <a class="reference" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
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
"""\
{'fragment': '''<div class="section" id="title">
<h1><a name="title">Title</a></h1>
<div class="section" id="not-a-subtitle">
<h2><a name="not-a-subtitle">Not A Subtitle</a></h2>
<p>Some stuff</p>
<div class="section" id="section">
<h3><a name="section">Section</a></h3>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h4><a name="another-section">Another Section</a></h4>
<p>And even more stuff</p>
</div>
</div>
</div>
</div>\\n''',
 'html_body': '''<div class="document">
<div class="section" id="title">
<h1><a name="title">Title</a></h1>
<div class="section" id="not-a-subtitle">
<h2><a name="not-a-subtitle">Not A Subtitle</a></h2>
<p>Some stuff</p>
<div class="section" id="section">
<h3><a name="section">Section</a></h3>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h4><a name="another-section">Another Section</a></h4>
<p>And even more stuff</p>
</div>
</div>
</div>
</div>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
["""\
* bullet
* list
""",
"""\
{'fragment': '''<ul class="simple">
<li>bullet</li>
<li>list</li>
</ul>\\n''',
 'html_body': '''<div class="document">
<ul class="simple">
<li>bullet</li>
<li>list</li>
</ul>
</div>\\n''',
 'html_head': '''...<title></title>\\n'''}
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
