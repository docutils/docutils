#! /usr/bin/env python

# Author: reggie dugard
# Contact: reggie@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Test for fragment code in HTML writer.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.HtmlFragmentTestSuite()
    s.generateTests(totest)
    return s


totest = {}

totest['Title promotion'] = ({}, [
    ["""\
Simple String
""",
     """\
Simple String"""
     ],
    ["""\
Simple String with *markup*
""",
     """\
Simple String with <em>markup</em>"""
     ],
    ["""\
Simple String with an even simpler ``inline literal``
""",
     """\
Simple String with an even simpler <tt class="literal"><span class="pre">inline</span> <span class="pre">literal</span></tt>"""
     ],
    ["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
     """\
<p>A simple <a class="reference" href="http://www.test.com/test_url">anonymous reference</a></p>
"""],
    ["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
     """\
<p>A simple <a class="reference" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
"""],
    ["""\
+++++
Title
+++++

Some stuff

Subtitle
--------

Some more stuff

Another Section
...............

And even more stuff
""",
     """\
<h1 class="title">Title</h1>
<p>Some stuff</p>
<div class="section" id="subtitle">
<h1><a name="subtitle">Subtitle</a></h1>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h2><a name="another-section">Another Section</a></h2>
<p>And even more stuff</p>
</div>
</div>
"""]
    ])

totest['No title promotion'] = ({'doctitle_xform' : 0}, [
    ["""\
Simple String
""",
     """\
Simple String"""
     ],
    ["""\
Simple String with *markup*
""",
     """\
Simple String with <em>markup</em>"""
     ],
    ["""\
Simple String with an even simpler ``inline literal``
""",
     """\
Simple String with an even simpler <tt class="literal"><span class="pre">inline</span> <span class="pre">literal</span></tt>"""
     ],
    ["""\
A simple `anonymous reference`__

__ http://www.test.com/test_url
""",
     """\
<p>A simple <a class="reference" href="http://www.test.com/test_url">anonymous reference</a></p>
"""],
    ["""\
A simple `named reference`_ with stuff in between the
reference and the target.

.. _`named reference`: http://www.test.com/test_url
""",
     """\
<p>A simple <a class="reference" href="http://www.test.com/test_url">named reference</a> with stuff in between the
reference and the target.</p>
"""],
    ["""\
+++++
Title
+++++

Some stuff

Subtitle
--------

Some more stuff

Another Section
...............

And even more stuff
""",
     """\
<div class="section" id="title">
<h1><a name="title">Title</a></h1>
<p>Some stuff</p>
<div class="section" id="subtitle">
<h2><a name="subtitle">Subtitle</a></h2>
<p>Some more stuff</p>
<div class="section" id="another-section">
<h3><a name="another-section">Another Section</a></h3>
<p>And even more stuff</p>
</div>
</div>
</div>
"""]
    ])

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
