README: Nested Inline
^^^^^^^^^^^^^^^^^^^^^
:Author: Paul Tremblay

:Contact: phthenry@earthlink.net

:Date: $Date$

:Web site: http://docutils.sourceforge.net/

.. contents::

.. |script_name| replace:: docultils-nest.py

========
Overview
========

The script |script_name| in this module are my own extensions to docutils. They change
inline markup in an rst document to inline tags, allowing you to use rst to
create XML documents. For example, you might have the following markup in your
rst document::
  
  [:comment: Maybe I should include *The Sun Also Rises?*]

If you process this text with the |script_name|, you get::

  <inline arg1 = "comment">Maye I should include <emphasis>The Sun Also
  Rises?</emphasis></inline>

The nested_inline module allows you to choose the way you want to markup your
inline text in the orignal rst document. For example, you could use
parenthesis rather than brackets, or choose to place your roles outside the
group delimeters.  Details are below.


Installation
============

1. Install the PyXml package.

2. Run the configuration script to set the path and location of the configuration file.

   ::

     python configure.py target = <desired location of configuration file>

   If no target is provided (or you choose not to run the configuration file),
   the configuration file will be placed in /etc/nest_docutils.


3. Install the modules in the usual way:

   python setup.py build

   python setup.py install


Use
===

::

  |script_name| --output <outfile> file.rst

You *must* specify an output option.

In addition,  you can specify any options you would if running docutils-xml.py::

    |script_name| --indents --quiet --output otupt.xml file.rst


How to markup your document
===========================

By default, the nested_inline module uses the following structure to markup text::

  [:some-tag test example: text [:nested: more text [text with brackets]]]

This markup gets converted to::

  <inline arg1 = "some-tag" arg2 = "test" arg3 = "example">text <inline arg1 =
  "nested" more text [text with brackets]</inline></inline>

You begin inline markup with an opening bracket followed by a colon, with no
  space. You then fill in the arguments by typing in words after the colon. A
  colon marks the end of the beginning tag and begins the text. A closing
  bracket marks the end of the inline text.
  
Note that you can nest your markup to as many levels as you wish. You can also
use brackets just as you norally would and not worry that nest_inline will
confuse regular brackets with text meant to be treated as markup. However, the
brackets must be matched. Consider this markup:

[:math-formula-containing-closing-bracket: 2x ] ]

You might mean for the first closing bracket to be part of the formula, but
the nested_inline module will think it marks the end of your inline text. The
script will produce this output::

  <inline arg1 = "math-forumla-containing-closing-bracket">2x</inline><problematic
  description = "solitary closing bracket my produce output you don't want"/>]

In order to overcome this problem, use the following::

  [:math-forumula-containing-closing-bracket: 2x ``]`` ]

Errors
======

The module nested_inline will not produce invalid XML. If you write a file
that has the potential to result in ill-formed XML, nested_inline produces an
error message, along with an error tag. Consider the following document::


    .. If the script converted the brackets to tag, you would get

    .. <emphasis><inline arg1 = "markup">text</emphasis></inline>

    *[:markup: text*]

The result is::

    <emphasis><problematic descrition = "tagging text would result in ill-formed XML"/>[:markup: text</emphasis>]

Customizing Markup
==================

The nested_inline module reads from a customization file in order to determine how inline text is marked up. Change this document in order to change how you want to markup your text.

The configuration file takes 6 values:

- start-role: the way to mark the start of a role. The default is ":". 

- end-role: the way to mark the end of a role. The default is ":". 

- start-group: the way to mark the start of a group. The default is "[".

- end-group: the way to mark the end of a group. The default is "]".

- tag-name: the name of the inline tag. The default is "inline". 

- warning: the name of the warning messge for errors. The default is "problematic".

Let's say you wanted to markup your text using parenethesis rather than brackets, and that you wanted your role to go outside of the parenthesis. You want your tag name to be "inline-tag", and  your warning to be "warning." Your configuration file would look like this::


  <configure>
    <start-role string = ':'/>
    <end-role string = ':'/>
    <start-group string = '('/>
    <end-group string = ')'/>
    <place-of-role place = 'outside'/>
    <tag-name name = 'inline-tag'/>
    <warning name = 'warning'/>
  </configure>

You would then markup your text as::

  :comment:(This is a comment with *italics*)

The output would be::

  <inline-tag arg1 = "comment">This is a comment with <emhasis>italics</emphasis></inline-tag>

Project Files & Directories
===========================

* README.txt: You're reading it.

.. To be filled in later





Getting Help
============

Contact me.

..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
