===================
 Manpage test area
===================

.. note::

  New semantic format mdoc https://mandoc.bsd.lv/

Additionally to standard testing, here we try to include tests that process
the generated man pages and verify the output.

The tests either require man or roff/nroff/... to verify the produced layout.

From the groff-manual at gnu.org
--------------------------------

5.1.6 Input Conventions

Since gtroff does filling automatically, it is traditional in groff not to try
and type things in as nicely formatted paragraphs. 

: this conflicts with reST because a reST-document is nicely formatted WYSIWYH.

These are some conventions commonly used when typing gtroff text:

* Break lines after punctuation, particularly at the end of a sentence and in
  other logical places. Keep separate phrases on lines by themselves, as entire
  phrases are often added or deleted when editing.

* Try to keep lines less than 40–60 characters, to allow space for inserting
  more text.

* Do not try to do any formatting in a WYSIWYG manner (i.e., don’t try using
  spaces to get proper indentation). 

5.1.3 Sentences

Although it is often debated, some typesetting rules say there should be
different amounts of space after various punctuation marks. For example, the
Chicago typesetting manual says that a period at the end of a sentence should
have twice as much space following it as would a comma or a period as part of
an abbreviation.

gtroff does this by flagging certain characters (normally ‘!’, ‘?’, and ‘.’) as
end-of-sentence characters. When gtroff encounters one of these characters at
the end of a line, it appends a normal space followed by a sentence space in
the formatted output. (This justifies one of the conventions mentioned in Input
Conventions.)

In addition, the following characters and symbols are treated transparently
while handling end-of-sentence characters: ‘"’, ‘'’, ‘)’, ‘]’, ‘*’, \[dg],
\[rq], and \[cq].

See the cflags request in Using Symbols, for more details.

To prevent the insertion of extra space after an end-of-sentence character (at
the end of a line), append \&. 


