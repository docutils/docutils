.. sectnum::

Introduction
************

This is a test file for the index writer for reStructuredText.
An _`index entry` is either an _`inline target`, or an _`indirect target`
pointing to an inline target.

.. _overview:

Overview of reStructuredText
============================

.. _indirect2: `indirect target`_

What is _`reStructuredText`? Find the answer 
`here <http://docutils.sourceforge.net/rst.html>`_.

The above link is defined as an _`external target`, therefore not
consider an `index entry`_.

Test Cases
**********

Here are some _`tests` for the index writer.

Test 1
======

Test 1 is about inline targets. Here are _`my inline target1`
and _`$$my fancy inline target2`, both considered indices.
Did you know that _`@indices@` is the plural of _`index`?

Test 2
======

.. _`2nd indirect target`: `indirect target`_
.. _`another indirect target`: `overview`_

Test 2 is about indirect targets. The `2nd indirect target`_ is an 
indirect target to `indirect target`_. It is considered an index
entry. Even though it's defined in this section, the index entry
for it will point to the section where `indirect target`_ is 
defined.

Here is `another indirect target`_. It's pointing to 
`overview`_, which is not an inline target. Neither `overview`_ nor
`another indirect target`_ will be included in the index file.

.. _perl: http://www.perl.org
.. _`Perl, a handy language`: perl_

The index writer is written in `Perl`_. `Perl, a handy language`_ is
an indirect target pointing to `Perl`_. Neither is considered index.

There is another indirect target in this file, namely `indirect2`_. 
It's pointing to `indirect target`_ and is considered an index entry.

Test 3
======

Sub-test 1
----------

Let's try some inline targets defined in list, such as the following
ones:

+ _`yellow` is nice
+ _`green` is pretty
+ _`blue` reminds me of sky and ocean


Sub-test 2
----------

|sub|. Do you know the previous sentence is actually defined in a 
substitution? The substitution contains an inline target, but such
usage is not recommended.

.. |sub| replace:: What about _`this inline target`?

Sub-test 3
----------

.. |smiley face| image:: smiley.gif

This _`smiley face` |smiley face| is defined in a substitution.

The previous paragraph just made |smiley face|_ a pointer!

Results
*******

All the tests passed! Is the definition for _`passed` in eyes of
the test writer? ``;)``

