Styling of Elements in Definition- or Field-List
================================================

:Authors: Hänsel, Gretel
:Address: 123 Example Street
          Example, EX  Canada
:Generic Docinfo List Field:
    - This is a list.
    - It does not require ``\leavevmode`` because it is in the docinfo.
:Abstract:
    Test that ``\leavevmode`` is inserted after the term or field-name
    when required for correct placement of the item.

Elements needing ``\leavevmode``
********************************

:Bullet List:
    - This is a bullet list nested in a field list.
    - It needs ``\leavevmode`` so that it will start on a new line
      after the term.
    - Without ``\leavevmode``, the first bullet would be on the same line as
      the term, and the following bullets would not line up.

:Enumerated List:
    1. This is an enumerated list.
    2. All lists need ``\leavevmode``.

:Field List:
    :Field List: Like this one
    :Needs ``\leavevmode``: Yes

:empty:

Definition List
    Nested
        inside another definition list.
    Needs ``\leavevmode``?
        Yes.
          Independent of the nesting level.

Option List
    -h   Show help
    -v   Be verbose
    --rare  a) This description starts with an enumeration
            b) but does not need ``\leavevmode``.

Literal Block
    ::

        _needs_leavevmode  = True

Doctest Block
    >>> needs_leavevmode(nodes.doctest_block)
    True

Line Block
    | Needs "``\leavevmode``",
    | so that all lines start with the same indent.

Block Quote
    ..

        Block Quotes need "``\leavevmode``", too,
        so that all lines start with the same indent.

Table
    +---+---+
    | 1 | 2 |
    +---+---+
    | 3 | 4 |
    +---+---+

Figure
    .. figure:: ../../../docs/user/rst/images/title.png

        A figure

Image
    .. image:: ../../../docs/user/rst/images/title.png

Rubric
    .. rubric:: A Rubric

Admonition
    .. note::

        Admonitions need to be preceded by ``\leavevmode``.
        Otherwise, the term ends up centered above the admonition box.

        So do *System Messages*, as they use the "DUadmonition"
        LaTeX environment.


Elements not needing ``\leavevmode``
************************************

Paragraph
    Paragraphs don't need ``\leavevmode``.  They are meant
    to start after the term and have a hanging indent.

    * Subsequent elements don't need ``\leavevmode`` either.

Math Block
    .. math::

        \sum_{i=1}^n i = \frac{n^2+n}{2}

    LaTeX starts math blocks (both single-line and multiline) in a new
    paragraph automatically, with or without ``\leavevmode``, so
    ``\leavevmode`` isn't needed.

Term with Classifier : classifier
    - After a *classifier*, ``\leavevmode`` is not required
    - because the classifier is added (in parentheses) after the term.

Ambiguous cases
***************

Comment and Target
    .. This is ignored.

    .. _foo:

    * Comments and other "Invisible" nodes (substitution definitions,
      targets, pending) must be skipped when determining whether a
      ``\leavevmode`` is required.

Substitution Definition and Class directive
    .. |no-leavevmode| replace:: No (because a paragraph follows).

    .. class:: test

    Is ``\leavevmode`` required?  Answer: |no-leavevmode|

Compound
    .. compound::

       `Compound` and `Container` wrap around other block elements.
       They get a ``\leavevmode``, if the first nested element is a
       list or similar.

Container
    .. container:: my-class

        * This list inside a container requires a ``\leavevmode``.

Footnote
    .. [#f1] This footnote will move to the bottom of the page.

    A ``\leavevmode`` is required, if the first list item value is a
    footnote and a list or similar follows.

Citation
    .. [example73] No Name, "Citations move to the bottom as well",
       Musterstadt, 1973.

    * A ``\leavevmode`` is required, if the first list item value is a
      citation and a list or similar follows.

Raw Block Text
    .. raw:: latex

        “Raw” blocks are always preceded by
        \verb|\leavevmode|, just in case.
