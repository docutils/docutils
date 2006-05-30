Tables with Different Attributes
================================

Here is a table with the default attributes.

======= ==============
Honey   Grocer
Hammer  Hardware Store
Piano   Music Store
======= ==============

The following simple table should get created with no attributes.

.. perl:: $main::opt_D{'tableattr'} = "";

======= ============
Bach    Prelude in C
Brahms  Lullaby
Holst   The Planets
======= ============

The following grid table should get created with a border 10, a pink
background, and no wrapping.

.. perl:: $main::opt_D{'tableattr'} = 'border="10" nowrap bgcolor="pink"';""

+---+----------------------------------------------------------------------------------------------------------------------------------------------+
| 1 | WHEN in the Course of human Events, it becomes necessary for one People to dissolve the Political Bands which have connected them with       |
|   +----------------------------------------------------------------------------------------------------------------------------------------------+
|   | another, and to assume among the Powers of the Earth, the separate and equal Station to which the Laws of Nature and of Nature's God entitle |
|   +----------------------------------------------------------------------------------------------------------------------------------------------+
|   | them, a decent Respect to the Opinions of Mankind requires that they should declare the causes which impel them to the Separation.           |
+---+----------------------------------------------------------------------------------------------------------------------------------------------+
