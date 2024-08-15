File "include16.rst": example of rekursive inclusion.

.. include:: include15.rst

No loop when clipping before the "include" directive:

.. include:: include15.rst
   :end-before: .. include
