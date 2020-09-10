File "include16.txt": example of rekursive inclusion.

.. include:: include15.txt

No loop when clipping before the "include" directive:

.. include:: include15.txt
   :end-before: .. include
