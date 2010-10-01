Check the new ``:parent:`` option to the class:: directive.

.. perl:: 
   sub red    { ".. class:: red\n   :parent:\n\n$_[0]" }

.. list-table::
   :header-rows: 1

   * - A
     - B
     - C
   * - .. class:: red
          :parent:

       abc
     - def
     - ghi
   * - .. class:: yellow

       cba
     - fed
     - ihg

* - D
  - E
  - F
* - .. class:: red
       :parent:

    jkl
  - mno
  - pqr
* - .. class:: yellow

    lkj
  - onm
  - rpq

====================== ============ =============
  abc                   .. class::   ghi
                           yellow
                           :parent:

                        def
.. perl::  red('cba')   fed          ihg
====================== ============ =============


.. raw:: html
   :head:

   <style type="text/css">
   .red    { background:red }
   .yellow { background:yellow }
   </style>
