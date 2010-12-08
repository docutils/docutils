Check the new ``:parent:`` option to the class:: directive.

.. perl:: 
   sub red    { ".. class:: red\n   :parent:\n\n$_[0]" }

.. list-table::
   :header-rows: 1

   * - .. class:: yellow
          :parent: row

       A
     - B
     - C
   * - .. class:: red
          :parent:

       abc

       .. class:: blue
          :parent:

       cba
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
       :parent: bullet_list 1

    jkl

    mno
  - .. class:: blue
       :parent: list_item 2

    pqr
  - stu
* - .. class:: yellow

    lkj

    onm
  - .. class:: black
       :parent: not_found      

    rqp
  - uts

====================== ============ =============
  abc                   .. class::   ghi
                           yellow
                           :parent:

                        def
.. perl::  red('cba')   fed          .. class:: blue
                                        :parent: row

                                     ihg
====================== ============ =============


.. raw:: html
   :head:

   <style type="text/css">
   .red    { background:red }
   .blue   { color:blue }
   .yellow { background:yellow }
   </style>
