Test Specifying Row and Entry Attributes
----------------------------------------

.. perl:: 
   $main::PARSER->{opt}{D}{row_attr}='valign="top"'; "";
   sub blue { $main::PARSER->{opt}{D}{entry_attr} = 'bgcolor=blue style="color:white"'; "" }
   sub red { $main::PARSER->{opt}{D}{entry_attr} = 'bgcolor=red style="color:white"'; "" }
   sub clear { delete $main::PARSER->{opt}{D}{entry_attr}; "" }
   sub bottom { $main::PARSER->{opt}{D}{row_attr}='valign="bottom"'; "" }

=========== ============= ============= =============
..	      **Average**                **Comment**
----------- --------------------------- -------------
..	     **Height**    **Weight**   .. perl::
                                           clear();
             .. perl::    .. perl::
                blue();      red();
=========== ============= ============= =============
**males**              11         0.003 stronger
**females**             9         0.002 *smarter*
=========== ============= ============= =============

+------------------------+------------+----------+----------+
| Header row, column 1   | Header 2   | Header 3 | Header 4 |
+========================+============+==========+==========+
| body row 1, column 1   | column 2   | column 3 | column 4 |
+------------------------+------------+----------+----------+
| body row 2             | Cells may span columns.          |
|                        |                                  |
| .. perl:: blue();      | .. perl:: red();                 |
+------------------------+------------+---------------------+
| body row 3             | Cells may  | - Table cells       |
|                        | span rows. | - contain           |
| .. perl:: bottom();    |            | - body elements.    |
| .. perl:: clear();     |            |                     |
+------------------------+            |                     |
| body row 4             |            |                     |
+------------------------+------------+---------------------+
