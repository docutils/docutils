Another test of function markup.

.. code-block:: generic
   :color:
   :states-flags: -D use_functions=1 \
      -D function_string='\b(sub)([ \t]+)([a-zA-Z_]+)' \
      -D function_format='keyword none function_name' \
      -D use_keywords=1 -D keyword_string='return|sub' \
      -D use_dq_strings=1

   sub	 mysub {
     return sub { "anonymous sub" }
   }
