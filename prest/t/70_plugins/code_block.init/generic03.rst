Generic code with lots of markup 

.. code-block:: generic
   :color:
   :states-flags: -D use_functions=1 \
      -D function_string='([ \t]*[_a-zA-Z]+[ \t]+)([^ \t]+)([ \t]*[\(])' \
      -D function_format='type function_name none' \
      -D use_pp_lines=1 -D pp_char='\+' \
      -D use_dq_strings=1 -D use_sq_strings=1 -D use_bq_strings=1 \
      -D use_c_comments=1 \
      -D use_line_comments=1 -D line_comment_string='#' \
      -D use_keywords=1 -D keyword_string='or|while' \
      -D use_types=1 -D type_string='addbit' \
      -D use_references=1 -D reference_string='\+[a-zA-Z][a-zA-Z0-9_]*' \
      -D use_variables=1 -D variable_string='$[a-zA-Z]+'

   +include "funnyfile"
   /* This is a multi-line
      c-comment
    */
   # Single-line perl comment
   addbit	g1__0.cor ("a string");
   addbit  g1__1.cor (+abC_123);
   while ($a == 1 or $b == 2) {
     message('String') < `ls -l`;
   } 

