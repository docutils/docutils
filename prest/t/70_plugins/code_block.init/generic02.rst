Generic code with default values

.. code-block:: generic
   :color:
   :states-flags: -D use_line_comments=1 -D use_pp_lines=1 -Duse_functions=1

   #ifdef FOO
   // A default function definition:
   void
   bar (baz) {
   }
   #endif // FOO
