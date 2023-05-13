Package rsm.gen-prog is a genetic programming package. It is not 
general purpose yet. Files example1.lisp and example2.lisp show what is 
needed currently to set up a genetic programming problem.


To run the examples: example.lisp and example2.lisp you must do:


1). Run an example: Do either
   a). (load "example1.lisp")
       Or, if compiled do (load "example1")
       This example tries to reproduce a boolean function of 6 variables.
       It bases its success on a set of 28 input points.
       Problems of this sort are probably the hardest thing for genetic
       programming algorithms to do.

   Or,

   b). (load "example2.lisp")
       Or, if compiled do (load "example2").
       This example tries to find a formula for cos(2x) using only
       +, - , *, % (a kind of divide that is safe), and the sin function.

       Armed with these building blocks, it should find a formula 
       equivalent to one of these:
       sin(2x + pi/2)
       Or,
       1 - 2 * sin(x) * sin(x)

      The formula found will, in general, be quite complicated, as it does 
      not have access to all of the real constants to use in its formulas.
      It will use randomly generated constants (generated initially) to 
      compose a formula.





   
