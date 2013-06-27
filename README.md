cursely
=======

Recursion anytime without eating the stack.

The intent is to facilitate the use of recursive programming without using the stack - something already available in 
functional programming using constructs and functions such as trampoline and lazy evaluation. I try here to generalize this
further by moving all recursion steps to call frames, and base values to value frames on an explicit stack data structure.

Also in the plans, a small DSL/macro to adapt a purely recurisve funciont in to one adapted to leverage the framework.

Initially in Clojure, but perhaps this can be done also in other functional languages where applicable.



