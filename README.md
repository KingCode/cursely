recursely
=======

A utility framework which manages the state represented by parameters as return values between repeated function invocations.

The goal is to provide a facility which can morph any recursive function (including any number of recursive functions) into 
a non-recursive one, with minimal change.

Functional programming languages already offer a great deal of flexibility and optimization techniques to allow recursive 
programming with constructs and functions such as trampoline and lazy evaluation, with a bit of work from the programmer. 
I try here to provide a one-stop shop for recursion optimization which does nothing else than emulate the call stack by "inlining it".
