recursely
=======

"Move a recursive descent's state from the JVM stack to the heap": recursely is a small framework which enables the 
use of recursive style programming on a single frame stack with any traditional use of recursion, including any level of
nested or mutually recursive multiply defined functions. 

Functional programming languages already offer a great deal of flexibility and optimization techniques to allow recursive 
programming with constructs and functions such as trampoline and lazy evaluation, with a bit of work from the programmer. 

I try here to provide a one-stop shop for recursion optimization which does nothing else than (selectively) emulate
the call stack by "inlining it".

At the moment, recursely usage comes from 'adapting' a traditional recursive function into a recursely compliant one - see usage below
and comments in /src/recursely/recursely.ccore.clj. 

Adapting a recursive function requires awareness of the order of invocation of recurrent calls, and knowledge of the 
'rules of the game': the goal is to provide eventually a macro which does the work, e.g. using a simple variable markup vocabulary.


Usage
=====
Here is an example of a recursive knapsack function with recursive calls nested within regular functions, followed by 
its recursely adaptation and usage:

    (defn KS [ coll, capacity ]
      (if (empty? coll) 0
        (let [ [c v] (-> (first coll) (#(vector (cost %) (value %)))) 
               tail (rest coll) ]
           (if (-> c (> capacity))
               (KS tail capacity)
                (max (KS tail capacity)  (+ v (KS tail (- capacity c)))))))) 

2) Function A onverted to recursely compliant APrime  - note the stack, pos prefixing the argument list, 
and calls to the public API functions: hval, hcall and rewind, as well as the order by which 
they are invoked:

    (defn adapted-KS [ stack pos coll capacity ]

      ;; base case, hoist value onto the stack 
      ;; and yield updated stack
      (if (empty? coll) (hval [stack pos] 0)

        (let [ [c v] (-> (first coll) (#(vector (cost %) (value %)))) 
               tail (rest coll) ]

            (if (-> c (> capacity))

                ;; hoist onto the stack the next recurrent call and yield updated stack and cursor
                ;; note the use of rewind, without which the frame containing the function call 
                ;; will get lost.
                (-> (hcall [stack pos] adapted-KS 2 tail capacity) (rewind pos))

                ;; else, hoist in their evaluation order nested recurrent and regular function calls, 
                ;; each one with number of args on the stack, and their resolved args if any; rewind,
                ;; and yield updated stack and cursor
                (-> (hcall [stack pos] adapted-KS 2 tail (- capacity c)) 
                    (hfn + 2 v)
                    (hcall adapted-KS 2 tail capacity)
                    (hfn max 2)

                    ;; don't forget to rewind
                    (rewind pos))))))


3) Evaluate with:

    (recursely.play adapted-KS coll capacity)

See recursely.ccore-test for full example

Remarks
=======
1) A faulty adaptation will cause undeterminate behaviour and there is next to no error checking.

2) It is crucial that the order of evaluation in the original be preserved by the order of hfn/hcall invocations.

3) Keep it simple. Something to bear in mind when adapting a recursive function to recursely: making recursive 
calls within let bindings and such works fine - however this is error prone with adapted code, 
e.g. in the knapsack example above we can reduce code redundancy a bit in the original function 
with the use of local nextKS: 

    (defn KS [ coll, capacity ]
      (if (empty? coll) 0
        (let [ [c v] (-> (first coll) (#(vector (cost %) (value %)))) 
               tail (rest coll) 
               nextKS (KS tail capacity) ]
           (if (-> c (> capacity))
                nextKS
                (max nextKS  (+ v (KS tail (- capacity c)))))))) 

However in the adapted function nextKS represents a state, i.e. a snapshot [ tstack tpos ]
at that moment - as each hcall/hfn action requires the updated state, leaving nextCall 
in the let binding will actualy cause more code rework and harm readability:

    (defn adapted-KS [ stack pos coll capacity ]
      (if (empty? coll) (hval [stack pos] 0)
        (let [ [c v] (-> (first coll) (#(vector (cost %) (value %)))) 
               tail (rest coll) 
               nextCall (hcall [stack pos] adapted-KS 2 tail capacity) 
            ]
            (if (-> c (> capacity))
                (rewind nextCall pos)
                (-> (hcall [stack pos] adapted-KS 2 tail (- capacity c)) 
                    (hfn + 2 v)

                    ;; nextCall useless here, won't even compile, 
                    ;; so copy from let:
                    (hcall [stack pos] adapted-KS 2 tail capacity) 

                    (hfn max 2)
                    (rewind pos))))))

TODO: 1) use transient/native stack implementation
      2) macro API
      3) configurable debugging/audit/exception facility ; macro API.
