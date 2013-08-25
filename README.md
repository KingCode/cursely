recursely
=======

*Move your recursive state from the stack to the heap*

Recursely is a small framework which enables the use of recursive style programming on a single stack frame with any 
traditional use of recursion, including any level of nested or mutually recursive functions. 

Functional programming languages already offer a great deal of flexibility and optimization techniques to allow recursive 
programming with constructs and functions such as trampoline and lazy evaluation, with a bit of work from the programmer. 

I try here to provide a one-stop shop for recursion optimization which does nothing else than (selectively) emulate
the call stack by "inlining it".

At the moment, recursely usage comes from adapting a traditional recursive function into a recursely compliant one - see below.
Although usable, manual adaptation is error prone and not user friendly - in the works is a macro API to do that. Also planned are
configurable memoization, and performance improvements from tuning the data structures storing the recursive state.

Current API is provided by recursely.ccore functions functions play, hval, hparam, hfn and hcall - see usage notes below. 

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

2) Function KS converted to recursely compliant adapted-KS - note the stack, pos prefixing the argument list, 
and calls to the public API functions: hval, hcall and rewind, as well as the order by which 
they are invoked:

    (defn adapted-KS [ stack pos coll capacity ]

      ;; base case, hoist value onto the stack 
      ;; and yield updated stack
      (if (empty? coll) (hval [stack pos] 0) ;; BASE CASE: use hval

        (let [ [c v] (-> (first coll) (#(vector (cost %) (value %)))) 
               tail (rest coll) ]

            (if (-> c (> capacity))

                ;; hoist onto the stack the next recurrent call and yield updated stack and cursor
                ;; note the use of rewind, without which the frame containing the function call 
                ;; will get lost.
                (-> (hcall [stack pos] adapted-KS 2 tail capacity) (rewind pos))

                ;; else, hoist in their evaluation order, nested recurrent and regular function calls, 
                ;; each one with number of args on the stack, and their resolved args if any; rewind,
                ;; and yield updated stack and cursor

                (-> (hcall [stack pos] adapted-KS 2 tail capacity) ;; INVOKE ADAPTED FN: use hcall
                    (hparam v)                                     ;; NESTED PARAM VALUE: use hparam
                    (hcall adapted-KS 2 tail (- capacity c)) 
                    (hfn + 2)                                      ;; INVOKE NON-ADAPTED NESTING FN: use hfn
                    (hfn max 2)                                    ;; ditto

                    ;; don't forget to rewind
                    (rewind pos))))))


3) Evaluate with:

    (recursely.ccore/play adapted-KS coll capacity)

See recursely.ccore-test for the full, and more examples.

Remarks
=======
1) A faulty adaptation will cause undeterminate behaviour and there is next to no error checking.

2) It is usually crucial that the order of evaluation in the original be preserved by the order of hfn/hcall invocationas: 
   therefore is it recommended that a post-order traversal of the source be preserved in the adaptation

3) The hcall/hfn/hval/hparam API functions all yield an updated state in [stack pos] and therefore it is important to 
   bear in mind that binding a result for reuse in different locations is not advisable unless the expected state is the
   same in both - this is a limitation, unlike native recursion where recurrent invocations represent a value and therefore
   can be reused anywhere.

TODOs: 

i) DSL API (currently working on recursely.core) 

ii) add configurable memoization 

iii) use transient/native stack implementation, or perhaps a faster data structure (tree? skip list?)

iv) configurable debugging/audit/exception facility.
