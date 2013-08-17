(ns recursely.core
  (:use recursely.ccore 
        [clj-utils [core :only [thread-it]]]))

(comment "API which transforms a recursive function into a non-recursive one, e.g. 
    (defn KS [ coll, capacity ]
      (if (empty? coll) 0
        (let [ [c v] (-> (first coll) (#(vector (cost %) (value %)))) 
               tail (rest coll) ]
           (if (-> c (> capacity))
               (KS tail capacity)
                (max (KS tail capacity)  
                     (+ v (KS tail (- capacity c))))))))
        
can be converted using the prefix $<value>, $<func name><numargs>, $<adapted-func name><numargs>
for output values, non-adaptive (regular) function invocations, and adaptive (recurrent) function invocations
respectively, where <func, adapted-func name> is a declared function symbol, <numargs> is the number of arguments
for the associated function in the adapted code, and <value> is a form evaluated at runtime,
e.g.

    (defrecursely KSprime [ coll capacity ]
        (if (empty? coll)  $0
          (let [ [c v] (-> (first coll) (#(vector (cost %) (value %)))) 
                 tail (rest coll) ]
            (if (-> c (> capacity))
                (KS tail capacity)
                ($f2max (KS tail capacity)
                        ($f2+ v (KS tail capacity) (- capacity c)))))))

creates a adapted function KSprime which takes the same parameters and yields the same value as KS, 
but does not consume the stack. Not implemented"

Two or more mutually recursely functions can be defined at once:

    (defrecursely func-specs*)
where each func spec is a name-args-body
"
)

