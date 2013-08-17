(ns recursely.types
 (:refer-clojure :exclude [pop peek remove replace])
    (:use clj-utils.stackp))

(comment 
 " Protocols and data types which define how a stack moves from one state to the next.
")

;;"Marks and stores a 'pure' value, i.e. with no recursely framework semantics."
(deftype Value [ value ]
    Object
    (toString [this] (str value)))

;;"Marks a recurrent function invocation, or a function call nesting a recurrent function,
;;  and stores the function and number of arguments."
(deftype Call [ fn, numargs]
    Object
    (toString [this] (str "func='" fn ", numargs=" numargs)))

;;"Marks a pure function, i.e unware of recursely semantics"
(deftype Fn [ fn, numargs]
    Object
    (toString [this] (str "func='" fn ", numargs=" numargs)))

(defprotocol Move
   "A function which knows how to yield a new stack and cursor position from its inputs"
    (step 
       [ this stack cursor ]
       "Determines what to do with the stack element at cursor position and yields the resulting
        state"))


