(ns recursely.ccore
  (:refer-clojure :exclude [pop peek remove replace])
  (:require [clj-utils.coll :as coll :only [find-index] :exclude [replace]])
  (:use recursely.types
        [clj-utils [stackp :as stp] [core :as cu]])
  (:import [recursely.types Value Fn Call])
  (:import [clj_utils.stackp DefaultStack+]))

(comment
"Assists in implementing a recursively defined algorithm as a non-recursive function which maintains
 recursive state solely through its arguments. Intended usage is for client code to store state changes
 not as recursive invocations but by modifying the stack by invoking utility functions hval, hcall or hfn
 and passing the adapted function to play (as in 'Play it again, Sam.').

 An adapted function can invoke either non-recursive functions, or other adapted recursive function(s).

 The expected usage is the following:

 1) Convert a recursive fn A, e.g.
        (defn A [ coll, capacity ]
          (if (empty? coll) 0
            (let [ tuple (first coll) 
                   nextA (A (rest coll) capacity) 
                   c (cost tuple)
                   v (value tuple) ]
               (if (-> c (> capacity))
                    nextA
                    (max nextA (+ v (A coll (- capacity c))))))))

   to fn APrime which always and only returns a [newstack, newpos]:

        (defn APrime [ stack pos coll capacity ]
          (if (empty? coll)  (-> (hval stack pos 0) (rewind pos))

              (let [ tuple (first coll)
                   nextAPrime (hcall [stack pos] Aprime (rest coll) capacity) 
                   [tstack tpos] nextAPrime
                   c (cost tuple)
                   v (value tuple) ]

                  (if (-> c (> capacity))

                    ;; yield updated stack  
                    (-> nextAprime (rewind pos))

                    ;; hoist unresolved calls, by order of nesting depth 
                    (-> (hcall [tstack tpos] APrime 2 coll (- capacity c))
                        (hfn + 2 v)
                        (hfn max 2)

                        ;; don't forget to rewind!

                        (rewind pos))))))

    NOTE that the output stack MUST have its cursor pointing to pos, in order for
    the framework to process correctly all additions made to the stack during the 
    client function's invocation (APrime here) - or use rewind as in the above
    example, for convenience.

2) 'Play it again, Sam':
   (play Aprime coll capacity)
")


(def ^:private s+ "The Stack+ implementation used to manipulate the stack"
   (DefaultStack+.)) 


(defn rewind
"Resets the cursor to pos. Yields [stack topos].
"
[ [stack pos] topos]
  [stack topos])


(defn hval 
"Hoists a value on the stack. Yields a  [newstack newpos] tuple from inserting value 
 inserted at pos in stack. IMPORTANT: the stack output by the client function must point 
 to the first frame added by either hval or hcall during the invocation which called this
 function, i.e. the cursor (pos) value in its argument - see module comments.

 Implementation details: the returned position is the next insert point for further calls to 
 hval/hcall, or for processing of the stack elsewhere - consistently with positioning
 done by hcall. Also, we want to avoid as much as possible having the client code
 do any manipulation beyond pointing out what needs to be managed. It is up to 
 a higher level to rewind the stack to the correct position after a series of hval/hcall
 invocations within a logical step.

 Note that the choice of a tuple to hold stack and pos is motivated by
 making it easier for client code to chain hval's and hcall's.
"
[ [stack pos] v ] 
  [(->> [(Value. v)] (insert s+ stack pos)), (inc pos) ])


(defn hparam
"Hoists a parameter value on the stack and yields the resulting [newstack newpos].
 Equivalent to (hval s&p v), except that the returned cursor points past the added element
 to allow for the next insertion. s&p is a [stack pos] tuple.
"
[ s&p  v ]
 (let [ [stack pos] (hval s&p v)]
    [stack pos]))


(defn hoist-fn
"See comments to hcall. Implements hcall/hfn with the proper type as per
 is-Call?.
"
[ [stack pos] is-Call? f numargs & args ]
  (let [ 
        object (if is-Call? (Call. f numargs)
                        (Fn. f numargs))
         vals (if (and (= 0 numargs) (empty? args)) args 
                    (map #(Value. %) args))
       ]
       #_(println "hoist-fn: F=>" f "<, Call/Fn object =>" object)
    [
        ;; stack with f and args inserted

        (cu/thread-it
         object
         (apply conj [] it vals) ;; wrapping re: protocols don't like variadic args
         (insert-b s+ stack pos it)), 

    ;; increment pos by the number of inserted elements
    ;; (add one for the Call frame)

        (+ pos 1 (count args))
     ]))


(defn invoke
"Yields the result of invoking macro or function referenced by mf-sym.
 mf-sym should be a fully qualified symbol.
"
([ mf-sym args]
  (let [ f (eval `'~mf-sym) 
         form (cons f args) ]

   #_(println "invoke: FORM =>" form "<")
    (condp cu/is-case f
        special-symbol? (eval form)
        macro?  (-> form macro-call eval)
        ifn? (eval form))))
([ mf-sym stack pos args ]
  (invoke mf-sym (concat [stack pos] args))))


#_(defn preprocess
"Yields a tuple of elements counterpart to each argument, after preprocessing
 f as a macro if applicable."
[ [stack pos] f numargs args ]
  (if (macro? f)
      (process-macro [stack pos] f numargs args))
      [ [stack pos] f numargs args])


#_(defmacro hcall 
"Preprocesses args if f is a macro, otherwise invokes hcall-impl with same args.
"
[ [stack pos] f numargs & args ]
  `(apply hcall-impl (preprocess [~stack ~pos] '~f ~numargs '~args)))


#_(defmacro hfn
"Preprocesses args if f is a macro, otherwise invokes hcall-impl with same args.
"
[ [stack pos] f numargs & args ]
  `(apply hfn-impl (preprocess [~stack ~pos] '~f ~numargs '~args)))



(defn hcall
"Hoists a function call on the stack.
 Yields a [newstack newpos] tuple with args and f inserted at pos, and f 
 marked to be invoked with numargs values on the stack. Note that numargs
 is required because (count args)  may be different from numargs when 
 (- numargs args) of f's arguments are unresolved call themselves, 
 e.g. when some the hoisted function's arguments are provided by Values
 hoisted externally to the client invocation which placed the current call.

 Also hcall invocations should NOT be nested lexically and args must be values 
 not for consumption by the framework (any evaluated var from recursely, or meta values
 such as stack and pos or modifications thereof). 

 IMPORTANT: the stack output by the client function MUST point to the first frame added by 
 either hval or hcall during the invocation which called this function, i.e. the cursor (pos) value 
 in its argument - see module comments for an example.

 Implementation details: the element at pos prior to insertion occupies the 
 first slot after the last of the inserted elements; and the insertion 
 occurs in reverse order, e.g. 
    (hcall + 1 2) 
 yields an insert [... 2 1 f ..]

 Finally, the cursor is pointed at the new position of the element formerly
 at pos, or the size of the new stack if the insert occurred at the end.
 This allows automatic positioning for either the next hcall/vcall, or
 processing of the stack elsewhere.

 Note that the choice of a tuple to hold stack and pos is motivated by
 making it easier for client code to chain hval's and hcall's.
"
[ [stack pos] f numargs & args ]
    (apply hoist-fn [stack pos] true f numargs args))


(defn hfn
"Same as for hcall, except that the hoisted function is not aware of the stack.
 Use this for any function other than a client (recursely-adapted) function.
"
[ [stack pos] f numargs & args ]
    (apply hoist-fn [stack pos] false f numargs args))


(declare ready-args? pop-call extract-fn)

(defn extract-fn
"Extracts a Fn or Call's function and arguments from the stack, removes them from the stack 
 and points the stack to the next insertion point. Yields a tuple of [[newstack newpos] func args].
 Fn/Call frame must be the elements at pos on stack. If there are missing or unresolved arguments
 an exception is thrown.
"
[ stack pos ]
  (let [ frame (nth stack pos)
         f (.fn frame)  
         numargs (.numargs frame)
         args (stp/peek-rnb s+ stack (dec pos) numargs)
         ready-args? (-> (coll/find-index #(not (= Value (class %))) args) nil?)
      ]

    (if  (not ready-args?)
       (throw (Exception. 
            (str "Args unresolved: function=" f ", numargs=" numargs
                ", args=" args ", cursor=" pos
                ", stack=" stack)))
         [ (pop-call stack pos numargs)
           f 
           (map #(.value %) args) ])))

(extend-protocol Move
  Value
    (step [this stack pos]  [stack (inc pos)])

  Fn
    (step [this stack pos]
       (let [ [[tstack tpos] f args] (extract-fn stack pos) ]
            [(insert s+ tstack tpos [(Value. (apply f args))]), 
            ;;[(insert s+ tstack tpos [(Value. (invoke f args))]), 
                tpos] ))
  Call
    (step [this stack pos] 

        ;; Retrieve args and func, and if args are resolved (all are Value's)
        ;; remove them from the stack, point the cursor to the insertion point, 
        ;; and invoke func with updated stack/pos and args. 
        ;; Otherwise move cursor to first Call frame which is not a value.

        (let [ [[tstack tpos] f args] (extract-fn stack pos) ]
                    (apply f tstack tpos args))))


(defn pop-call
"Removes Call element at pos and all numargs preceding elements, 
 and yields the new stack with cursor pointing to the element formerly
 at (inc pos).
 "
[ stack pos numargs ]
  (let [ numless (inc numargs) ]
    [ (remove-rnb s+ stack pos numless), 
      (-> pos (- numless) inc) ]))


(declare next-state end? extract newstack)

(defn again
"Coordinates changes to stack and yields the final result value."
[ stack pos ] 
  (if (end? stack pos) (.value (first stack))
        (let [ [newstack newpos] (next-state stack pos) ]
            #(again newstack newpos))))


(defn play
"Boots execution after preprocessing f and args."
[ f & args ]
  ( let [ [stack pos] (apply newstack f args) ]
    (trampoline again stack pos)))


(defn newstack
"Yields a vector initialized with args in their reverse argument order
 and ending with f.
"
[ f & args]
  (let [ numargs (count args) ]
      [ (-> (map #(Value. %) args)
                    reverse
                    vec
                    (conj (Call. f numargs)))
        , numargs ]))
     

(defn next-state
"Yields a new [stack pos] state from the current one, based on the type
 of the element at pos.
"
[ stack pos ]
    (step (nth stack pos) stack pos))


(defprotocol End
   "A workaround to the setback that a deftype's class does not respond
    to the equals test expectedly, e.g. with 
       (= Value (class (first [(Value. 1)]))) ;;=> false
       (= (class (Value. 1)) (class (Value. 1))) ;; => true")

(extend-protocol End Value)    


(defn end?
"Yields true if stack has a single element which is a Value.
"
[ stack pos ]
  (and (= 1 (count stack))
       ;; see End protocol comment above
       #_(= Value (class (first stack)))
      (extends? End (class (first stack)))))

