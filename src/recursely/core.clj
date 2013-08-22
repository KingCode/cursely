(ns recursely.core
  (:use recursely.ccore 
        [clojure [walk]]
        [clj-utils [core :only [thread-it macro-call funcvar macro? is-case]]]))

(comment "(Not implemented) API which transforms a recursive function into a non-recursive one, e.g. 
    (defn KS [ coll, capacity ]
      (if (empty? coll) 0
        (let [ [c v] (-> (first coll) (#(vector (cost %) (value %)))) 
               tail (rest coll) ]
           (if (-> c (> capacity))
               (KS tail capacity)
                (max (KS tail capacity)  
                     (+ v (KS tail (- capacity c))))))))
        
can be converted by enclosing the function in the defrecursely macro; prefixing end of recursion 
values (base cases) with $; and doing the same with the token in function position of each form yielding a result
other than base cases, e.g.

    (defrecursely KSprime [ coll capacity ]
        (if (empty? coll)  $0                                     ;; BASE CASE VALUE
          (let [ [c v] (-> (first coll) (#(vector (cost %) (value %)))) 
                 tail (rest coll) ]
            (if (-> c (> capacity))
                ($KS tail capacity)                               ;; RETURN POINT 1
                ($max (KS tail capacity)  
                        (+ v (KS tail capacity) (- capacity c)))  ;; RETURN POINT 2
                                                            ))))

creates a adapted function KSprime which takes the same parameters and yields the same value as KS, 
but does not consume the stack. 

Two or more mutually recursely functions can be defined at once:

    (defrecursely func-specs*)
where each func spec is a name-args-body
"
)


(defn strip
[ form ]
  (let [ ftok (apply str (-> (first form) str rest)) ]
      `( ~(symbol ftok) ~@(rest form))))


(defn parse-markup
"Strips markup from function positions in forms within body, storing transformed
 forms as keys in a may, and yields a tuple of the new markup in pos. 0, and the 
 map."
[ body ]
  (let [ regs (atom {})
         update (fn [form value] (swap! regs #(merge % {form value}))) ]
           [ (prewalk #(if (and (list? %) 
                       (-> (first %) str (.startsWith "$")))
                                (let [ stripped (strip %) ]
                                            (update stripped {}) stripped) 
                                                %) body)
              ,@regs ]))


(defn hoist-form
"Hoists a function call onto the stack, either as a recursely function call if found in regs,
 or a regular function call otherwise.
"
[ regs form ])



#_(defn adapt-1
"Yields an adapted form from a top-level marked up form; regs is a set of
 registered function symbols"
[ regs form ]
  (if (list? form)
      (condp is (first form)
            special-symbol? (hoist-special regs form)
            macro? (hoist-macro regs form)
            ifn? (hoist-func regs form)
            :else form)
       form))



(defn- parsefor-numargs
"Yields a map of each list form in body with the number of args for its var 
 in function position."
[ regs body ]
(let [ form2mumargs (atom {}) ]
  #_(prewalk #(do (if (list? %) (swap! form2numargs 
                                    (fn [x] (assoc x % (dec (count %)))))) %)
                                                                            body)
  (postwalk (fn [ x ] (if (list? x) 
                          (condp instance? (first x)))))))
  


(defn- adapt-1
[ regs name args-body ]
    (let [ body (drop 1 args-body) ]))
       

;;args, body copied from clojure.core_deftypes
(defn- parse-specs
[specs]
  (loop [ret {} s specs]
      (if (seq s)
            (recur (assoc ret (first s) (take-while seq? (next s)))
                         (drop-while seq? (next s)))
                               ret)))


;;body copied from clojure.core_deftypes
#_(defn- emit-adapted [specs]
  (let [impls (parse-impls specs)
        registered (set (keys impls)) ]
      `(do
             ~@(map (fn [[name arities]]
                             `(adapt registered ~name ~@arities ))
                                           impls))))


(defn hoist-spec
"Yields a map of symbols from form to maps of {:type <hfn, hcall, nil> :numargs <positive int> :val}
 for all symbols in form requiring hoisting by recursely.
 "
 [ form ]
 )

 (defn register-funcs
 "Yields a set of function symbols from func-specs with each func spec is a (funcname, ([& args] body)*)
 "
 [ func-spec & func-specs ]
 )

(defmacro defrecursely
"Creates a recurseldeclared functiony adapted function for each spec, where spec is a (name ([& args] body)*)."
[ spec & specs ] )

  
