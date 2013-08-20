(ns recursely.core
  (:use recursely.ccore 
        [clojure [walk]]
        [clj-utils [core :only [thread-it]]]))

(comment "(Not implemented) API which transforms a recursive function into a non-recursive one, e.g. 
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
but does not consume the stack. 

Two or more mutually recursely functions can be defined at once:

    (defrecursely func-specs*)
where each func spec is a name-args-body
"
)


(defn funcvar
"Yields the var for a macro or function symbol.
"
[ sym ]
  (->> sym str (str "#'") read-string eval))


(defn macro? 
"Yields true if form has a macro in function position, false otherwise.
"
[ form ]
  (-> (first form) funcvar meta :macro true?))



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


#_(defn adapt-1
"Yields an adapted form from a top-level marked up form."
[ form ]
  (condp))

  


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

  
