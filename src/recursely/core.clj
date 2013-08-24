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
;; High Level Alg: adapt-1
;; Input: a form F implementing a recursive function, a set S of registered functions names
;; Output: a form Fprime implementing an adapted version of the function implemented by F
;; 1) Parse-markup: perform POSTWALK of F producing Ftemp
;;   For each form in F
;;   1-1) If a non-list and prefixed with a $, replace the form with a (hval...) form
;;   1-2) If a list with a $ prefixing the form in function position, "LF": 
;;      1-2-1) strip away the $ from the form into form LFtemp and 
;;      1-2-2) transform into LFout using (Transform LFtemp, S) 
;;      1-2-3) output the resulting LFprime 
;;  (for all other forms yield the input form unchanged)
;; 
;; 2) Transform:
;;   2-0) Create local CArgs storing mappings of form -> count of args 
;;      2-0-1) Perform PREWALK of LFtemp, and for each form LFtemp-sub
;;          2-0-1-1) if a list, add LFtemp-sub -> list size - 1 
;;          2-0-1-2) output LFtemp-sub unchanged
;;   2-1) 
    ;;   2-1-0) Initialize local atom with mappings: 
    ;;              Started to: FALSE
    ;;              LFout to: `(->) 
    ;;   2-1-1) Perform POSTWALK of LFtemp: for each form LFTsub  in LFtemp
        ;;   2-1-1-1) If it is a list it has one of the following in its function position:
        ;;
        ;;      2-1-1-1-1) the symbol for a function named in S, or (apply <name>...: 
        ;;               in which case a LFsub-prime is ouput:
        ;;                        if NOT Started, append to LFout the equivalent of
        ;;                              (hcall [stack pos] <name> (get CArgs LFTsub) (rest LFTsub))
        ;;                                          OR
        ;;                                  (apply hcall .... (count (rest LFTsub)) (rest LFTsub))) ;; if (apply <name>
        ;;                              and set Started to TRUE
        ;;                        else, append to LFout 
        ;;                              (hcall <name> (get CArgs LFTsub) (rest LFTsub))
        ;;                                          OR (apply hcall .... (count (rest LFTsub))...  
        ;;
        ;;      2-1-1-1-2) the symbol for some other function :
        ;;          2-1-1-1-2-1) if a postwalk search of LFTsub reveals a name in S anywhere within,
        ;;            2-1-1-1-2-1-0) Initialize Fn: 
        ;;                      if (first LFTsub) is a macro symbol, 
        ;;                           wrap it in a function: set Fn to 
        ;;                                    `#((first LFTsub) (for [ x (range (count (rest args)))] (-> 'x (str x) symbol)) 
        ;;                      else set Fn to (first LFTsub).
        ;;            2-1-1-1-2-1-1) a LFsub-prime is output:
        ;;                        if NOT Started, append to LFout
        ;;                              (hfn [stack pos] Fn (get CArgs LFTsub) (rest LFTsub))
        ;;                                          OR (apply hfn ...   ;; if (apply <name> 
        ;;                              and set Started to TRUE
        ;;                        else, append to LFout
        ;;                              (hfn  (get CArgs LFTsub) (rest LFTsub))
        ;;                                          OR (apply hfn ... ;; if (apply <name>
        ;;          2-1-1-1-2-2) else, leave unchanged
        ;;      2-1-1-1-3) a special form or a literal, or some other non-function entity:
        ;;          leave unchanged
    ;;  2-1-2) output LFout

(defn strip
[ form ]
  (let [ ftok (apply str (-> (first form) str rest)) ]
      `( ~(symbol ftok) ~@(rest form))))


(defn strip-literal
[ form ]
  (-> form str (.substring 1) symbol))


(defn hvalize
[ form ]
  (let [ lit (strip-literal form) ]
    `(hval [~'stack ~'pos] ~lit)))


(defn fn-form_OLD
"Yields a form consisting of fsym if not a macro, or a wrapper function otherwise.
"
[ fsym numargs ]
  (if (macro? fsym) 
        (let [ params (-> (for [x (range 1 (inc numargs))] 
                            (-> "arg" (str x) symbol))) ]
             `(fn [~@params] ('~fsym ~@params)))    ;;=> ((eval ff) arg1 arg2....)
        fsym))


(defn fn-form
[ fsym numargs ]
  (if (macro? fsym)
        (let [ params (->> (-> (for [x (range 1 (inc numargs))]
                            (-> "arg" (str x))))
                           (interpose " ") (apply str)) ]
            (str "(fn [" params "] (" (str fsym) " " params "))"))
        (str fsym)))


(defn hoist-fn-form_OLD
[ hsym s&p? fsym numargs args ]
  (let [ func (fn-form fsym numargs) ]
      (if s&p?
        `('~hsym [~'stack ~'pos] ~func  ~numargs ~@args) 
        `('~hsym ~func ~numargs ~@args))))


(defn hoist-fn-form
[ hsym s&p? fsym numargs args ]
  (let [ func (fn-form fsym numargs) 
         s&p-arg (if s&p? "[stack pos]" "") 
         args-str (->> (map #(str %) args) (interpose " ") (apply str))  ]
     (str "(" hsym " " s&p-arg " " func " " numargs " " args-str ")")))


(defn parse
"Strips markup from function positions in forms within body, storing transformed
 forms as keys in a may, and yields a tuple of the new markup in pos. 0, and the 
 map."
[ body ]
  (let [ regs (atom {})
         update (fn [form value] (swap! regs #(merge % {form value}))) ]
           [ (postwalk #(if (and (list? %) 
                       (-> (first %) str (.startsWith "$")))
                                (let [ stripped (strip %) ]
                                            (update stripped {}) stripped) 
                                                %) body)
              ,@regs ]))


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

  
