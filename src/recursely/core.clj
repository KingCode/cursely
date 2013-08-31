(ns recursely.core
  (:use recursely.ccore 
        [clojure [walk]]
        [clj-utils [core :only [thread-it macro-call funcvar macro? is-case]]
                   [coll :only [in?]]]))

(def NSPFX "recursely.ccore/")

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
;;      Transform 
;;      Input: form F, a list, requiring conversion; set S; atom STARTED; string BUFFER 
;;      
;;          - if a postwalk on F does not reveal any symbol in S:
;;                    (hparam F) * OR, if this is the first, do instead (-> (hparam [stack pos] F-SUB)
;;                                      and set STARTED to true
;;
;;          - else, its head is a fn or macro call: 
;;                    -- perform Transform-rest on (rest F): 
;;                    -- emit (hfn <head fn/macro> (count F)) * see above 
;;                                                            ** if Fin S, use (hcall ...) instead
;;                             
;;       Transform-rest
;;       Input: form F; set S; atom STARTED; string BUFFER
;;       For each form f' of F:
;;              - if a list, invoke (Transform f' S STARTED)
;;              - else, emit (hparam ....)
;;              

(defn strip
[ form ]
  (let [ ftok (apply str (-> (first form) str rest)) ]
      `( ~(symbol ftok) ~@(rest form))))


(defn strip-literal
[ form ]
  (-> form str (.substring 1) symbol))


(defn hval-form
[ form ]
  (let [ lit (strip-literal form) ]
     (-> (str "(recursely.ccore/hval [stack pos] " lit ")") read-string)))
 ;;   `(hval [~'stack ~'pos] ~lit)))


(defn str-fn-form
[ fsym numargs ]
  (if (macro? fsym)
        (let [ params (->> (-> (for [x (range 1 (inc numargs))]
                            (-> "arg" (str x))))
                           (interpose " ") (apply str)) ]
            (str "(fn [" params "] (" (str fsym) " " params "))"))
        (str fsym)))


(defn of-interest?
"Yields true if a symbol in regs is found as anywhere within form."
[ form regs ]
  (let [ found? (atom false) ]
        (postwalk #(if (in? regs %) (do (reset! found? true) %) %) form)
        @found?))

(defn started?-and-set
"Yields the value of started? atom. If false, started?'s value is set to true.
"
[ started?-atom ]
(let [ started?  @started?-atom ]
  (if (not started?)
    (reset! started?-atom true))
  started?))

(declare is-apply?)

(defn emit-invoke
"Yields a hcall or hfn call. If the target fn symbol is in regs, the call is an hcall, else it is an hfn.
"
[ form regs started?-atom ]
(let [ emit-state? (not (started?-and-set started?-atom)) 
       apply? (is-apply? form)
       fn-symbol (if apply? (second form) (first form)) 
       siz (count form)
       numargs (if apply? (- siz 2) (dec siz)) ] 
(-> (str "(")
    (str (if apply? "apply " ""))                ;;space
    (str NSPFX)
    (str (if (in? regs fn-symbol) "hcall" "hfn"))
    (str (if emit-state? " [stack pos] " " "))  ;;space
    (str (str-fn-form fn-symbol numargs))
    (str " ")                                   ;;space
    (str numargs)
    (str ")"))))


(defn emit-param
[ sym started?-atom ]
(let [ emit-state? (not (started?-and-set started?-atom)) ]
   (-> (str "(" NSPFX "hparam ")
       (str (if emit-state? "[stack pos] " ""))
       (str sym ")"))))
   

(defn emit-rewind
[]
(str "(rewind pos)"))

(defn emit-close
[]
(str ")"))

(defn emit-open
[]
(str "(->"))

(declare transform-str transform-rest-str is-apply? starts-with?)

(defn transform
"Yields a form which invokes a sequence of hparam/hfn/hcall invocations according 
 to in-form's structure and regs contents.
 "
 [ in-form regs ]
   (read-string (transform-str in-form regs)))


(defn transform-str
"in-form is a deserialized form, regs a seq of symbols, and started? is an atom referencing a boolean.
 Yields a serialized form which invokes a sequence of hparam, hfn/call invocations 
 for each element from in-form, based on the element's structure and position, symbols in regs, 
 and whether @started? is true; the contents of buffer is prefixed to the output.
"
([ in-form regs started?]
(do (println "Transform-str: in-form=>'" in-form "<, REGS=" regs) 
  (-> 
      (str (if (not (of-interest? in-form regs))
                        ;; no nested forms requiring special treatment:
                        ;; emit param with form as is
                        (do (println "transform-str: found nothing")
                        (emit-param in-form started?))

                        ;; nested forms having registered functions:
                        ;; handle them first as params to this function symbol,
                        ;; then hoist fn call itself.
                        (-> (str (transform-rest-str (rest in-form) regs started?))
                            (str (emit-invoke in-form regs started?))))))))
([ in-form regs ]
  (str (emit-open) 
       (transform-str in-form regs (atom false))
       (emit-rewind)
       (emit-close))))


(defn transform-rest-str
"Same as transform, except that elements in function position are treated like any other.
"
[ in-form regs started? ]
(do (println "Transform-REST-str: in-form=>'" in-form "<") 
   (->> (map #(do (println "MAP LOOP: " %) (if (list? %) (transform-str % regs started?)
                            (emit-param % started?))) in-form)
        (reduce #(str %1 %2)))))


(defn is-apply?
[ form ]
(starts-with? form 'apply))


(defn starts-with?
"Yields true if a form starts with symbol if a list, or is the symbol
 otherwise.
"
[ form symbol ]
   (if (list? form)
        (-> (first form) (= symbol))
        (= form symbol)))


(defn starts-with-marker?
"Yields true if a form has marker as the prefix to its first symbol if list,
 or prefixes form otherwise.
"
[ form marker]
   (if (list? form)
        (-> (first form) str (.startsWith marker))
        (-> (str form) (.startsWith marker))))


(defn parse
"Parses marked up forms in body and replaces them with framework function calls,
 using symbols in regs to recognize recurrent invocations in form.

 Uses marker to indentify marked up literals and forms.
"
[ regs body marker ]
    (postwalk #(cond (and (list? %) (starts-with-marker? % marker)) 
                                    (let [ stripped (strip % marker) ]
                                        (transform stripped))
                      (starts-with-marker? %) 
                                    (hval-form %) 
                      :else %) body)) 
                              

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


 (defn register-funcs
 "Yields a set of function symbols from func-specs with each func spec is a (funcname, ([& args] body)*)
 "
 [ func-spec & func-specs ]
 )

(defmacro defrecursely
"Creates a recurseldeclared functiony adapted function for each spec, where spec is a (name ([& args] body)*)."
[ spec & specs ] )

  
