(ns recursely.core
  (:use recursely.ccore 
        [clojure [walk]]
        [clj-utils [core :only [thread-it is-case]]
                   [coll :only [in? split-find]]]))

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
;; ADAPT-1
;; Input: a form F implementing a recursive function, a set S of registered functions names
;; Output: a form Fprime implementing an adapted version of the function implemented by F
;;
;; - Parse-markup: perform POSTWALK of F producing Ftemp
;;   For each form FIN in F
;;   -- If FIN is a list and has a direct child form '$, the next form FIN-L is a literal collection:
;;      --- replace FIN-L with a (hval...) wrapper form around FIN-L, remove '$ and output the result
;;   -- If FIN is a list with a $ prefixing the form in function position: 
;;      --- strip away the $ from FIN to obtain form FTMP
;;      --- transform into FOUT using (Transform FTMP, S) 
;;      --- output FOUT instead of F
;;   -- if FIN is a form starting with $, strip $ and wrap the remaining value with
;;      a (hval...) wrapper form FOUT and output FOUT 
;;   -- else output FIN unchanged


;; TRANSFORM 
;; Input: form F, a list; set S; atom STARTED; string BUFFER 
;; Output: form F' an adapted version of F.
;;      
;; - if a postwalk on F does not reveal any symbol in S:
;;      -- return (hparam F) * OR, if this is the first, do instead (-> (hparam [stack pos] F-SUB)
;;                                      and set STARTED to true
;; - else, its head is a fn or macro call: 
;;      -- perform Transform-rest on (rest F): 
;;      -- emit (hfn <head fn/macro> (count F)) * see above 
;;                                              ** if Fin S, use (hcall ...) instead


;; TRANSFORM-REST*
;; Input: form F; set S; atom STARTED; string BUFFER
;; - For each form f' of F:
;;   -- if a list, invoke (Transform f' S STARTED)
;;   -- else, emit (hparam ....) *  see above
;;                               ** ditto
;;              

;; OPTIMIZE (not implemented)
;; Input: form F, a list output by TRANSFORM
;; Output: an optimized version of F, i.e. with possibly smaller number of (hparam invocations)
;; - PostWalk through F, and for each subform FSUB:
;;   -- if it nests one or more (hcall..) forms, create an FSUB'
;;      --- walk through its subforms FSUBSUBs in reversed order
;;          ---- if an hcall or an hfn, retrieve all following (hparams..) 
;;               ----- append the params in reversed order to the end of the (hcall...) FSUB' form
;;          ---- otherwise append FSUBSUB to FSUB' as is
;;          ---- output  FSUB' instead of FSUB

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
     (-> (str "(" NSPFX "hval [stack pos] " lit ")") read-string)))


(defn str-fn-form
[ fsym numargs ]
(let [ params (->> (-> (for [x (range 1 (inc numargs))]
                    (-> "arg" (str x))))
                   (interpose " ") (apply str)) ]
    (str "(fn [" params "] (" (str fsym) " " params "))")))


(defn str-fn-form-apply
[ fsym ]
  (str "(fn [& args] (apply " (str fsym) " args))"))


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


(defn apply-numargs
"Yields a renders a form computing a runtime number of stacked arguments
 for an (apply...) form, based on in-form's structure.
 in-form is assumed to have 'apply in function position and a seq as its
 last element.
"
[ inf ]
  (let [ pfx-args (-> (count inf) (- 3))           ;;subtract 3 for 'apply myfunc', and last elem (a seq) 
         variadic-form (->  (str " (count ") 
                            (str (last inf))
                            (str ")")) ]
    (if (= 0 pfx-args) variadic-form
                       (-> "(+ " 
                            (str pfx-args " ")
                            (str variadic-form)
                            (str ")")))))


(defn emit-invoke
"Yields a hcall or hfn call. If the target fn symbol is in regs, the call is an hcall, else it is an hfn.
"
[ form regs started?-atom ]
(let [ emit-state? (not (started?-and-set started?-atom)) 
       apply? (is-apply? form)
       fn-symbol (if apply? (second form) (first form)) 
       hcall? (in? regs fn-symbol)
       siz (count form)
       numargs (if apply? (apply-numargs form)
                          (-> (dec siz)
                              #_(+ (if hcall? 2 0))))

       numparams (if hcall? (if apply? nil (+ numargs 2)) numargs)      ;; this should be used only for non-apply 
    ]
(-> (str "(")
    (str NSPFX)
    (str (if (in? regs fn-symbol) "hcall" "hfn"))
    (str (if emit-state? " [stack pos] " " "))              ;;space
    (str (if apply? (str-fn-form-apply fn-symbol)
                        (str-fn-form fn-symbol numparams)))
    (str " ")                                               ;;space
    (str numargs)
    (str ")"))))


(defn emit-param*
[ type sym started?-atom ]
(let [ emit-state? (not (started?-and-set started?-atom)) ]
   (-> (str "(" NSPFX type " ") 
       (str (if emit-state? "[stack pos] " ""))
       (str sym ")"))))
       
   
(defn emit-param
[ sym started?-atom ]
(emit-param* "hparam" sym started?-atom))


(defn emit-paramlist
[ sym started?-atom ]
(emit-param* "hparam-list" sym started?-atom))


(defn emit-rewind
[]
(-> (str "(")
    (str NSPFX)
    (str "rewind pos)")))

(defn emit-close
[]
(str ")"))

(defn emit-open
[]
(str "(->"))

(declare transform transform-str transform-rest-str transform-rest-apply-str is-apply? starts-with?)

(defn adapt-1
"Transforms form into a form adapted for recursely usage. Yields the adapted form.
"
[ form regs ]
   ;;(postwalk #(
)


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
(do #_(println "Transform-str: in-form=>'" in-form "<, REGS=" regs) 
  (-> 
      (str (if (not (of-interest? in-form regs))

                        ;; no nested forms requiring special treatment:
                        ;; emit param with form as is
                        (do #_(println "transform-str: found nothing")

                        (emit-param in-form started?))

                        ;; nested forms having registered functions:
                        ;; handle them first as params to this function symbol,
                        ;; then hoist fn call itself.

                        (let [ apply? (is-apply? in-form)
                               rest-fn (if apply? transform-rest-apply-str transform-rest-str) 
                               rest-tgt (if apply? (rest (rest in-form)) (rest in-form)) ]
                            (-> (str (rest-fn rest-tgt regs started?))
                                (str (emit-invoke in-form regs started?)))))))))
([ in-form regs ]
  (str (emit-open) 
       (transform-str in-form regs (atom false))
       (emit-rewind)
       (emit-close))))


(defn transform-rest-str
"Maps each of its non-list element (including those in function position) to an (hparam..) invocation.
 Lists elements are processed by transform-str.
"
[ in-form regs started? ]
   (->> (map #(if (list? %) (transform-str % regs started?)
                            (emit-param % started?)) in-form)
        (reduce #(str %1 %2))))


(defn transform-rest-apply-str
"Same as transform-rest-str, except that the last element is mapped to an hparam-list invocation.
 Others are processed as they would be transform-rest-str.
"
[ form regs started? ]
(let [ last (-> form reverse first) 
       but-last (butlast form)
       last-out (if (of-interest? last regs) (transform-str last regs started?) 
                                        (emit-paramlist last started?)) ]
    (-> (if (empty? but-last) "" (transform-rest-str (butlast form) regs started?)) 
        (str last-out))))


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


(defn has-marked-coll?
"Yields marker if found as a direct sub-form of form, or nil.
"
[ form marker] 
  (and (list? form) (some #{marker} form)))


(defn notcoll-starts-with-marker?
"Yields true if a marker prefixes a literal that is not a collection.
"
[ form marker ]
  (let [ fs (str form) ]
    (and (-> 1 (< (.length fs))) 
         (-> (str form) (.startsWith marker)))))


(defn funcpos-starts-with-marker?
"Yields true if a form has marker as the prefix to its first symbol if list,
 or prefixes form otherwise.
"
[ form marker]
   (and (list? form)
        (-> (first form) str (.startsWith marker))))


(defn replace-endmarks
"Parses marked forms for end literals to be wrapped in (hval..) forms.
"
[ form marker ]
  (let [ tmp (postwalk 
                    #(cond (has-marked-coll? % marker) 
                        (let [ parts (split-find % marker)
                               parts-1 (first parts)
                               parts-2 (second parts)
                               end (rest (rest parts-2)) 
                               val (second (second parts)) ]
                            (concat parts-1 [hval-form val] end))
                :else %) form) ]
         (postwalk 
             #(if (notcoll-starts-with-marker? form marker)
                   (-> (strip-literal %) hval-form) %) 
              tmp)))
                            

(defn parse
"Parses marked up forms in body and replaces them with framework function calls,
 using symbols in regs to recognize recurrent invocations in form.

 Uses marker to indentify marked up literals and forms.
"
[ regs body marker ]
    (postwalk #(cond (funcpos-starts-with-marker? % marker) 
                         (let [ stripped (strip % marker) ]
                                        (transform stripped))
                      (notcoll-starts-with-marker? %) 
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

  
