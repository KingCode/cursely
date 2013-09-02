(ns recursely.core-test
  (:use recursely.core)
  (:use  
        [clj-utils [coll :only [subsq foreach]]
                   [stackp] 
                   [debug :only [debug-info-1 debug-info]]]
        clojure.test
        [robert.hooke :as h]))
                

(deftest transform-test-counter
  (testing "Simplest, recursive counter form should yield correct adapted form"
     (let [ src '(+ 1 (counter (rest coll)))
            exp '(-> (recursely.ccore/hparam [stack pos] 1)
                     (recursely.ccore/hparam (rest coll))
                     (recursely.ccore/hcall (fn [arg1 arg2 arg3] (counter arg1 arg2 arg3)) 1)
                     (recursely.ccore/hfn (fn [arg1 arg2] (+ arg1 arg2)) 2)
                     (recursely.ccore/rewind pos)) 
            act (transform src #{'counter}) ]
        (is (= exp act)))))


(deftest transform-test-odd?
  (testing "Simplest recursive odd? form should yield correct adapted form"
    (let [ src '(recursive-odd? (- x 2))
           exp '(-> (recursely.ccore/hparam [stack pos] (- x 2))
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3] (recursive-odd? arg1 arg2 arg3)) 1)
                    (recursely.ccore/rewind pos))
           act (transform src #{'recursive-odd?}) ]
        (is (= exp act)))))


(deftest transform-test-cat
  (testing "Simple fn cat form should yield correct adapted form"
    (let [ src '(hat (dec n) (+ term acc))
           exp '(-> (recursely.ccore/hparam [stack pos] (dec n))
                    (recursely.ccore/hparam (+ term acc))
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3 arg4] (hat arg1 arg2 arg3 arg4)) 2)
                    (recursely.ccore/rewind pos))
           act (transform src #{'hat 'cat}) ]
        (is (= exp act)))))


(deftest transform-test-hoffstadter
  (testing "Male/female sub-form should yield correct adapted form"
    (let [ src '(- n (female (male (dec n))))
           exp '(-> (recursely.ccore/hparam [stack pos] n)
                    (recursely.ccore/hparam (dec n))
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3] (male arg1 arg2 arg3)) 1)
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3] (female arg1 arg2 arg3)) 1)
                    (recursely.ccore/hfn (fn [arg1 arg2] (- arg1 arg2)) 2)
                    (recursely.ccore/rewind pos))
           act (transform src #{'male 'female}) ]
        (is (= exp act)))))


(deftest transform-test-KS-1
  (testing "Simple form from KS logic should yield correct framework adapted form"
    (let [ src '(KS tail capacity) 
           exp '(-> (recursely.ccore/hparam [stack pos] tail)
                    (recursely.ccore/hparam capacity)
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3 arg4] (KS arg1 arg2 arg3 arg4)) 2)
                    (recursely.ccore/rewind pos))
           act (transform src #{'KS}) ]
        (is (= exp act))))) 

(deftest transform-test-KS-2
  (testing "A form nesting calls to KS should yield the correct framework adapted form"
        (let [ 
                src '(max (KS tail capacity)  (+ v (KS tail (- capacity c))))

                exp '(-> (recursely.ccore/hparam [stack pos] tail)
                    (recursely.ccore/hparam capacity)
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3 arg4] (KS arg1 arg2 arg3 arg4))  2)
                    (recursely.ccore/hparam v)
                    (recursely.ccore/hparam tail)
                    (recursely.ccore/hparam (- capacity c)) 
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3 arg4] (KS arg1 arg2 arg3 arg4))  2)
                    (recursely.ccore/hfn (fn [arg1 arg2] (+ arg1 arg2)) 2)
                    (recursely.ccore/hfn (fn [arg1 arg2] (max arg1 arg2)) 2)
                    (recursely.ccore/rewind pos)) 

                act (transform src #{'KS})
             ]
    #_(do
    (println "EXPECTED: ") 
        (foreach [ f exp] (println f))
    (println "ACTUAL: " ) 
        (foreach [ f act] (println f)))

          (is (= exp act))))) 


(deftest transform-test-powercounter
  (testing "A complex form nesting multiple and mutually recursive  calls and fns mixed with multiple 
            positional parameters, should yield the correct adapted form"
    (let [ src '(power-counter (rest power-in-numbers1) 
                      (* factor (+ (counter power-in-numbers1)
                                   (counter power-in-numbers2))) 
                      (rest power-in-numbers2)  
                      (rest coll))

           exp '(-> (recursely.ccore/hparam [stack pos] (rest power-in-numbers1))
                    (recursely.ccore/hparam factor)
                    (recursely.ccore/hparam power-in-numbers1)
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3] (counter arg1 arg2 arg3)) 1)
                    (recursely.ccore/hparam power-in-numbers2)
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3] (counter arg1 arg2 arg3)) 1)
                    (recursely.ccore/hfn (fn [arg1 arg2] (+ arg1 arg2)) 2) 
                    (recursely.ccore/hfn (fn [arg1 arg2] (* arg1 arg2)) 2)
                    (recursely.ccore/hparam (rest power-in-numbers2))
                    (recursely.ccore/hparam (rest coll))
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3 arg4 arg5 arg6]
                                                (power-counter arg1 arg2 arg3 arg4 arg5 arg6)) 4)
                    (recursely.ccore/rewind pos))

            act (transform src #{'power-counter 'counter}) ]

        (is (= exp act)))))


(deftest transform-test-variadic
  (testing "A form using apply should yieldthe correct adapted form"
    (let [ src '(and (first args) (apply recursive-and (rest args)))

           exp '(-> (recursely.ccore/hparam [stack pos] (first args))
                (recursely.ccore/hparam-list (rest args))
                (recursely.ccore/hcall (fn [& args] (apply recursive-and args)) (count (rest args)))
                (recursely.ccore/hfn (fn [arg1 arg2] (and arg1 arg2)) 2)
                (recursely.ccore/rewind pos))

           act (transform src #{'recursive-and}) ]
         (do
    (println "EXPECTED: ")  
        (foreach [ f exp] (println f)) 
    (println "ACTUAL: " ) 
        (foreach [ f act] (println f)))
 
        (is (= exp act)))))            
