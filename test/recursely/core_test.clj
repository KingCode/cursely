(ns recursely.core-test
  (:use recursely.core)
  (:use  
        [clj-utils [coll :only [subsq]]
                   [stackp] 
                   [debug :only [debug-info-1 debug-info]]]
        clojure.test
        [robert.hooke :as h]))
                
(def ^:dynamic  KS) 

#_(deftest transform-test
    (testing "A form nesting calls to KS should yield the correct framework adapted form"
    (binding [KS (fn []) ] ;;dummy function for keep compiler quiet for now
        (let [ 

                src '(max (KS tail capacity)  (+ v (KS tail (- capacity c))))

                exp '(-> (recursely.ccore/hparam [stack pos] tail)
                    (recursely.ccore/hparam capacity)
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3 arg4] (adapted-KS arg1 arg2 arg3 arg4))  2)
                    (recursely.ccore/hparam v)
                    (recursely.ccore/hparam tail)
                    (recursely.ccore/hparam (- capacity c)) 
                    (recursely.ccore/hcall (fn [arg1 arg2 arg3 arg4] (adapted-KS arg1 arg2 arg3 arg4))  2)
                    (recursely.ccore/hfn (fn [arg1 arg2] (+ arg1 arg2)) 2)
                    (recursely.ccore/hfn (fn [arg1 arg2] (max arg1 arg2)) 2)
                    (recursely.ccore/rewind pos)) 

                act (transform src #{'KS})
             ]
          (is (= exp act)))))) 

