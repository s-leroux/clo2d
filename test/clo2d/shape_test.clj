(ns clo2d.shape-test
  (:require [clojure.test :refer :all]
            [clo2d.shape :refer :all])
  (:import (java.io File)))

(deftest utils-test
  (testing "Unfolding"
    (doseq [[in out] [
              [ :x:y :y ]
              [ :x   :x ]
            ]]
      (is (= (unfold in) out)))
  ))

(deftest updt-keywords-test
  (testing "Updating keywords"
    (doseq [[in out] [
                 '( ( :a )  ( :x:a ) )
                 '( ( :a :b )  ( :x:a :x:b ) )
                 '( ( 1 :b )  ( 1 :x:b ) )
                 '( ( :a ( :b :c) )  ( :x:a ( :x:b :x:c) ) )
                 '( ( :a ( :b :+ :c) )  ( :x:a ( :x:b :+ :x:c) ) )
                 '( ( :a ( :b :- :c) )  ( :x:a ( :x:b :- :x:c) ) )
                 '( ( :a ( :b := :c) )  ( :x:a ( :x:b := :x:c) ) )
                 '( ( :a ( :b + :c) )  ( :x:a ( :x:b + :x:c) ) )
                     ]]
      (is (= (updt-keywords "x" in) out)))))