(ns clo2d.shape-test
  (:require [clojure.test :refer :all]
            [clo2d.shape :refer :all])
  (:import (java.io File)))

(deftest utils-test
  (testing "Folding"
    (is (= (fold* "x" :y :z) { :x:y :y :x:z :z }))
  )

  (testing "Unfolding"
    (doseq [[in out] [
              [ { :x:top 1 :x:bottom 2 :y:top 3 :y:bottom 4 }
                { :x { :top 1 :bottom 2 } :y { :top 3 :bottom 4}}
              ]
              ]]
      (is (= (unfold* in) out)))
  )
)

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

(deftest group-test
  (testing "Group creation and manipulation"
    (let [ g (group (shape "x" square) (shape "p1" point)
                    (having (:p1:y = :x:top) 
                            (:p1:x = :x:right + 50 )
                            (:x:top = 50)
                            (:x:left = 50)
                            (:x:height = 20))) ]
      (is (= (second g) '()))
      (is (= (-- (first g) (nw :x) :p1) [ 50 50 120 50])))))
