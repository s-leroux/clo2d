(ns clo2d.linear-test
  (:require [clojure.test :refer :all]
            [clo2d.linear :refer :all]
            [clojure.java.io :refer :all])
  (:import (java.io File)))

(deftest utilities-test
  (testing "Product"
    (let [a [1 2 3]
          b [4 5 6]
          c (product * a b)]
      (is (= [4 10 18] c)))))

(deftest gauss-solver-test
  (testing "Equation reordering"
    (let [ eqs [[1 2 3 4]
                [5 1 1 1]
                [3 2 2 2]
                [1 6 6 6]]]
      (println (reorder eqs))))

  (testing "Reduction"
    (let [[head eqs] [[5 1 1 1] '([1 6 6 6] [3 2 2 2] [1 2 3 4])]]
      (println (row-reduce head eqs))))

      
)
