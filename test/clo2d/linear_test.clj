(ns clo2d.linear-test
  (:require [clojure.test :refer :all]
            [clo2d.linear :refer :all]
            [clojure.java.io :refer :all])
  (:import (java.io File)))

(deftest product-test
  (testing "Product"
    (let [a [1 2 3]
          b [4 5 6]
          c (product * a b)]
      (is (= [4 10 18] c)))))

(deftest accumulate-test
  (testing "Accumulate"
    (let [ vec   '(1 2 3 4)
           roots '(5 6 7)
           acc   (accumulate vec roots) ]
      (is (= '(38 4) acc))))) 

(deftest zero?-test
  (testing "fzero?"
    (is (fzero? (/ +epsilon 2)))
    (is (fzero? (/ +epsilon -2)))
    (is (fzero? 0.5 1))
    (is (fzero? -0.5 -1 0.1)) ))


(deftest gauss-solver-test
  (testing "Equation reordering"
    (let [ eqs [[1 2 3 14] ;; roots 1 2 3
                [5 1 1 10]
                [3 2 2 13]
                [1 6 6 31]]
           r (reorder eqs) ]
      (is (= '[[5 1 1 10] ([1 6 6 31] [3 2 2 13] [1 2 3 14])] r))))

  (testing "Reduction"
    (let [[head eqs] [[5 1 1 10] '([1 6 6 31] [3 2 2 13] [1 2 3 14])] 
          rr (row-reduce head eqs) ]
      (is (= '((29/5 29/5 29N) (7/5 7/5 7N) (9/5 14/5 12N)) rr))))

  (testing "Pivot"
    (let [ eqs [[1 2 3 14]
                [5 1 1 10]
                [3 2 2 13]
                [1 6 6 31]] 
           pivot (pivot eqs) ]
      (is (= '((0N) (1N 3N) (29/5 29/5 29N) [5 1 1 10]) pivot))))

  (testing "Solve"
    (let [ eqs [[1 2 3 14]
                [5 1 1 10]
                [3 2 2 13]
                [1 6 6 31]]
           roots (solve eqs)]
      (is (= '(1 2 3) roots))))
)

