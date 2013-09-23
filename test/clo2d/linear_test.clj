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
                [3 2 2 13]]
           r (reorder eqs) ]
      (is (= '[[5 1 1 10] ([3 2 2 13] [1 2 3 14])] r))))

  (testing "Reduction"
    (let [[head eqs] [[5 1 1 10] '([3 2 2 13] [1 2 3 14])] 
          rr (row-reduce head eqs) ]
      (is (= '((7/5 7/5 7N) (9/5 14/5 12N)) rr))))

  (testing "Pivot"
    (let [ eqs [[1 2 3 14]
                [5 1 1 10]
                [3 2 2 13]]
           pivot (pivot eqs) ]
      (is (= '((0N)(-7/9 -7/3) (9/5 14/5 12N) [5 1 1 10]) pivot))))

  (testing "Solve"
    (let [ eqs [[1 2 3 14]
                [5 1 1 10]
                [3 2 2 13]]
           roots (solve eqs)]
      (is (= '(1 2 3) roots))))
  
  (testing "Extra consistent eqs"
    (let [ eqs [[1 0 0 1]
                [0 1 0 2]
                [0 0 1 3]
                [1 1 1 6]
                [2 2 2 12]] ]
      (is (= '(1 2 3) (solve eqs)))))
  
  (testing "Inconsistent eqs"
    (let [ eqs [[1 0 1 1]
                [0 1 0 2]
                [1 1 1 9]] ]
      (is (thrown-with-msg? IllegalArgumentException 
                            #"Inconsistent equations" 
                            (solve eqs)))))
  
  (testing "Inconsistent equations"
    (let [ eqs [[1 0 1 2]
                [0 1 0 1]
                [1 0 2 4]
                [1 1 1 3]
                [2 0 2 3]
                [4 0 4 8]] ]
      (is (thrown-with-msg? IllegalArgumentException 
                            #"Inconsistent equations" 
                            (solve eqs)))))
  
  (testing "Unsolvable"
    (let [ eqs [[1 0 1 1]
                [0 1 1 2]
                [1 1 2 3]] ]
      (is (thrown-with-msg? IllegalArgumentException 
                            #"Unsolvable" 
                            (solve eqs)))))
  
  (testing "Unsolvable"
    (let [ eqs [[1 0 1 2]
                [0 1 0 1]
                [1 1 1 3]
                [1 2 1 4]
                [2 1 2 5]] ]
      (is (thrown-with-msg? IllegalArgumentException 
                            #"Unsolvable" 
                            (solve eqs)))))

)

(deftest map-eq-test
  (testing "indep"
    (let [ eqs [ {:x 1 := 1 } 
                 {:y 1 := 2 }
                 {:x 1 :y 2 :z 3 := 4 } ]
           res (indeps eqs) ]
      (is (= res #{ :x :y :z }))))

  (testing "eq parsing"
    (let [ eqs [ {:x 1 := 1 } 
                 {:y 1 := 2 }
                 {:x 1 :y 2 :z 3 := 4 } ]
           [keys res] (parse-eq eqs) ]
      (is (= keys #{:x :y :z}))
      (is (= res [[3 2 1 4]
                  [0 1 0 2]
                  [0 0 1 1]]))))

  (testing "eq parsing with 0 default"
    (let [ eqs [ {:x 1 := 0 } 
                 {:y 1 }
                 {:x 1 :y 2 :z 3 } ]
           [keys res] (parse-eq eqs) ]
      (is (= keys #{:x :y :z}))
      (is (= res [[3 2 1 0]
                  [0 1 0 0]
                  [0 0 1 0]]))))

  (testing "eq solving"
    (let [ eqs [ {:x 1 := 1 } 
                 {:y 1 := 2 }
                 {:x 1 :y 2 :z 3 := 4 } ]
           res (solve-eq eqs) ]
      (is (= res {:x 1 :y 2 :z -1/3}))))
                  
)

;;
;; New map-based linear solver
;;
(deftest map-based-solver-test
  (testing "Utility mp-product"
    (is (= (mp-product #(+ %1 %2) {:a 1 :b 2 } { :a 1 :c 1})
           {:a 2 :b 2 :c 1})))

  (testing "Equation reordering"
    (let [ eqs [{:x 1 :y 2 :z 3 := 14} ;; roots 1 2 3
                {:x 5 :y 1 :z 1 := 10}
                {:x 3 :y 2 :z 2 := 13}]
           r (mp-reorder :x eqs) ]
      (is (= '[{:x 5 :y 1 :z 1 := 10} 
               ({:x 3 :y 2 :z 2 := 13} {:x 1 :y 2 :z 3 := 14})] r))))

  (testing "Reduction"
    (let [[head eqs] 
          [{:x 5 :y 1 :z 1 := 10}
                         '({:x 3 :y 2 :z 2 := 13} {:x 1 :y 2 :z 3 := 14})]
          rr (mp-row-reduce :x head eqs) ]
      (is (= '({:y 7/5 :z 7/5 := 7N} {:y 9/5 :z 14/5 := 12N}) rr))))

  (testing "Pivot"
    (let [ eqs [{:x 1 :y 2 :z 3 := 14} ;; roots 1 2 3
                {:x 5 :y 1 :z 1 := 10}
                {:x 3 :y 2 :z 2 := 13}]
           pivot (mp-pivot eqs) ]
      (is (= '({:= 0N} 
               {:x 7/2, := 7/2} 
               {:y 2/3, :x 7/3, := 11/3} 
               {:z 3, :y 2, :x 1, := 14}) pivot)))

    (let [ eqs [{:a 1 := 1} {:b 1 := 2}]
           pivot (mp-pivot eqs) ]
      (is (= '({:= 0N} 
               {:b 1, := 2} 
               {:a 1, := 1}) pivot)))
  )

  (testing "Solve"
    (let [ eqs [{:x 1 :y 2 :z 3 := 14} ;; roots 1 2 3
                {:x 5 :y 1 :z 1 := 10}
                {:x 3 :y 2 :z 2 := 13}]
           [roots unsolved] (mp-solve eqs)]
      (is (= {:x 1 :y 2 :z 3} roots))
      (is (empty? unsolved))))

  (testing "With unsolvable var"
    (let [ eqs [{:x 1 :y 2 :z 3 := 14} ;; roots 1 2 3
                {:x 5 :y 1 :z 1 := 10}
                {:x 3 :y 2 :z 2 := 13}
                {:u 1 :v 1 := 0}]
           [roots unsolved] (mp-solve eqs)]
      (is (= {:x 1 :y 2 :z 3 :u nil :v nil} roots))
      (is (= '({:u 1 :v 1 := 0}) unsolved))))

  (testing "With unsolvable var"
    (let [ eqs [{:x 1 :y 2 :z 3 := 14} ;; roots 1 2 3
                {:x 5 :y 1 :z 1 := 10}
                {:x 3 :y 2 :z 2 := 13}
                {:x 9 :u 1 :v 1 := 0}]
           [roots unsolved] (mp-solve eqs)]
      (is (= {:x 1 :y 2 :z 3 :u nil :v nil} roots))
      (is (= '({:u 1 :v 1 := 0}) unsolved))))
)
