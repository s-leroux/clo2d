(ns clo2d.linear-test
  (:require [clojure.test :refer :all]
            [clo2d.linear :refer :all]
            [clojure.java.io :refer :all])
  (:import (java.io File)))

(deftest zero?-test
  (testing "fzero?"
    (is (fzero? (/ +epsilon 2)))
    (is (fzero? (/ +epsilon -2)))
    (is (fzero? 0.5 1))
    (is (fzero? -0.5 -1 0.1)) ))

;;
;; Testing equation algebra
;;
(deftest equation-algebra-test
  (testing "Adding equations"
    (let [eq1      {:x  1 :y  2       :=  3 }
          eq2      {:x  4       :z  5 }
          expected {:x  5 :y  2 :z  5 :=  3 }]
      (is (= (eq-add eq1 eq2) expected))))

  (testing "Substracting equations"
    (let [eq1      {:x  1 :y  2       :=  3 }
          eq2      {:x  4       :z  5 }
          expected {:x -3 :y  2 :z -5 :=  3 }]
      (is (= (eq-sub eq1 eq2) expected))))

  (testing "Multiply equation (by constant)"
    (let [eq1      {:x  1 :y  2       :=  3 }
          x        2
          expected {:x  2 :y  4       :=  6 }]
      (is (= (eq-mul eq1 x) expected))))

  (testing "Fused multiply-add equations"
    (let [eq1      {:x  1 :y  2       :=  3 }
          x        2
          eq2      {:x  4       :z  5 }
          expected {:x  9 :y  2 :z 10 :=  3 }]
      (is (= (eq-madd eq1 x eq2) expected))))

  (testing "Fused multiply-add returning *normalized* eq"
    (let [eq1      {:x -2 :y -2       := -6 }
          x        2
          eq2      {:x  1       :z  4 :=  3 }
          expected {      :y -2 :z  8       }]
      (is (= (eq-madd eq1 x eq2) expected))))

  (testing "Fused multiply-sub equations"
    (let [eq1      {:x  1 :y  2       :=  3 }
          x        2
          eq2      {:x  4       :z  5 }
          expected {:x -7 :y  2 :z -10 :=  3 }]
      (is (= (eq-msub eq1 x eq2) expected))))

  (testing "Fused multiply-sub returning *normalized* eq"
    (let [eq1      {:x  2 :y  2       :=  6 }
          x        2
          eq2      {:x  1       :z  4 :=  3 }
          expected {      :y  2 :z -8       }]
      (is (= (eq-msub eq1 x eq2) expected))))

  (testing "Proportional?"
    (doseq [[eq1 eq2 result] [[{} {} nil]
                              [{:= 6} {:= 2} 3]
                              [{:a 3 :b 6 := 9} {:a 1 :b 2 := 3} 3]
                              [{:a 3 :b 6 :e 0 := 9} {:d 0 :a 1 :b 2 := 3} 3]
                              [{:a 3 :b 6 := 9} {:b 2 := 3} nil]] ]
      (is (= (eq-proportional? eq1 eq2) result))))

  (testing "Constant?"
    (doseq [[eq result] [[{} true]
                         [{:= 6} true]
                         [{:x 0 := 6} true]
                         [{:x 1 := 6} false]] ]
      (is (= (eq-constant? eq) result))))
)

(deftest eq-eval-test
  (testing "Evaluation with normalization"
    (doseq[[in out] [[{}                          {}]
                     [{:a 1 := 1}                 {}]
                     [{:a 1 :u 1 := 1}            {:u 1}]
                     [{:a 1}                      {:= -1}]
                     [{:a 0 :u 1 := 0}            {:u 1}]] ]
      (let [ctx { :a 1 :b 2 :c 3 }]
        (is (= (eq-eval in ctx) out)))
    )
  )
)

(deftest eq-norm-test
  "Normalized equations have all non-zero terms removed (incl. the
  constant term)"
  (testing "Equation normalization"
    (doseq[[in out] [[{}                          {}]
                     [{:= 0}                      {}]
                     [{:= 1}                      {:= 1}]
                     [{:x 0}                      {}]
                     [{:x 1}                      {:x 1}]
                     [{:x 1 :y 2 := 3}            {:x 1 :y 2 := 3}]
                     [{:x 0 :y 1 := 2}            {:y 1 := 2}]]]
      (is (= (eq-norm in) out))))
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
      (is (= '({:x 7/2, := 7/2} 
               {:y 2/3, := 4/3} 
               {:z 3,   := 9}) pivot)))

    (let [ eqs [{:a 1 := 1} {:b 1 := 2}]
           pivot (mp-pivot eqs) ]
      (is (= '({:b 1, := 2} 
               {:a 1, := 1}) pivot)))
  )

  (testing "Solve"
    (let [ eqs [{:x 1 :y 2 :z 3 := 14} ;; roots 1 2 3
                {:x 5 :y 1 :z 1 := 10}
                {:x 3 :y 2 :z 2 := 13}]
           [roots unsolved] (mp-solve eqs)]
      (is (= {:x 1 :y 2 :z 3} roots))
      (is (empty? unsolved))))

  (testing "With zero-only terms"
    (let [ eqs [{:a 1 :b 1 :c 0 := 2}{:a 1 :b 0 := 1}]
           [roots unsolved] (mp-solve eqs)]
      (is (= {:a 1 :b 1} roots))
      (is (= '() unsolved))))

  (testing "With unsolvable and inconsistent terms"
    (let [ eqs [{:a 1 :b 1 :c 0 := 2}
                {:a 1 :b 0 := 1}
                {:a 1 :c 1 :d 1}
                {:b 5 :c 1 :d 1}]]
      (is (thrown-with-msg? IllegalArgumentException
                            #"Inconsistent equations"
                            (mp-solve eqs)))))

  (testing "With unsolvable var"
    (let [ eqs [{:x 1 :y 2 :z 3 := 14} ;; roots 1 2 3
                {:x 5 :y 1 :z 1 := 10}
                {:x 3 :y 2 :z 2 := 13}
                {:u 1 :v 1 := 0}]
           [roots unsolved] (mp-solve eqs)]
      (is (= {:x 1 :y 2 :z 3} roots))
      (is (= '({:u 1 :v 1}) unsolved))))

  (testing "With unsolvable var"
    (let [ eqs [{:x 1 :y 2 :z 3 := 14} ;; roots 1 2 3
                {:x 5 :y 1 :z 1 := 10}
                {:x 3 :y 2 :z 2 := 13}
                {:x 9 :u 1 :v 1 := 0}]
           [roots unsolved] (mp-solve eqs)]
      (is (= (count unsolved) 1))
      (is (eq-proportional? '{:= 14/3, :v -14/27, :u -14/27} (first unsolved)))))
)
