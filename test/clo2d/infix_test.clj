(ns clo2d.infix-test
  (:require [clojure.test :refer :all]
            [clo2d.infix :refer :all]
            [clojure.java.io :refer :all])
  (:import (java.io File)))

(deftest infix-parser-test
  (testing "parser (basic case)"
    (doseq [[in out] [ ['()           '()]
                       ['(2)          '({:= -2})]
                       ['(2 + 2)      '({:= -4})]
                       ['(2 :p)       '({:= -2}{:p 1})] ;; Trick case !!!
                       ['(2 :p = 0)   '({:p 2})] ;; Trick case !!!
                       ['(2 :p - :p)  '({:p 1})]
                 ]]
      (is (= (parse-infix in) out))))

  (testing "parser"
    (let [eq1 [2 :x + 3 :y = 4 ]
          eq2 [2 :x + 3 :y - 4 ]
          eq3 [2 :x + 3 :y - 4 = 0 ]
          eq4 [-4 = -2 :x + -3 :y ] 
          eq5 [ 2 * :x ] ]
      (is (= (first (parse-infix eq1)) {:x 2 :y 3 := 4 }))
      (is (= (first (parse-infix eq2)) {:x 2 :y 3 := 4 }))
      (is (= (first (parse-infix eq3)) {:x 2 :y 3 := 4 }))
      (is (= (first (parse-infix eq4)) {:x 2 :y 3 := 4 }))
      (is (= (first (parse-infix eq5)) {:x 2 }))))

  (testing "parser (distributivity)"
    (let [eq1 [2 * [ :x + 3 :y - 2 ] ] 
          eq2 '(2 * ( :x + 3 :y - 2)) ]
      (is (= (first (parse-infix eq1)) {:x 2 :y 6 := 4 }))
      (is (= (first (parse-infix eq2)) {:x 2 :y 6 := 4 })) ))

  (testing "parser (vector vs expr.)"
    (doseq [[in out] [ ['(2 3)          '({:= -2}{:= -3})]
                       ['((2 3)(4 5))   '({:= -2}{:= -3}{:= -4}{:= -5})]
                       ['((2 3 4 5))    '({:= -2}{:= -3}{:= -4}{:= -5})]
                       ['(2 3 4 5)      '({:= -2}{:= -3}{:= -4}{:= -5})]
                       ['(2 (3 4) 5)    '({:= -2}{:= -3}{:= -4}{:= -5})]
                       ['(2 (3 4 5))    '({:= -2}{:= -3}{:= -4}{:= -5})]
                       ['(2 * (3 4 5))  '({:= -6}{:= -8}{:= -10})]
                       ['(2 (3 * 4 5))  '({:= -2}{:= -60})]
                 ]]
      (is (= (parse-infix in) out))))

  (testing "parser (mediation)"
    (doseq [[in out] [ ['( 0/3 ** ((1 10) (4 40))) '({:= -1}{:= -10})]
                       ['( 1/3 ** ((1 10) (4 40))) '({:= -2}{:= -20})]
                       ['( 2/3 ** ((1 10) (4 40))) '({:= -3}{:= -30})]
                       ['( 3/3 ** ((1 10) (4 40))) '({:= -4}{:= -40})]

                       ['( :r ** ((1 10) (4 40))) '({:r 3 := -1}{:r 30 := -10})]
                 ]]
      (is (= (parse-infix in) out))))
)

;;
;; p-eq algebra
;;
(deftest p-eq-algebra-test
  (testing "add"
    (doseq[[s1 s2 out] [['({:= 3} {:x 1}) '({:= 1}) '({:= 4} {:= 1 :x 1})]] ]
      (is (= (p-add s1 s2) out)))))

