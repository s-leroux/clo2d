(ns clo2d.infix-test
  (:require [clojure.test :refer :all]
            [clo2d.infix :refer :all]
            [clojure.java.io :refer :all])
  (:import (java.io File)))

(deftest infix-parser-test
  (testing "parser"
    (let [eq1 [2 :x + 3 :y = 4 ]
          eq2 [2 :x + 3 :y - 4 ]
          eq3 [2 :x + 3 :y - 4 = 0 ]
          eq4 [-4 = -2 :x + -3 :y ] 
          eq5 [ 2 :x ] ]
      (is (= (first (parse-infix eq1)) {:x 2 :y 3 := 4 }))
      (is (= (first (parse-infix eq2)) {:x 2 :y 3 := 4 }))
      (is (= (first (parse-infix eq3)) {:x 2 :y 3 := 4 }))
      (is (= (first (parse-infix eq4)) {:x 2 :y 3 := 4 }))
      (is (= (first (parse-infix eq5)) {:x 2 }))))

  (testing "parser"
    (let [eq1 [2 [ :x + 3 :y - 2 ] ] 
          eq2 '(2 ( :x + 3 :y - 2)) ]
      (is (= (first (parse-infix eq1)) {:x 2 :y 6 := 4 }))
      (is (= (first (parse-infix eq2)) {:x 2 :y 6 := 4 })) ))

  (doseq [expr [ '()
                 '(2)
                 '(2 + 2)
                 '(2 :p)
                 '(2 :p - :p)
               ]]
    (parse-infix expr))
)

;;
;; p-eq algebra
;;
(deftest p-eq-algebra-test
  (testing "add"
    (doseq[[s1 s2 out] [['({:= 3} {:x 1}) '({:= 1}) '({:= 4} {:= 1 :x 1})]] ]
      (println s1 s2)
      (is (= (p-add s1 s2) out)))))

