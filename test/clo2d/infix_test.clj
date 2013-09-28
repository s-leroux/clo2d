(ns clo2d.infix-test
  (:require [clojure.test :refer :all]
            [clo2d.infix :refer :all]
            [clojure.java.io :refer :all])
  (:import (java.io File)))

(deftest base-functions-test
  (testing "Product"
    (let [ m {:a 1 :b 2 :c 3} ]
      (is (= (m-product m 3) {:a 3 :b 6 :c 9})))))

(deftest infix-parser-test
  (testing "back-eval add"
   (let [ res (back-eval '({:a 3} {:b 1}) '(:+)) ]
     (is (= res '{:a 3 :b 1})))
  )

  (testing "back-eval add same"
   (let [ res (back-eval '({:a 3} {:a 1}) '(:+)) ]
     (is (= res '{:a 4})))
  )

  (testing "back-eval add same"
   (let [ res (back-eval '({:a 3} {:a 1 :b 2}) '(:+)) ]
     (is (= res '{:a 4 :b 2})))
  )

  (testing "back-eval eq same"
   (let [ res (back-eval '({:a 3} {:a 1 :b 2}) '(:=)) ]
     (is (= res '{:a -2 :b 2})))
  )

  (testing "parser"
    (let [eq1 [2 :x + 3 :y = 4 ]
          eq2 [2 :x + 3 :y - 4 ]
          eq3 [2 :x + 3 :y - 4 = 0 ]
          eq4 [-4 = -2 :x + -3 :y ] 
          eq5 [ 2 :x ] ]
      (is (= (parse-infix eq1) {:x 2 :y 3 := 4 }))
      (is (= (parse-infix eq2) {:x 2 :y 3 := 4 }))
      (is (= (parse-infix eq3) {:x 2 :y 3 := 4 }))
      (is (= (parse-infix eq4) {:x 2 :y 3 := 4 }))
      (is (= (parse-infix eq5) {:x 2 := 0 }))))

  (testing "parser"
    (let [eq1 [2 [ :x + 3 :y - 2 ] ] 
          eq2 '(2 ( :x + 3 :y - 2)) ]
      (is (= (parse-infix eq1) {:x 2 :y 6 := 4 }))
      (is (= (parse-infix eq2) {:x 2 :y 6 := 4 })) ))

)

;;
;; p-eq algebra
;;
(deftest p-eq-algebra-test
  (testing "add"
    (doseq[[s1 s2 out] ['({:= 3} {:x 1}) '({:= 1}) '({:= 4} {:= 1 :x 1})]]
      (is (= (p-add s1 s2) out)))))

(deftest parse-infix2-test
  (doseq [expr [ '()
                 '(2)
                 '(2 + 2)
                 '(2 :p)
                 '(2 :p - :p)
               ]]
    (parse-infix2 expr)))
