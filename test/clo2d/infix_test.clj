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
          eq4 [-4 = -2 :x + -3 :y ] ]
      (is (= (parse-infix eq1) {:x 2 :y 3 := 4 }))
      (is (= (parse-infix eq2) {:x 2 :y 3 := 4 }))
      (is (= (parse-infix eq3) {:x 2 :y 3 := 4 }))
      (is (= (parse-infix eq4) {:x 2 :y 3 := 4 }))))

  (testing "parser"
    (let [eq1 [2 [ :x + 3 :y - 2 ] ] 
          eq2 '(2 ( :x + 3 :y - 2)) ]
      (is (= (parse-infix eq1) {:x 2 :y 6 := 4 }))
      (is (= (parse-infix eq2) {:x 2 :y 6 := 4 })) ))

)
