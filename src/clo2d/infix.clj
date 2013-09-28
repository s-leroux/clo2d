(ns clo2d.infix
  (:use clojure.set)
  (:require [clo2d.linear :as l]))

(defn m-product
  [ map factor ]
  (let [ks (keys map)]
    (zipmap ks (for [k ks] (* factor (map k))))))

(defn back-eval
  [ values ops ]
    (let [op (first ops)
          value (first values)]
      (case op
        := (recur (cons (m-product value -1) (rest values)) 
                  (cons :+ (rest ops)))
        :+ (let [values (rest values)
                 v2 (first values)]
             (recur (cons (merge-with + value v2) (rest values))
                    (rest ops)))

        value)))

(defn parse-infix
  "Parse an infix (linear) equation" ;; XXX should return normalized eq.
  [ eq ]
  (loop [ terms (cons 0 (cons + eq)) ; force constant term in equation
          values '()
          ops '() 
          sign 1 ]
    (let [ term (first terms)
           tail (rest terms) ]
      (cond
        (not term)
        (back-eval values ops)

        (#{+,:+,'+} term)
        (recur (rest terms) values (cons :+ ops) 1)

        (#{-,:-,'-} term)
        (recur (rest terms) values (cons :+ ops) -1)

        (#{=,:=,'=} term)
        (recur (rest terms) values (cons := ops) 1)

        (or (keyword? term) (coll? term))
        (recur (cons sign terms) values ops 1)

        (number? term)
        (let [k (first tail) r (rest tail)]
          (cond
            (keyword? k)
            (recur r (cons (m-product { k sign } term) values) ops 1)

            (coll? k)
            (recur r (cons (m-product (parse-infix k) term) values) ops 1)

            true
            (recur tail (cons { := (* (- sign) term)} values) ops 1)))
      ))))

;;
;; Parallel eq algebra
;;
(defn p-do
  [f eq1 eq2]
  (let [c1 (count eq1)
        c2 (count eq2)]
    (cond
      (= c1 c2)
      (map f eq1 eq2)

      (and (= 1 c1) (> c2 1))
      (map f (cycle eq1) eq2)

      (and (= 1 c2) (> c1 1))
      (map f eq1 (cycle eq2))

      :default
      (throw (IllegalArgumentException. "Ill formed expression")))))

(defmacro p-add
 [eq1 eq2]
 `(p-do #(l/eq-add %1 %2) ~eq1 ~eq2))

(defmacro p-sub
 [eq1 eq2]
 `(p-do #(l/eq-sub %1 %2) ~eq1 ~eq2))
   
(defmacro p-mul
 [eq1 eq2]
 (let [f (fn[a b]
           (cond 
             (l/eq-constant? a)
             (l/eq-mul b (- (:= a)))
  
             (l/eq-constant? b)
             (l/eq-mul a (- (:= b)))
 
             :default
             (throw (IllegalArgumentException. "Could only multiply by constant"))
             )
            
        )]
   `(p-do ~f ~eq1 ~eq2)))

;;
;; New Shunting-yard parser
;; (http://en.wikipedia.org/wiki/Shunting-yard_algorithm)
;;

(defn push-atom
  [atom out stack]
  (let [value (if (keyword? atom) { atom 1 } { := (- atom) })]
    [(cons (list value) out) stack]))

(defn eval-op
  [op out stack]
  (let [[b a & tail] out]
    ; (println "eval" op a b out stack)
    (case op
      :+ [(cons (p-add a b) tail) stack]
      :- [(cons (p-sub a b) tail) stack]
      := [(cons (p-sub a b) tail) stack]
      :* [(cons (p-mul a b) tail) stack]
    )))

(defmacro point?
  [ form ]
  `(not (some #(not (or (keyword? %1) (number? %1) (seq? %1))) ~form)))

(defn parse-op
  [ op out stack ]
  (let [prec { :+     5
               :-     5
               :*     6
               :=     4 
               :start 0 }
        [o2 & tail] stack]
    (if (and o2 (> (prec o2) (prec op)))
      (let [[out stack] (eval-op o2 out tail)]
        (recur op out stack ))
      [out (cons op stack)])))

(defn parse-end-expr
 [ out stack ]
 (if-let [[op & tail] (seq stack)]
   (if (not= op :start)
     (let [[out stack] (eval-op op out tail)]
       (recur out stack))
     [out tail])))

(defn parse-seq
  [ lst out stack ]
  (if (point? lst)
    (loop [ s lst out (cons () out) stack stack ]
      (if-let [[term & tail] s]
        (let [[out stack] (parse-expr (list term) out (cons :start stack) false)]
          (recur tail (let [[a b & tail] out] (cons (concat b a) tail)) stack))
        [out stack]))
    (parse-expr lst out (cons :start stack) false)))

(defn parse-expr
  [ terms out stack out-prec]
  (let [ [term & tail] terms]
    (cond
      (not term)
      (parse-end-expr out stack)

      (contains? #{:+,+,'+} term)
      (let [[out stack] (parse-op :+ out stack )]
        (recur tail out stack false))

      (contains? #{:-,-,'-} term)
      (let [[out stack] (parse-op :- out stack )]
        (recur tail out stack false))

      (contains? #{:*,*,'*} term)
      (let [[out stack] (parse-op :* out stack )]
        (recur tail out stack false))

      (contains? #{:=,=,'=} term)
      (let [[out stack] (parse-op := out stack )]
        (recur tail out stack false))

      (seq? term)
      (if out-prec
        (recur (cons :* terms) out stack out-prec)
        (let [[out stack] (parse-seq term out stack)]
          (recur tail out stack true)))

      (or (number? term) (keyword? term))
      (if out-prec
        (recur (cons :* terms) out stack out-prec)
        (let [[out stack] (push-atom term out stack)]
          (recur tail out stack true)))
    )))

(defn parse-infix2
  "Parse an infix (linear) equation.

  Some sample syntax:
  - Producing one equation:
    ( 2 :y + 3 = 3 :y )
    ( :k ( 1 + 2 ) = :v )
    ( 3 ( :a + 2 :b) = :c )

  - Shortcut to produce two or more equations
    ( 2 :y + (5 6) = 3 :y ) 
    ( :k ( (1 2) + (3 4) ) = :v )
    ( 3 ( :a + 2 :b ) = :c )

  More strictly:
  - A single keyword is assumed to by an unknown of coefficient 1
  - A pair of terms is assumed to be a scalar coefficient followed by an
  equation
  - A list whose second term is missing or is not an operator is assumed
    to be a shortcut expression to produce several equations.
  "
  ;; XXX should return normalized eq. ?
  [ eq ]
  (let [[out stack] (parse-expr eq () '( :start ) false)]
    (if (or (not= (count out) 1) (seq stack))
      (throw (IllegalArgumentException. "Ill formed expression"))
      (first out))))
