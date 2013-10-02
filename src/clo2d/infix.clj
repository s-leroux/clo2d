(ns clo2d.infix
  (:use clojure.set)
  (:require [clo2d.linear :as l]))


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

      ; (and (= 1 c1) (> c2 1))
      (> c2 c1)
      (map f (cycle eq1) eq2)

      ; (and (= 1 c2) (> c1 1))
      (> c1 c2)
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
             (l/eq-mul b (- (get a := 0)))
  
             (l/eq-constant? b)
             (l/eq-mul a (- (get b := 0)))
 
             :default
             (throw (IllegalArgumentException. "Can only multiply by constant"))
             )
            
        )]
   `(p-do ~f ~eq1 ~eq2)))

(def ^:dynamic **) ;; homothecy operator (dynamic to silent warning :/ )

(defn p-homothecy
  [factor a-b]
  (let [[a b] (split-at (/ (count a-b) 2) a-b)]
    (p-add a (p-mul factor (p-sub b a)))))

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
      :** [(cons (p-homothecy a b) tail) stack]
    )))

(defmacro point?
  [ form ]
  `(not (some #(not (or (keyword? %1) (number? %1) (coll? %1))) ~form)))

(defn parse-op
  [ op out stack ]
  (let [prec { :+     4
               :-     4
               :*     7
               :**    5
               :=     2 
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

(declare parse-term)
(declare parse-expr)

(defn parse-seq
  [ lst out stack ]
  (if (point? lst)
    (loop [ s lst out (cons () out) stack stack ]
      (if-let [[term & tail] (seq s)]
        (let [[out stack] (parse-term term out stack)]
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

      (contains? #{:**,**,'**} term)
      (let [[out stack] (parse-op :** out stack )]
        (recur tail out stack false))

      (contains? #{:=,=,'=} term)
      (let [[out stack] (parse-op := out stack )]
        (recur tail out stack false))

      (coll? term)
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

(defn parse-term
  [ term out stack ]
  (if (coll? term)
    (parse-seq term out stack)
    (push-atom term out stack)))

(defn parse-infix
  "Parse an infix (linear) equation system."
  ;; XXX should return normalized eq. ?
  [ term ]
  (assert (not (nil? term)))
  (let [[out stack] (parse-term term () '())]
    (if (or (> (count out) 1) (seq stack))
      (throw (IllegalArgumentException. "Ill formed expression"))
      (first out))))
