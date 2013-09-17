(ns clo2d.linear
  (:import ()))

;;
;; utilities
;;

(defmacro abs
 [ x ]
 `(if (neg? ~x) (- ~x) ~x))

(defmacro product
  [ ^clojure.lang.IFn f a b ]
  `(map #(~f %1 %2) ~a ~b))

(defn accumulate
  "Given a vector and the first root values
  returns the sum v(i)*r(i) and any unused v(j) terms"
  ( [ vector roots ] (accumulate vector roots 0))
  ( [ vector roots result ]
    (let [vi (first vector)
          ri (first roots)]
      (if ri
        (recur (rest vector) (rest roots) (+ (* vi ri) result))
        (cons result vector)))))


(def ^:const +epsilon 1e-5)
(def ^:const -epsilon (- +epsilon))

(defmacro fzero?
  "Test if a number is close to zero"
  ( [ x ] `(fzero? ~x -epsilon +epsilon))
  ( [ x epsilon] `(fzero? ~x (- ~epsilon) ~epsilon))
  ( [ x -epsilon +epsilon ] `(< ~-epsilon ~x ~+epsilon)))

;;
;; Gauss solver
;;
(defn reorder 
  [ eqs ]
  (loop [top (first eqs)
         result []
         bag (rest eqs)]
    (let [head (first bag)
          tail (rest bag)]
      (if head
        (if (> (abs (first head)) (abs (first top)))
          (recur head (cons top result) tail)
          (recur top (cons head result) tail)
        )
        [top result]))))

(defn row-reduce
  [ head eqs ]
  (let [factor (first head)
        vec    (rest head)
        pivot  (fn [eq]
                 (let [h (first eq) 
                       t (rest eq)
                       c (/ h factor)]
                    (product #(- %2 (* %1 c)) vec t)))]
    (map pivot eqs)))

(defn pivot
  ( [ eqs ] (pivot eqs '()))
  (
  [ eqs result ]
  (let [[eq bag] (reorder eqs)]
    (if eq
      (let [ rr (row-reduce eq bag) ]
        (println "rr" rr (cons eq result))
        (recur rr (cons eq result)))
      result))))

(defn solve
  [ eqs ]
  (let [p (pivot eqs)]
    (loop [ roots '()
            eqs p ]
      (let [ eq (first eqs) ]
        (if eq
          (let [ eqs (rest eqs)
                 a (first eq)
                 [s r] (accumulate (rest eq) roots) ]
            (recur (cons (/ (- r s) a) roots) eqs))
          roots )))))

