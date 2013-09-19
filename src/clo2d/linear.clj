(ns clo2d.linear
  (:use clojure.set))

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

(defn all
  "Is the predicate true for all values of a  collection?"
  ( [ f ] true)
  ( [ f coll ]
     (if (seq coll) (if (f (first coll)) (recur f (rest coll))
                                         false)
                    true)))

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
      (if (seq head)
        (if (> (abs (first head)) (abs (first top)))
          (recur head (cons top result) tail)
          (recur top (cons head result) tail)
        )
        [top result]))))

(defn row-reduce
  [ head eqs ]
    (let [factor (first head)
          vec    (rest head)]
      (if (and vec (empty? eqs))
          (recur head (list (map (constantly 0) head)))
          (let [pivot  (if (fzero? factor)
                         (fn [eq] (map (constantly 0) (rest eq)))
                         (fn [eq]
                           (let [h (first eq) 
                                 t (rest eq)
                                 c (/ h factor)]
                             (product #(- %2 (* %1 c)) vec t)))) ]
            (map pivot eqs)))))

(defn pivot
  ( [ eqs ] (pivot eqs '()))
  (
  [ eqs result ]
  (if (empty? eqs)
    result
    (let [[eq bag] (reorder eqs)]
        ; (println "eqbag" eq bag result)
        (let [ rr (if (seq (rest eq)) (row-reduce eq bag) bag) ]
          
          ; (println "rr" rr (cons eq result))
          (recur rr (cons eq result)))))))

(defn solve
  [ eqs ]
  (let [p (pivot eqs)]
    ; (println "p" p)
    (loop [ roots '()
            eqs p ]
      (let [ eq (first eqs) ]
        (if eq
          (let [ [ head & tail ] eq ]
              (if (fzero? head)
                (if tail
                  (if (all #(fzero? %1) tail )
                    (throw (IllegalArgumentException. "Unsolvable"))
                    (throw (IllegalArgumentException. "Inconsistent equations")))
                  (recur roots (rest eqs)))
                (if (seq tail) 
                  (let [ [s r] (accumulate tail roots) ]
                    (recur (cons (/ (- r s) head ) roots) 
                           (rest eqs)))
                  (throw (IllegalArgumentException. "Inconsistent equations")))))
          roots )))))

(defn indeps
  "Parse a collection of equation expressed as a map to
  collect all independant terms (in fact, keys of the maps)"
  ([eqs] (indeps eqs #{}))
  ([eqs result]
    (if (seq eqs)
      (recur (rest eqs) (union result (set (keys (first eqs)))))
      (disj result :=))))

(defn parse-eq
  [ eqs ]
  (let [ keys (indeps eqs) ]
    (loop [ result '()
            eqs eqs]
      (if (seq eqs)
        (let [ eq (first eqs) ]
          (recur (cons (for [x (concat keys '(:=))] (or (eq x) 0) ) result) (rest eqs)))
        [keys result]))))

(defn solve-eq
  [ eqs ]
  (let [ [keys eqs] (parse-eq eqs)
         roots (solve eqs)]
    (zipmap keys roots)))

