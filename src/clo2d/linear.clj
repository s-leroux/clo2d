(ns clo2d.linear
  (:use clojure.set))

;;
;; utilities
;;

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

(defmacro abs
 [ x ]
 `(if (neg? ~x) (- ~x) ~x))

(defmacro product
  [ ^clojure.lang.IFn f a b ]
  `(map #(~f %1 %2) ~a ~b))

(defmacro mp-product
  [ ^clojure.lang.IFn f a b ]
  `(reduce (fn [ map# [ k# v# ] ]
              (let [p# (~f (get map# k# 0) v#)]
                (assoc map# k# p#)))
      ~a ~b))

(defmacro mp-eval
  [ eq ctx ]
  `(reduce (fn [ map# [ k# v# ] ]
             ; (println map# k# v#)
             (if-let [ r# (~ctx k#) ]
               (assoc map# := (- (get map# := 0) (* v# r#)))
               (assoc map# k# (if (= k# :=) (+ (get map# := 0) v#) v#))))
        {} ~eq))

(defmacro mp-resolve
  [ eq ]
  `(let [ cst# (get ~eq := 0)
          m# (dissoc (eq-norm ~eq) :=) ]
     ; (println "rs" ~eq)
     (case (count m#)
       0 (if (fzero? cst#) 
             [true {}] 
             (throw (IllegalArgumentException.  "Inconsistent equations")))
       1 (let [[k# v#] (first m#)]
           (cond
             (and (fzero? cst#) (fzero? v#))
             [true {}]
             
             (fzero? v#)
             (throw (IllegalArgumentException.  "Inconsistent equations"))

             :default
             [true {k# (/ cst# v#)}])
         )

       [false (reduce (fn [map# [k# v#]] (assoc map# k# nil)) {} m#)]
     ))) 

(defn a-key
  "Returns a non-zero key from a set of maps rejecting :=."
  [ maps ]
  (let [eq    (first maps)
        [k v] (first eq)]
    (cond
      (nil? eq) nil
      (nil? k)  (recur (rest maps))
      (or (= k :=) (fzero? v))  (recur (cons (rest eq) (rest maps)))
      :default  k
    )))

(defn eq-norm
  "Normalize equation by keeping only non-zero terms"
  [ eq ]
  (loop [ result {} eq eq ]
    (if-let [[k v] (first eq)]
      (if (and (fzero? v) (not= k :=))
        (recur result (rest eq))
        (recur (assoc result k v) (rest eq)))
    result)))

(defn eq-div
  "Euclidian division of one equation by the other"
  [terms eq1 eq2]
  (apply min (map #(/ (get eq1 %1 0) (eq2 %1)) terms)))

(defn eq-*+
  "fused multiply-add operation on equations.
  Multiply eq2 by x and add the result to eq1."
  [eq1 x eq2]
  (loop [result eq1 eq2 eq2]
    (if-let[[k v] (first eq2)]
      (recur (assoc result k (+ (get result k 0) (* x v))) (rest eq2))
      result)))

(defmacro eq-*-
  "fused multiply-sub operation on equations."
  [eq1 x eq2]
  `(eq-*+ ~eq1 (- ~x) ~eq2))

(defn eq-terms
  "Return all non-null terms from an equation"
  [ eq ]
  (loop [ result '() eq eq]
    (if-let [[k v] (first eq)]
      (if (or (zero? v) (= k :=))
        (recur result (rest eq))
        (recur (cons k result) (rest eq)))
    result)))


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

;;
;; New map based Gauss linear equation solver
;;
(defn mp-reorder 
  "Find the equation having the greatest factor for key `k`"
  [ k eqs ]
  (loop [top (first eqs)
         result []
         bag (rest eqs)]
    (let [head (first bag) ;; should use if-let ???
          tail (rest bag)]
      (if (seq head)
        (if (> (abs (get head k 0)) (abs (get top k 0)))
          (recur head (cons top result) tail)
          (recur top (cons head result) tail)
        )
        [top result]))))

(defn mp-row-reduce
  "(Try to) eliminated the given key in `eqs` by linear combination with
  `head`"
  [ k head eqs ]
  (if k
    (let [factor (head k)
          vec    (dissoc head k)]
      (if (and vec (empty? eqs))
          (recur k head (list {:= 0}))
          (let [pivot  (if (fzero? factor)
                         (fn [eq] {:= 0}) ; XXX should remove that case
                         (fn [eq]
                           (let [h (get eq k 0) 
                                 t (dissoc eq k)
                                 c (/ h factor)]
                             (mp-product #(- %1 (* %2 c)) t vec)))) ]
            (map pivot eqs))))
    eqs))

(defn mp-pivot
  ( [ eqs ] (mp-pivot eqs {})) ; XXX should be a list here?
  (
  [ eqs result ]
  (if (empty? eqs)
    result
    (let [k (a-key eqs)
          [eq bag] (mp-reorder k eqs)]
        ; (println "k eq bag result" k eq bag result)
        (let [ rr (if (seq (rest eq)) (mp-row-reduce k eq bag) bag) ]
          
          (recur rr (cons eq result)))))))

(defn mp-diff-n
  "Remove n times `eq` from equations in the `bag`."
  [bag eq]
  (let [terms (eq-terms eq)]
    (map (fn[e] (let [dv (eq-div terms e eq)] (eq-*- e dv eq))) bag)))

(defn mp-solve
  [ eqs ]
  (let [p (mp-pivot eqs)]
    ; (println "p" p)
    (loop [ roots {}
            eqs p 
            unsolved '()]
      ; (println eqs unsolved)
      (let [ eq (first eqs) ]
        ; (println "eq:" eq)
        (if eq
          (let [ s (mp-eval eq roots)
                 [solved? vals] (mp-resolve s) ]
            ; (println "s" s)
            (if solved?
              (recur (merge roots vals)
                     (rest eqs)
                     unsolved)
              (recur (merge roots vals) ;; ??? if not solved, nothing to merge?
                     (mp-diff-n (rest eqs) s)
                     (cons s unsolved))))
        [roots unsolved])))))
