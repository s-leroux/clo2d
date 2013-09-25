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
  `(let [[m# cst#] (eq-norm* ~eq) ]
     ; (println "rs" ~eq)
     (case (count m#)
       0 (if (fzero? cst#) 
             [true {}] 
             (throw (IllegalArgumentException.  "Inconsistent equations")))
       1 (let [[k# v#] (first m#)]
           ;; v# can't be zero for normalized equations
           [true {k# (/ cst# v#)}])

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

(defn eq-norm*
  "Normalize equation by keeping only non-zero terms.
  Constant term is returned separately."
  [ eq ]
  (loop [ result {} eq eq cst 0]
    (if-let [[k v] (first eq)]
      (cond
        (= := k)   (recur result (rest eq) v)
        (fzero? v) (recur result (rest eq) cst)
        :default   (recur (assoc result k v) (rest eq) cst))
    [result cst])))

(defn eq-norm
  "Normalize equation by keeping only non-zero terms."
  [ eq ]
  (loop [ result {} eq eq]
    (if-let [[k v] (first eq)]
      (cond
        (fzero? v) (recur result (rest eq))
        :default   (recur (assoc result k v) (rest eq)))
    result)))

(defn eq-div
  "Euclidian division of one equation by the other"
  ( [eq1 eq2] (eq-div (keys eq2) eq1 eq2) )
  ( [terms eq1 eq2]
  (apply min (map #(/ (get eq1 %1 0) (eq2 %1)) terms)))
  )

(defn eq-madd
  "fused multiply-add operation on equations.
  Multiply eq2 by x and add the result to eq1."
  [eq1 x eq2]
  (loop [result eq1 eq2 eq2]
    (if-let[[k v] (first eq2)]
      (recur (assoc result k (+ (get result k 0) (* x v))) (rest eq2))
      result)))

(defmacro eq-msub
  "fused multiply-sub operation on equations."
  [eq1 x eq2]
  `(eq-madd ~eq1 (- ~x) ~eq2))

(defmacro eq-add
  "Add operation on equations."
  [eq1 eq2]
  `(eq-madd ~eq1 1 ~eq2))

(defmacro eq-sub
  "Sub operation on equations."
  [eq1 eq2]
  `(eq-madd ~eq1 -1 ~eq2))

(defmacro eq-mul
  "Multiply an equation by a constant."
  [eq1 x]
  `(eq-madd ~eq1 (dec ~x) ~eq1))

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
    (map (fn[e] (let [dv (eq-div terms e eq)] (eq-msub e dv eq))) bag)))

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
