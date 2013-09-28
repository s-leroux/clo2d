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

(defn eq-constant?
  "Is the equation constant. That is, does only contain
  := as non-zero term."
  [ eq ]
  (loop [terms (seq eq)]
    (if-let [[[k v] & tail] terms]
      (and (or (= := k) (fzero? v))
           (recur tail))
      true)))

(defn eq-proportional?
  "Are two equations proportional?"
  ( [eq1 eq2] 
    (eq-proportional? (into #{} (concat (keys eq2) (keys eq1))) eq1 eq2)
  )
  ( [terms eq1 eq2]
    (loop [val nil terms terms]
      (if-let [[k & tail] (seq terms)]
        (let [v1 (get eq1 k 0)
              v2 (get eq2 k 0)
              z1 (fzero? v1)
              z2 (fzero? v2)]
          (cond
            (and z1 z2) (recur val tail)
            (or z1 z2)  nil
            (nil? val)  (recur (/ v1 v2) tail)

            :default
            (let [c (/ v1 v2)]
              (if (fzero? (- c val))
                (recur c tail)
                nil))
          )
        )
        val
      )
    )
))

(defn eq-terms
  "Return all non-null terms from an equation"
  [ eq ]
  (loop [ result '() eq eq]
    (if-let [[k v] (first eq)]
      (if (or (zero? v) (= k :=))
        (recur result (rest eq))
        (recur (cons k result) (rest eq)))
    result)))

(defn eq-div
  "Euclidian division of one equation by the other"
  ( [eq1 eq2] (eq-div (eq-terms eq2) eq1 eq2) )
  ( [terms eq1 eq2]
  (apply min (map #(/ (get eq1 %1 0) (eq2 %1)) terms)))
  )

(defn eq-madd
  "fused multiply-add operation on equations.
  Multiply eq2 by x and add the result to eq1. 
  Returns a normalized eq."
  [eq1 x eq2]
  (loop [result eq1 eq2 eq2]
    (if-let[[k v] (first eq2)]
      (let [new-val (+ (get result k 0) (* x v))]
        (if (fzero? new-val)
          (recur (dissoc result k) (rest eq2))
          (recur (assoc result k new-val) (rest eq2))
        ))
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

(defn eq-eliminate
 "Eliminate the given key (only one) from `eq1` by linear 
 combination with `eq2`"
 [ k eq1 eq2 ]
 (let [c1 (get eq1 k 0)
       c2 (get eq2 k 0)]
    (eq-msub eq1 (/ c1 c2) eq2)))

(defn eq-eval
  "Evaluate (''reduce'') an equation in the given context (''term
  bindings''). Return a normalized eq."
  [ eq ctx ]
  (loop [ result {} 
          cst 0 
          [head & tail] (seq eq) ]
    (if-let [[k v] head]
      (if (= := k) 
        (recur result (+ cst v) tail)
        (if-let [ r (ctx k) ]
          (recur result (- cst (* r v)) tail)
          (recur (if (fzero? v) result (assoc result k v)) cst tail)
        )
      )
      (if (fzero? cst) result (assoc result := cst))
    )
  )
)

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
      (if (nil? head)
        [top result]
        (if (> (abs (get head k 0)) (abs (get top k 0)))
          (recur head (cons top result) tail)
          (recur top (cons head result) tail)
        )
      ))))

(defn mp-row-reduce
  "(Try to) eliminated the given key in `eqs` by linear combination with
  `head`"
  [ k head eqs ]
  (if k
    (map #(eq-eliminate k %1 head) eqs)
    eqs))

(defn mp-pivot
  ( [ eqs ] (mp-pivot eqs '()))
  (
  [ eqs result ]
  (if (empty? eqs)
    result
    (let [k (a-key eqs)
          [eq bag] (mp-reorder k eqs)]
        ; (println "k eq bag result" k eq bag result)
        (let [ rr (if (seq (rest eq)) (mp-row-reduce k eq bag) bag) ]
                 ;;^^^^^^^^^^^^^^^^^^ XXX this is probably false
                 ;; for map-based eq. representation.

          ; (println "rr" rr)
          
          (recur rr (cons eq (mp-row-reduce k eq result))))))))

(defn mp-diff-n
  "Remove n times `eq` from equations in the `bag`."
  [bag eq]
  (let [terms (eq-terms eq)]
    (map (fn[e] (let [dv (eq-div terms e eq)] (eq-msub e dv eq))) bag)))

(defn mp-solve-in
  [ [ ctx init ] eqs ]
  ; (println "eqs" eqs)
  (let [eqs (map #(eq-eval %1 ctx) (concat init eqs))
        p (mp-pivot eqs)]
    ; (println "p" p)
    (loop [ roots ctx
            eqs p 
            unsolved '()]
      ; (println eqs unsolved)
      (let [ eq (first eqs) ]
        ; (println "eq:" eq)
        (if eq
          (let [ s (eq-eval eq roots)
                 [solved? vals] (mp-resolve s) ]
            ; (println "s" s solved? vals)
            (if solved?
              (recur (merge roots vals)
                     (rest eqs)
                     unsolved)
              (recur roots
                     (mp-diff-n (rest eqs) s)
                     (cons s unsolved))))
        [roots unsolved])))))

(defmacro mp-solve
  [ eqs ]
  `(mp-solve-in [{}()] ~eqs))
