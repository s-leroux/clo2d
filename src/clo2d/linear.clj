(ns clo2d.linear
  (:import ()))

;;
;; utilities
;;
(defmacro product
  [ ^clojure.lang.IFn f a b ]
  `(map #(~f %1 %2) ~a ~b))


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
        (if (> (first head) (first top))
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
                       c (/ factor h)]
                    (product #(- (* %2 c) %1) vec t)))]
    (map pivot eqs)))

