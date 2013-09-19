(ns clo2d.infix
  (:use clojure.set))

(defn push
  [stack v]
  (cons v stack))

(defn pop
  [stack]
  (let [[v & tail] stack ]
    [v (or tail '())]))

(defn product
  [ map factor ]
  (let [ks (keys map)]
    (zipmap ks (for [k ks] (* factor (map k))))))

(defn back-eval
  [ values ops ]
    (let [op (first ops)
          value (first values)]
      (case op
        := (recur (cons (product value -1) (rest values)) 
                  (cons :+ (rest ops)))
        :+ (let [values (rest values)
                 v2 (first values)]
             (recur (cons (merge-with + value v2) (rest values))
                    (rest ops)))

        value)))


(defn parse-infix
  "Parse an infix (linear) equation"
  [ eq ]
  (loop [ terms eq
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
            (recur r (cons (product { k sign } term) values) ops 1)

            (coll? k)
            (recur r (cons (product (parse-infix k) term) values) ops 1)

            true
            (recur tail (cons { := (* (- sign) term)} values) ops 1)))
      ))))
