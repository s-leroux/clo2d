(ns clo2d.shape
  (:use clo2d.infix clo2d.linear))

(defn updt-keywords
 "Recursivly walk through a collection to prefix keywords by the given
 string"
 [ prefix coll ]
 (let [f (fn [ x ]
           (or 
             (and (keyword? x) 
                  (not (#{:=,:+,:-} x)) 
                  (keyword (str prefix ":" (name x))))
             (and (coll? x) (updt-keywords prefix x))
             x)) ]
    (map f coll)))

(defn prefix-map
 "prefix keys from a map by the given string"
 [ prefix map ]
 (let [ ks (keys map) 
        f (fn [ x ]
            (or 
              (and (keyword? x) 
                   (not (#{:=,:+,:-} x)) 
                   (keyword (str prefix ":" (name x))))
              (and (coll? x) (updt-keywords prefix x))
              x)) ]
   (into {} (for [[k v] map] [(f k) v]))))

(def ^:const point
  (map parse-infix [ '( :x = :x )
                     '( :y = :y ) ]))

(def ^:const rectangular
  (map parse-infix [ '( :bottom - :top = :height ) 
                     '( 2 ( :bottom - :center-y ) = :height )
                     '( :right - :left = :width )
                     '( 2 ( :right - :center-x ) = :width ) ]))

(def ^:const square
  (concat rectangular (map parse-infix [
                     '( :height = :width ) ])))

(defmacro shape
  [ name eqs ]
  `(map #(prefix-map ~name %1) ~eqs))

(defmacro group
  [ & body ]
  `(mp-solve (concat ~@body)))

(defmacro having
  [ & constraints ]
  `(map parse-infix '~constraints))

(defn fold*
  [ prefix & kw-list ]
  (loop [ map {} kw-list kw-list]
     (if-let [[kw & tail] kw-list]
       (recur (assoc map (keyword (str prefix kw)) kw) tail)
       map)))

(defmacro unfold
  [ k ]
  `(let [k# (name ~k)
         idx# (.lastIndexOf k# ":")]
     [ (keyword (.substring k# 0 idx#))
       (keyword (.substring k# (inc idx#)))]))

(defn unfold*
  [ map ]
  (loop [ result {} iter (seq map) ]
    (if-let [[key value] (first iter)]
      (let [[h t] (unfold key)
            m     (get result h {})]
        (recur (assoc result h (assoc m t value)) (rest iter)))
      result)))

(defmacro gget
  [ prefix map & kw-list ]
  `(let [kw#     (fold* ~prefix ~@kw-list)]
     (reduce #(assoc %1 (kw# %2) (~map %2)) {} (keys kw#))))

(defmacro make-xy 
 [ kw x y ]
 `[ (keyword (str (name ~kw) (str ~x)))
    (keyword (str (name ~kw) (str ~y))) ])

(defmacro center
 [ kw ]
 `(make-xy ~kw ":x" ":y"))

(defmacro nw
 [ kw ]
 `(make-xy ~kw ":left" ":top"))

(defn --
 [ map & points ]
 (loop [points points result [] ]
   (if-let [[p & tail] points]
     (if (keyword? p)
       (recur (cons (center p) tail) result)
       (let [[x y] p]
         (recur tail (conj result (map x) (map y)))))
   result)))

(defn rect
  [ prefix map ]
  (gget prefix map :top :left :width :height))
