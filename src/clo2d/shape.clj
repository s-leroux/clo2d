(ns clo2d.shape
  (:use clo2d.infix))

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


(def ^:const rectangular
  (map parse-infix[ '( :bottom - :top = :height ) 
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
  `(concat ~@body))

(defmacro having
  [ & constraints ]
  `(map parse-infix '~constraints))

(defn fold*
  [ prefix & kw-list ]
  (loop [ map {} kw-list kw-list]
     (if-let [[kw & tail] kw-list]
       (recur (assoc map (keyword (str prefix kw)) kw) tail)
       map)))

(defn rect
  [ prefix map ]
  (let [kw     (fold* prefix :top :left :width :height)
        args   (reduce #(assoc %1 (kw %2) (map %2)) {} (keys kw))]
    args))
