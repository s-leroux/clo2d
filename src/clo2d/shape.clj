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

(defmacro unfold
  [ k ]
  `(let [k# (str ~k)] (keyword (.substring k# (inc (.lastIndexOf k# ":"))))))

(defn rect
  [ prefix map ]
  (let [top    (find-keyword (str prefix ":top"))
        left   (find-keyword (str prefix ":left"))
        width  (find-keyword (str prefix ":width"))
        height (find-keyword (str prefix ":height"))
        args   (reduce #(assoc %1 (unfold %2) (map %2)) {} [top left width height])]
    args))