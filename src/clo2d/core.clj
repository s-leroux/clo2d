(ns clo2d.core
  (:import (java.awt.image BufferedImage)))

(defn buffered-image 
  "Create a new buffered image"
  [ width height mode ]
  (BufferedImage. width height (. BufferedImage TYPE_INT_ARGB)))

;;
;; Working with graphic contexts (Graphics2D)
;;

(def ^:dynamic *g2d*)

(defmacro with-2d-context 
  "Push a new 2D graphic context"
  [ img & body ]
  `(binding [*g2d* (.createGraphics ~img)] ~@body))
