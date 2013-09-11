(ns clo2d.core
  (:import (java.awt.image BufferedImage)))

(defn buffered-image 
  "Create a new buffered image"
  [ width height mode ]
  (BufferedImage. width height (. BufferedImage TYPE_INT_ARGB)))

