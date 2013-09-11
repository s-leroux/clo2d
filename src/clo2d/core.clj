(ns clo2d.core
  (:import (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.io File)))

(defn buffered-image 
  "Create a new buffered image"
  [ width height mode ]
  (BufferedImage. width height (. BufferedImage TYPE_INT_ARGB)))

;;
;; Working with graphic files
;;

(defn file-name-extension [ fname ]
  (let [sep (.lastIndexOf fname ".")]
    (if (>= sep 0)
        (.substring fname (inc sep))
        "")))

(defn save 
  "Save an image to a file. The file format is assumed from 
  the file name extension"
  [ img fname ]
  (let [ext (file-name-extension fname)
        file (File. fname)]
    (. ImageIO write img ext file)
    
  ))

;;
;; Working with graphic contexts (Graphics2D)
;;

(def ^:dynamic *g2d*)

(defmacro with-2d-context 
  "Push a new 2D graphic context"
  [ img & body ]
  `(binding [*g2d* (.createGraphics ~img)] ~@body))
