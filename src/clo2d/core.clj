(ns clo2d.core
  (:import (java.awt.image BufferedImage)
           (java.awt.geom Line2D$Double)
           (java.awt.geom Rectangle2D$Double)
           (javax.imageio ImageIO)
           (java.io File)))

;;
;; Working with BufferedImage
;;
(defn buffered-image 
  "Create a new buffered image"
  [ width height mode ]
  (BufferedImage. width height (. BufferedImage TYPE_INT_ARGB)))

(defn pixels
  "Returns the image pixels as an array.
  Probably mostly useful for testing purposes"
  [ img ]
  (.. img (getRaster) (getDataBuffer) (getData)))

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

;;
;; Working with colors
;;
(defn color
  "Create a new color object"
  [ r g b a ]
  (if (instance? java.lang.Double r)
      (apply color (map float [r g b a]))
      (java.awt.Color. r g b a)))

(defmacro rgb-components
  "Returns the RGB+alpha components of a color is the default sRGB
  color space."
  [ color ]
  `(seq (.getRGBComponents ~color nil)))

;;
;; Graphic primitives
;;
(defn draw 
  "Draw a shape in the current context"
  [ ^java.awt.Shape shape ]
  (.draw *g2d* shape))

(defn background
  "Set the background color and erase the entire image"
  [ ^java.awt.Color color ]
  (doto *g2d* 
      (.setBackground color) 
      (.clearRect 0 0 (Integer/MAX_VALUE) (Integer/MAX_VALUE) )))

(defn line
  "Draw a line from (x1,y1) to (x2,y2).
  Coordinates are expressed as double."
  [ x1 y1 x2 y2 ]
  (draw (Line2D$Double. x1 y1 x2 y2)))

(defn rectangle
  "Draw a rectangle of width `w` and height `h` at (x,y)"
  [ x y w h ]
  (draw (Rectangle2D$Double. x y w h)))


