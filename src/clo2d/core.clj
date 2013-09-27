(ns clo2d.core
  (:use clo2d.linear clo2d.infix)
  (:import (java.awt.image BufferedImage)
           (java.awt.geom AffineTransform)
           (java.awt Font)
           (java.awt.font TextLayout)
           (java.awt Shape)
           (java.awt Graphics2D)
           (java.awt.geom Area)
           (java.awt.geom Line2D$Double)
           (java.awt.geom Ellipse2D$Double)
           (java.awt.geom Rectangle2D$Double)
           (java.awt.geom Path2D$Double)
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
;; Working with colors
;;
(defn color
  "Create a new color object"
  ( [ r g b a ]
    (if (instance? java.lang.Double r)
        (apply color (map float [r g b a]))
        (java.awt.Color. r g b a)))
  ( [ name ]
    (case name
      :black       ( java.awt.Color/BLACK )
      :white       ( java.awt.Color/WHITE )
      :red         ( java.awt.Color/RED )
      :green       ( java.awt.Color/GREEN )
      :blue        ( java.awt.Color/BLUE )
      :transparent ( color 0.0 0.0 0.0 0.0 )
    )))

(defmacro rgb-components
  "Returns the RGB+alpha components of a color is the default sRGB
  color space."
  [ color ]
  `(seq (.getRGBComponents ~color nil)))

;;
;; Working with graphic contexts (Graphics2D)
;;

(def ^:dynamic *g2d*)

(defmacro with-2d-context 
  "Push a new 2D graphic context"
  [ img & body ]
  `(binding [*g2d* { :ctx (.createGraphics ~img) 
                     :sc (color :white)
                     :fc (color :transparent) 
                     :tm (text-modes :left) } ] ~@body))

(defn set-stroke
  "Set the stroke drawing attribute"
  [ c ]
  (set! *g2d* (assoc *g2d* :sc (if (keyword? c) (color c) c))))

(defn set-fill
  "Set the fill drawing attribute"
  [ c ]
  (set! *g2d* (assoc *g2d* :fc (if (keyword? c) (color c) c))))

;;
;; Transformations
;;
(defmacro with-transformation
  "Push the current transformation matrix and
  execute the body, finally restore it on exit."
  [ & body ]
  `(let [ ^AffineTransform matrix# (.getTransform (:ctx *g2d*))]
     (try ~@body
     (finally
        (.setTransform (:ctx *g2d*) matrix#)))))

(defn rotate
  "Apply a rotation transform."
  [ theta ]
  (.rotate (:ctx *g2d*) theta ))

(defn translate
  "Apply a translation transform."
  [ dx dy ]
  (.translate (:ctx *g2d*) dx dy ))

;;
;; Clipping
;;
(defmacro with-clip
  "Push the current clipping region and and the given shape
  to it. Then execute the body, finally restore the clipping
  region on exit."
  [ ^Shape shape & body ]
  `(let [ ^Shape clip# (.getClip (:ctx *g2d*))]
     (try 
       (clip ~shape)
       ~@body
     (finally
        (.setClip (:ctx *g2d*) clip#)))))

(defn clip
  "Add a shape to the current clipping region"
  [ ^Shape s ]
  (.clip (:ctx *g2d*) s))

;;
;; Graphic primitives
;;
(defn stroke 
  "Draw the outline of a shape in the current context"
  [ ^java.awt.Shape shape ]
  (.setColor (:ctx *g2d*) (:sc *g2d*))
  (.draw (:ctx *g2d*) shape))

(defn fill 
  "Fill a shape in the current context"
  [ ^java.awt.Shape shape ]
  (.setColor (:ctx *g2d*) (:fc *g2d*))
  (.fill (:ctx *g2d*) shape))

(def display-fn {
 :shape
 (fn [ ^java.awt.Shape shape & tail ]
   (fill shape)
   (stroke shape)
   tail)

 :stroke
 (fn [ ^java.awt.Color color & tail ]
   (set-stroke color)
   tail)

 :fill
 (fn [ ^java.awt.Color color & tail ]
   (set-fill color)
   tail)
})

(defn display
  "Draw a shape in the current context"
  [ & args ]
  (loop [shapes args]
    (if-let [item (first shapes)]
      (recur
        (cond
          (keyword? item)
          (apply (item display-fn) (rest shapes))

          (instance? java.awt.Shape item)
          (apply (:shape display-fn) item (rest shapes)))))))

(defn background
  "Set the background color and erase the entire image"
  [ ^java.awt.Color color ]
  (doto (:ctx *g2d*) 
      (.setBackground color) 
      (.clearRect 0 0 (Integer/MAX_VALUE) (Integer/MAX_VALUE) )))

(defn line
  "Create a line object from (x1,y1) to (x2,y2).
  Coordinates are expressed as double."
  [ x1 y1 x2 y2 ]
  (Line2D$Double. x1 y1 x2 y2))

(defn rectangle
  "Create a rectangle of width `w` and height `h` at (x,y)"
  [ x y w h ]
  (Rectangle2D$Double. x y w h))

(defn ellipse
  "Create an ellipse of width `w` and height `h` at (x,y)"
  [ x y w h ]
  (Ellipse2D$Double. x y w h))

(defn csg*
  "Combine one shape/area with all those of a given list"
  [ ^clojure.lang.IFn f 
    ^Shape s1 
    args ]
  (let [ ^Area a1 (if (instance? Area s1) s1 (Area. s1))]
    (loop [ ^Shape s2 (first args)
            shapes (rest args) ]
      (when s2
        (let [ ^Area a2 (if (instance? Area s2) s2 (Area. s2)) ]
          (f a1 a2)
          (recur (first shapes) (rest shapes)))))
    a1))

(defn union
  "Combine multiple shapes and return their union"
  [ ^Shape s1 & args]
  (csg* (fn [^Area a1 ^Area a2] (.add a1 a2)) s1 args))

(defn intersect
  "Combine multiple shapes and return their intersection"
  [ ^Shape s1 & args]
  (csg* (fn [^Area a1 ^Area a2] (.intersect a1 a2)) s1 args))

(defn diff
  "Combine multiple shapes and return their difference"
  [ ^Shape s1 & args]
  (csg* (fn [^Area a1 ^Area a2] (.subtract a1 a2)) s1 args))

(defn xor
  "Combine multiple shapes by performing an exclusive or
  operation"
  [ ^Shape s1 & args]
  (csg* (fn [^Area a1 ^Area a2] (.exclusiveOr a1 a2)) s1 args))

(defn path
  "Create a path using SVG-like specifications"
  [ & args ]
  (let [p (Path2D$Double.)]
    (loop [specs args kw :M ]
      (if-let [f (first specs)]
        (let [cmd (if (keyword? f) f kw)
              data (if (keyword? f) (rest specs) specs)]
          (case cmd
            :M ; absolute moveTo
            (let [[x y & tail] data]
              (.moveTo p x y)
              (recur tail :L))

            :L ; absolute lineTo
            (let [[x y & tail] data]
              (.lineTo p x y)
              (recur tail :L))

            :C ; absolute curveTo (cubic Bezier curve)
            (let [[x1 y1 x2 y2 x y & tail] data]
              (.curveTo p x1 y1 x2 y2 x y)
              (recur tail :C))
          )
        )))
    p))

;;
;; Text
;;
(defn set-font
  "Set the current font given its name"
  ( [ font-name ]
    (if-let [ ^Font font (Font/decode font-name )]
      (.setFont (:ctx *g2d*) font)))
  ( [ font-name font-size ]
    (if-let [ ^Font font (Font. font-name Font/PLAIN font-size)]
      (.setFont (:ctx *g2d*) font)))
)

(def
  text-modes
  {
    :left   (fn [ layout x y ] [x y])
    :right (fn [ ^TextLayout layout x y ] 
      (let [ width (.getAdvance layout) ]
        [ (- x (+ 1 width) ) y ])
    )
    :center (fn [ ^TextLayout layout x y ] 
      (let [ width (.getAdvance layout) ]
        [ (- x (/ (+ 1 width) 2)) y ])
    )

  }
)

(defn text-align
 "Set the text alignment"
 [ mode ]
 (set! *g2d* (assoc *g2d* :tm (text-modes mode))))

(defn string
  "Create a shape representing the string at the given position.
  This will not produce quality rendering at small font sizes."
  [ ^String str bx by ]
  (let [ ^Graphics2D ctx (:ctx *g2d*)
         frc (.getFontRenderContext ctx)
         font (.getFont ctx)
         layout (TextLayout. str font frc)
         tm (:tm *g2d*)
         [x y] (tm layout bx by)
         shape (.getOutline layout (AffineTransform/getTranslateInstance x y)) ]
    shape))

(defn draw-string
  "Draw the specified string at the given position"
  [ ^String str bx by ]
  (let [ ^Graphics2D ctx (:ctx *g2d*)
         frc (.getFontRenderContext ctx)
         font (.getFont ctx)
         layout (TextLayout. str font frc)
         tm (:tm *g2d*)
         [x y] (tm layout bx by)]
    (.setColor ctx (:fc *g2d*))
    (.draw layout ctx x y)))

