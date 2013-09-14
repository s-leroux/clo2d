(ns clo2d.core
  (:import (java.awt.image BufferedImage)
           (java.awt.geom AffineTransform)
           (java.awt Font)
           (java.awt Shape)
           (java.awt Graphics2D)
           (java.awt.geom Line2D$Double)
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
;; Text
;;
(defn set-font
  "Set the current font given its name"
  [ font-name ]
  (if-let [ ^Font font (Font/decode font-name )]
    (.setFont (:ctx *g2d*) font)))

(def
  text-modes
  {
    :left   (fn [ ctx str x y ] [x y])
    :right (fn [ ^Graphics2D ctx ^String str x y ] 
      (let [ frc (.getFontRenderContext ctx)
             font (.getFont ctx)
             bounds (.getStringBounds font str frc)
             width (.getWidth bounds)]
        [ (- x (+ 1 width) ) y ])
    )
    :center (fn [ ^Graphics2D ctx ^String str x y ] 
      (let [ frc (.getFontRenderContext ctx)
             font (.getFont ctx)
             bounds (.getStringBounds font str frc)
             width (.getWidth bounds)]
        [ (- x (/ (+ 1 width) 2)) y ])
    )

  }
)

(defn text-align
 "Set the text alignment"
 [ mode ]
 (set! *g2d* (assoc *g2d* :tm (text-modes mode))))

(defn draw-string
  "Draw the specified string at the given position"
  [ ^String str bx by ]
  ( let [ ctx (:ctx *g2d*)
          tm  (:tm *g2d*)
          [x y] (tm ctx str bx by) ]
    (.setColor (:ctx *g2d*) (:sc *g2d*))
    (.drawString ctx str (float x) (float y) )))

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

