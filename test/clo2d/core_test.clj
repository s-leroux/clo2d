(ns clo2d.core-test
  (:require [clojure.test :refer :all]
            [clo2d.core :refer :all]
            [clojure.java.io :refer :all])
  (:import (java.io File)))

(deftest create-buffered-image-test
  (testing "Buffered image creation"
    (let [width 100
          height 50
          img (buffered-image width height :rgb)]
      (is (instance? java.awt.image.BufferedImage img))
      (is (= (seq (.getComponentSize (.getColorModel img))) [8 8 8 8]))
      (is (= (.getWidth img) width))
      (is (= (.getHeight img) height)))))

(deftest image-file-subsystem-test
  (testing "File format extraction"
    (is (= (file-name-extension "img.png") "png"))
    (is (= (file-name-extension "img.xxx.png") "png"))
    (is (= (file-name-extension "img.jpg") "jpg"))
    (is (= (file-name-extension "img") "")))

  (testing "PNG file image creation"
    (let [fname ".test-out/test.png"
          img (buffered-image 50 50 :rgb)]
      (delete-file fname true)
      (save img fname)
      (let [file (File. fname)]
        (is (.isFile file))))))


(deftest g2d-context-creation-test
  (testing "Graphics2D context creation"
    (let [old-g2d *g2d*]
      (with-2d-context (buffered-image 50 50 :rgb)
        (is (not= old-g2d *g2d*))
        (is (instance? java.awt.Graphics2D (:ctx *g2d*)))
      )
      (is (= *g2d* old-g2d)))))

(defn close-to 
  (
  [ a b delta1 delta2]
  (if (seq? a)
      (or  (and (empty? a) (empty? b))
           (and (close-to (first a) (first b) delta1 delta2)
                (close-to (rest a) (rest b) delta1 delta2)))
      (< (- b delta1) a (+ b delta2))))
  (
  [ a b delta ]
  (close-to a b delta delta))
  )
    

(deftest color-test
  (testing "RGB Color creation"
    (let [r    1.000
          g    0.500
          b    0.250
          a    0.125
          col (color r g b a)]
      (is (close-to (rgb-components col) [ r g b a ] 0.01))))

  (testing "Color by keyword"
    (let [col (color :red)]
      (is (close-to (rgb-components col) [ 1.0 0.0 0.0 1.0 ] 0.01)))
  )
)

(defmacro is-image [ width height pixels name & body ]
  `(testing ~name
     (let [~'img (buffered-image ~width ~height :rgb)
           ~'W -1
           ~'K -16777216
           ~'R -65536
           ~'G -16711936
           ~'B -16776961
           ~'T  0]
       (with-2d-context ~'img
         ~@body
  
         (save ~'img (str ".test-out/" ~name "-" ~width "x" ~height ".png"))
         (is (=(seq (pixels ~'img)) ~pixels))))))


(deftest background-test
  (is-image 3 3 [R R R
                 R R R
                 R R R]
    "background"
    (background (color 1.0 0.0 0.0 1.0))))

(deftest shape-drawing-test
  (is-image 5 5 [T T T T T
                 T W T T T
                 T T W T T
                 T T T W T
                 T T T T T]
    "line"
    (display (line 1 1 3 3)))

  (is-image 5 5 [T T T T T
                 T W W W T
                 T W T W T
                 T W W W T
                 T T T T T]
    "rectangle"
    (display (rectangle 1 1 2 2)))


  (is-image 5 5 [T T T T T
                 T W W W T
                 T K K W T
                 T W W W T
                 T T T T T]
    "path"
    (display :fill (color :black) (path 1 1 3 1 3 3 1 3)))

  (is-image 5 5 [T T T T T
                 T W T W T
                 T W T W T
                 T W T W T
                 T T T T T]
    "path-open"
    (display :fill (color :black) (path :M 1 1 1 3 :M 3 1 :L 3 3)))

  (is-image 5 5 [R R T T T
                 K K R T T
                 K K R T T
                 K K R T T
                 R R T T T]
    "path-curveTo"
    (display :stroke (color :red)
             :fill (color :black)
             (path :M 0 0 :L 0 0 :C 3 1 3 4 0 4)))

  (is-image 14 14 [T T T T T T T T T T T T T T
                   T T T T K K K K K K T T T T
                   T T K K K K K K K K K K T T
                   T T K K K K K K K K K K T T
                   T K K K K K K K K K K K K T
                   T K K K K K K K K K K K K T
                   T K K K K K K K K K K K K T
                   T K K K K K K K K K K K K T
                   T K K K K K K K K K K K K T
                   T K K K K K K K K K K K K T
                   T T K K K K K K K K K K T T
                   T T K K K K K K K K K K T T
                   T T T T K K K K K K T T T T
                   T T T T T T T T T T T T T T]
    "ellipse"
    (display :stroke :transparent
             :fill (color :black)
             (ellipse 0 0 13 13)))
)


(deftest path-manipulation-test
  (is-image 8 8 [T T T T T T T T
                 T B B B B B T T
                 T B K K K B B T
                 T B K K K K B T
                 T B K K K K B T
                 T B B K K K B T
                 T T B B B B B T
                 T T T T T T T T]
    "path-union"
    (let [r1 (rectangle 1 1 4 4)
          r2 (rectangle 2 2 4 4)
          path  (union r1 r2)]
      (display :stroke :blue 
               :fill :black
               path)))

  (is-image 8 8 [B B B B B T T T
                 B K K K B B T T
                 B K K K K B B T
                 B K K K K K B B
                 B B K K K K K B
                 T B B K K K K B
                 T T B B K K K B
                 T T T B B B B B]
    "path-union-n"
    (let [r1 (rectangle 1 1 4 4)
          r2 (rectangle 2 2 4 4)
          r3 (rectangle 0 0 4 4)
          r4 (rectangle 3 3 4 4)
          path  (union r1 r2 r3 r4)]
      (display :stroke :blue 
               :fill :black
               path)))

  (is-image 8 8 [T T T T T T T T
                 T T T T T T T T
                 T T B B B B T T
                 T T B K K B T T
                 T T B K K B T T
                 T T B B B B T T
                 T T T T T T T T
                 T T T T T T T T]
    "path-intersection"
    (let [r1 (rectangle 1 1 4 4)
          r2 (rectangle 2 2 4 4)
          path  (intersect r1 r2)]
      (display :stroke :blue 
               :fill :black
               path)))

  (is-image 8 8 [T T T T T T T T
                 T T T T T T T T
                 T T B B B T T T
                 T T B K B T T T
                 T T B B B T T T
                 T T T T T T T T
                 T T T T T T T T
                 T T T T T T T T]
    "path-intersection-n"
    (let [r1 (rectangle 1 1 4 4)
          r2 (rectangle 2 2 4 4)
          r3 (rectangle 0 0 4 4)
          path  (intersect r1 r2 r3)]
      (display :stroke :blue 
               :fill :black
               path)))

  (is-image 8 8 [T T T T T T T T
                 T B B B B B T T
                 T B K K K B T T
                 T B K B B B T T
                 T B K B T T T T
                 T B B B T T T T
                 T T T T T T T T
                 T T T T T T T T]
    "path-diff"
    (let [r1 (rectangle 1 1 4 4)
          r2 (rectangle 3 3 4 4)
          path  (diff r1 r2)]
      (display :stroke :blue 
               :fill :black
               path)))

  (is-image 8 8 [T T T T T T T T
                 T B B B B B T T
                 T B K K K B T T
                 T B K B B B B B
                 T B K B T B K B
                 T B B B B B K B
                 T T T B K K K B
                 T T T B B B B B]
    "path-xor"
    (let [r1 (rectangle 1 1 4 4)
          r2 (rectangle 3 3 4 4)
          path  (xor r1 r2)]
      (display :stroke :blue 
               :fill :black
               path)))

  (is-image 14 14 [T T T T T B B B B T T T T T
                   T T T B B K K K K B B T T T
                   T T B K K K K K K K K B T T
                   T B K K K B B B B K K K B T
                   T B K K B T T T T B K K B T
                   B K K B T T T T T T B K K B
                   B K K B T T T T T T B K K B
                   B K K B T T T T T T B K K B
                   B K K B T T T T T T B K K B
                   T B K K B T T T T B K K B T
                   T B K K K B B B B K K K B T
                   T T B K K K K K K K K B T T
                   T T T B B K K K K B B T T T
                   T T T T T B B B B T T T T T]
    "ellipse-hole"
    (let [e1 (ellipse 0 0 13 13)
          e2 (ellipse 3 3  7  7)
          path  (diff e1 e2)]
      (display :stroke :blue 
               :fill :black
               path)))
)

(deftest text-drawing-test
  (is-image 10 11 [T T T T T T T T T T
                   T K K K T T K K K T
                   T T K T T T T K T T
                   T T T K T T K T T T
                   T T T T K K T T T T
                   T T T T K K T T T T
                   T T T T K K T T T T
                   T T T K T T K T T T
                   T T K T T T T K T T
                   T K K K T T K K K T
                   T T T T T T T T T T]
    "text"
    (set-font java.awt.Font/SERIF)
    (set-fill (color :black))
    (draw-string "X" 0 10))

  (is-image 10 11 [T T T T T T T T T T
                   T K K K T K K K T T
                   T T K T T T K T T T
                   T K T T T T T K T T
                   K T T T T T T T K K
                   K T T T T T T T K K
                   K T T T T T T T K K
                   T K T T T T T K T T
                   T T K T T T K T T T
                   T K K K T K K K T T
                   T T T T T T T T T T]
    "text-align-center"
    (set-font java.awt.Font/SERIF)
    (set-fill (color :black))
    (text-align :center)
    (draw-string "XXX" 0 10))

  (is-image 10 11 [T T T T T T T T T T
                   T K K K T T K K K T
                   T T K T T T T K T T
                   T T T K T T K T T T
                   T T T T K K T T T T
                   T T T T K K T T T T
                   T T T T K K T T T T
                   T T T K T T K T T T
                   T T K T T T T K T T
                   T K K K T T K K K T
                   T T T T T T T T T T]
    "text-align-center-placement"
    (set-font java.awt.Font/SERIF)
    (set-fill (color :black))
    (text-align :center)
    (draw-string "XX" 0 10))

  (is-image 10 11 [T T T T T T T T T T
                   T K K K T T K K K T
                   T T K T T T T K T T
                   T T T K T T K T T T
                   T T T T K K T T T T
                   T T T T K K T T T T
                   T T T T K K T T T T
                   T T T K T T K T T T
                   T T K T T T T K T T
                   T K K K T T K K K T
                   T T T T T T T T T T]
    "text-align-right"
    (set-font java.awt.Font/SERIF)
    (set-fill (color :black))
    (text-align :right)
    (draw-string "WX" 10 10))

  (is-image 30 30 [
           T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T 
           T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T 
           T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T 
           T T K K K K K K K K K K K T T T T T K K K K K K K K T T T T 
           T T K K K K K K K K K K K T T T T T K K K K K K K K T T T T 
           T T T T T K K K K K T T T T T T T T T T T K K T T T T T T T 
           T T T T T T K K K K K T T T T T T T T T K K T T T T T T T T 
           T T T T T T T K K K K T T T T T T T T K K K T T T T T T T T 
           T T T T T T T K K K K K T T T T T T T K K T T T T T T T T T 
           T T T T T T T T K K K K K T T T T T K K T T T T T T T T T T 
           T T T T T T T T T K K K K T T T T K K K T T T T T T T T T T 
           T T T T T T T T T K K K K K T T T K K T T T T T T T T T T T 
           T T T T T T T T T T K K K K K T K K T T T T T T T T T T T T 
           T T T T T T T T T T T K K K K K K K T T T T T T T T T T T T 
           T T T T T T T T T T T K K K K K K T T T T T T T T T T T T T 
           T T T T T T T T T T T T K K K K K T T T T T T T T T T T T T 
           T T T T T T T T T T T T T K K K K T T T T T T T T T T T T T 
           T T T T T T T T T T T T K K K K K K T T T T T T T T T T T T 
           T T T T T T T T T T T T K K K K K K K T T T T T T T T T T T 
           T T T T T T T T T T T K K T T K K K K T T T T T T T T T T T 
           T T T T T T T T T T K K T T T K K K K K T T T T T T T T T T 
           T T T T T T T T T K K K T T T T K K K K K T T T T T T T T T 
           T T T T T T T T T K K T T T T T T K K K K T T T T T T T T T 
           T T T T T T T T K K T T T T T T T K K K K K T T T T T T T T 
           T T T T T T T K K K T T T T T T T T K K K K K T T T T T T T 
           T T T T T T T K K T T T T T T T T T T K K K K T T T T T T T 
           T T T T T T K K T T T T T T T T T T T K K K K K T T T T T T 
           T T T T T K K K T T T T T T T T T T T T K K K K K T T T T T 
           T T K K K K K K K K K T T T T T T K K K K K K K K K K T T T 
           T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T
           ]
    "text-glyph"
    (set-font java.awt.Font/SERIF 36)
    (text-align :center)
    (display :stroke :transparent
             :fill :black
             (string "X" 15 29)))
)

(deftest draw-mini-language-test
  (is-image 5 5 [T T T T T
                 T W T T T
                 T T W T T
                 T T T W T
                 T T T T T]
    "shape-keyword"
    (display :shape (line 1 1 3 3)))

  (is-image 5 5 [W W W W W
                 W R B G W
                 W B G B W
                 W G B R W
                 W W W W W]
    "color-keywords"
    (display :fill (color :blue) (rectangle 0 0 4 4)
             :stroke (color :red) (line 1 1 3 3)
             :stroke (color :green) (line 1 3 3 1)))

  (is-image 5 5 [W W W W W
                 W G G G W
                 W W W W W
                 W G G G W
                 W W W W W]
    "rect+line"
    (set-fill (color :green))
    (display (rectangle 0 0 4 4)
             (line 0 2 4 2))))

(deftest transformation-test
  (is-image 5 5 [W W W T T
                 T T T T T
                 T T T T T
                 T T T T T
                 T T T T T]
    "rotate"
    (rotate (/ Math/PI -2.0))
    (display :shape (line 0 0 0 2)))

  (is-image 5 5 [T T T T T
                 T T W T T
                 T T W T T
                 T T W T T
                 T T T T T]
    "translate"
    (translate 2 1)
    (display :shape (line 0 0 0 2)))
)

(deftest transformation-context-test
  (is-image 5 5 [W W W T T
                 W T T T T
                 W T T T T
                 T T T T T
                 T T T T T]
    "transformation"
    (with-transformation 
      (rotate (/ Math/PI -2.0))
      (display :shape (line 0 0 0 2)))

    (display :shape (line 0 0 0 2)))
)

(deftest clipping-test
  (is-image 5 5 [T T T T T
                 T W W W T
                 T W W W T
                 T W W W T
                 T T T T T]
    "clipping"
    (clip (rectangle 1 1 3 3))
    (display :stroke (color :transparent) 
             :fill (color :white)
             (rectangle 0 0 5 5)))

  (is-image 5 5 [T T T T T
                 T T T T T
                 T T W T T
                 T T T T T
                 T T T T T]
    "clip-context"
    (clip (rectangle 0 0 3 3))
    (let [old-clip (.getClip (:ctx *g2d*))]
      (with-clip (rectangle 2 2 3 3)
        (display :stroke (color :transparent) 
                 :fill (color :white)
                 (rectangle 0 0 5 5)))
      (is (= (.getClip (:ctx *g2d*)) old-clip))))
)


(deftest pen-color-test
  (is-image 3 3 [R R R
                 R G R
                 R R R]
    "pen-color"
    (set-stroke (color :red))
    (set-fill (color :green))

    (display (rectangle 0 0 2 2)))

  (is-image 3 3 [R R R
                 R G R
                 R R R]
    "pen-color-by-keyword"
    (set-stroke :red)
    (set-fill :green)

    (display (rectangle 0 0 2 2)))
    
)
    

