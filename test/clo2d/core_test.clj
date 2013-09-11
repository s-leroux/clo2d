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
        (is (instance? java.awt.Graphics2D *g2d*))
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
  (testing "Color creation"
    (let [r    1.000
          g    0.500
          b    0.250
          a    0.125
          col (color r g b a)]
      (is (close-to (rgb-components col) [ r g b a ] 0.01)))))

(defmacro is-image [ width height pixels name & body ]
  `(testing ~name
     (let [~'img (buffered-image ~width ~height :rgb)
           ~'W -1
           ~'R -65536
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
  (is-image 3 3 [W T T
                 T W T
                 T T W]
    "line"
    (line 0 0 2 2))
  (is-image 3 3 [W W W
                 W T W
                 W W W]
    "rectangle"
    (rectangle 0 0 2 2)))

