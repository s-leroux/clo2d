(ns clo2d.core-test
  (:require [clojure.test :refer :all]
            [clo2d.core :refer :all]))

(deftest create-buffered-image-test
  (testing "Buffered image creation"
    (let [width 100
          height 50
          img (buffered-image width height :rgb)]
      (is (instance? java.awt.image.BufferedImage img))
      (is (= (seq (.getComponentSize (.getColorModel img))) [8 8 8 8]))
      (is (= (.getWidth img) width))
      (is (= (.getHeight img) height)))))

(deftest g2d-context-creation-test
  (testing "Graphics2D context creation"
    (let [old-g2d *g2d*]
      (with-2d-context (buffered-image 50 50 :rgb)
        (is (not= old-g2d *g2d*))
        (is (instance? java.awt.Graphics2D *g2d*))
      )
      (is (= *g2d* old-g2d)))))
