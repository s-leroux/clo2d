(ns clo2d.adv-test
  (:require [clojure.test :refer :all]
            [clo2d.core :refer :all]
            [clo2d.shape :refer :all])
  (:import (java.io File)))

(deftest constrained-test
  (testing "Constrained square"
    (let [g (group (shape :r square [:width = 50])
                   (shape :s square [:width = 20])
                   (having [(nw :r) = (nw :s)]
                           [(nw :r) = [20 10]]))
          width 100
          height 100
          img (buffered-image width height :rgb)]
      (with-2d-context img
        (display :stroke (color :red) 
                 :fill (color :white)
                 (with g rectangle (bounds :r))
                 (with g rectangle (bounds :s))))
      (save img ".test-out/constrained.png"))))

