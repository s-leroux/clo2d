(ns clo2d.adv-test
  (:require [clojure.test :refer :all]
            [clo2d.core :refer :all]
            [clo2d.shape :refer :all])
  (:import (java.io File)))

(deftest constrained-test
  (testing "Constrained square"
    (let [g (group-> (shape :r square [:width = 50])
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
      (save img ".test-out/constrained.png")))

  (testing "Intersection"
    (let [g (group->
               (shape :p1 point [[:x :y] = [5 5]])
               (shape :p2 point [[:x :y] = [50 50]])
               (shape :p3 point [[:x :y] = [50 5]])
               (shape :p4 point [[:x :y] = [20 60]])
               (shape :dot circular [:radius = 2])
               (having
                  [(center :dot) = :w0 '** [(center :p1) (center :p2)]]
                  [(center :dot) = :w1 '** [(center :p3) (center :p4)]]
            ))
          width 100
          height 100
          img (buffered-image width height :rgb)]
      (with-2d-context img
        (display :stroke (color :red) 
                 :fill (color :white)
                 (with g line :p1 :p2)
                 (with g line :p3 :p4)
                 (with g ellipse (bounds :dot))))
      (save img ".test-out/constrained isect.png")))


)

