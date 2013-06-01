(ns masonry.core-test
  (:require [clojure.test :refer :all]
            [masonry.core :refer :all]))

(def some-rects
  [ {:x 1 :y 3 :width 2 :height 4}
  , {:x 2 :y 5 :width 1 :height 1}
  , {:x 3 :y 7 :width 1 :height 5}
  ])

(def some-photos
  [ {:id 1 :width 1 :height 1}
  , {:id 2 :width 2 :height 1}
  , {:id 3 :width 1 :height 5}
  ])

(def rects-for-some-photos
  [ {:id 1 :x 0 :y 0 :width 1 :height 1}
  , {:id 2 :x 0 :y 1 :width 2 :height 1}
  , {:id 3 :x 0 :y 2 :width 1 :height 5}])

(deftest a-test
  (testing "FIXME, I fail."
    (is (= (count some-photos) (count (layout 200 some-photos))))))

(deftest contains-column-test
  (testing "d"
    (is (not (contains-column {:x 3 :width 2} 1)))
    (is (not (contains-column {:x 3 :width 2} 2)))
    (is (contains-column {:x 3 :width 2} 3))
    (is (contains-column {:x 3 :width 2} 4))
    (is (not (contains-column {:x 3 :width 2} 5)))
    (is (not (contains-column {:x 3 :width 2} 6)))
    (is (not (contains-column {:x 3 :width 2} 7)))
    (is (= (map #(contains-column % 2) some-rects) '(true true false)))))

(deftest bottom-test
  (testing "d"
    (is (= 8 (bottom {:y 3 :height 5})))))

(deftest find-bottom-in-column-test
  (testing "d"
    (is (= 7 (find-bottom-in-column 2 some-rects)))))

(deftest lower-edge-shape-test
  (testing "d"
    (is (= '(0 7 7 12) (lower-edge-shape 4 some-rects)))))

(deftest free-y-at-column-for-width-test
  (testing "d"
    (is (and
          (= (free-y-at-column-for-width 0 2 [0 7 7]) 7)
          (= (free-y-at-column-for-width 0 1 [0 7 7]) 0)))))

(deftest free-points-test
  (testing "d"
    (is (= [[0 7] [1 7] [2 12]] (free-points 2 [0 7 7 12])))
    (is (= [[0 1] [1 1] [2 1] [3 1]] (free-points 2 [0 1 0 1 0])))
    (is (= [[0 0] [1 7] [2 7] [3 12]] (free-points 1 [0 7 7 12])))))

(deftest layout-iteration-test
  (testing "d"
    (is (= [{:x 0 :y 0 :width 2 :height 42}])
           (layout-iteration 2 [] {:width 2 :height 42}))
    (is (= [{:x 0 :y 0 :width 2 :height 42}, {:x 2 :y 0 :width 2 :height 42}])
           (layout-iteration 4 [{:x 0 :y 0 :width 2 :height 42}] {:width 2 :height 42}))
    (is (= [{:x 0 :y 0 :width 2 :height 42}, {:x 0 :y 42 :width 2 :height 42}])
           (layout-iteration 2 [{:x 0 :y 0 :width 2 :height 42}] {:width 2 :height 42}))
    (is (= (conj some-rects {:x 0 :y 7 :width 2 :height 42})
           (layout-iteration 4 some-rects {:width 2 :height 42})))
    (is (= (conj some-rects {:x 0 :y 0 :width 1 :height 42})
           (layout-iteration 4 some-rects {:width 1 :height 42})))))

(deftest layout-test
  (testing "d"
    (is (= rects-for-some-photos
           (layout 2 some-photos)))))
