(ns masonry.core-test
  (:require [clojure.test :refer :all]
            [masonry.core :refer :all]))

(def some-rects
  [ {:x 1 :y 3 :width 2 :height 4}
  , {:x 2 :y 5 :width 1 :height 1}
  , {:x 3 :y 7 :width 1 :height 5}
  ])

(def layouted-some-rects
  [ {:x 0 :y 3 :width 2 :height 4}
  , {:x 2 :y 5 :width 1 :height 1}
  , {:x 3 :y 7 :width 1 :height 5}
  ])

(def some-photos
  [ {:id 1 :width 1 :height 1}
  , {:id 2 :width 2 :height 1}
  , {:id 3 :width 1 :height 5}
  ])

(def rects-for-some-photos
  [ {:id 1 :x 0 :y 0 :width 2 :height 1}
  , {:id 2 :x 0 :y 1 :width 2 :height 1}
  , {:id 3 :x 0 :y 2 :width 1 :height 5}])

(deftest a-test
  (testing "FIXME, I fail."
    (is (= (count some-photos) (count (layout 2 some-photos))))))

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
    (is (= '(0 7 7 12) (lower-edge-shape 4 some-rects)))
    (is (= '(7 7 6 12) (lower-edge-shape 4 layouted-some-rects)))))

(deftest free-y-at-column-for-width-test
  (testing "d"
    (is (and
          (= (free-y-at-column-for-width 0 2 [0 7 7]) 7)
          (= (free-y-at-column-for-width 0 1 [0 7 7]) 0)))))

(deftest eliminate-gap-test
  (testing "d"
    (is (= (set (eliminate-gap 2 {:x 1 :y 1 :height 1} [{:x 0 :y 0 :width 1 :height 2} {:x 1 :y 0 :width 1 :height 1}]
                          {:x 0 :y 2 :width 2 :height 1}))
           #{{:x 0 :y 0 :width 1 :height 2} {:x 1 :y 0 :width 1 :height 2} {:x 0 :y 2 :width 2 :height 1}}))
    (is (= (set (eliminate-gap 2 {:x 1 :y 0 :height 1} [{:x 0 :y 0 :width 1 :height 1}]
                          {:x 0 :y 1 :width 2 :height 1}))
           #{{:x 0 :y 0 :width 2 :height 1} {:x 0 :y 1 :width 2 :height 1}}))))

(deftest find-first-gap-test
  (testing "d"
    (is (= {:x 2 :y 0 :height 1} (find-first-gap {:x 1 :y 1 :width 2 :height 1} [1 1 0 1])))
    (is (= {:x 0 :y 0 :height 1} (find-first-gap {:x 0 :y 1 :width 3 :height 1} [0 1 0 1])))
    (is (= {:x 0 :y 0 :height 1} (find-first-gap {:x 0 :y 1 :width 2 :height 1} [0 1 1 1])))
    (is (= {:x 0 :y 0 :height 2} (find-first-gap {:x 0 :y 2 :width 4 :height 1} [0 2 1 1])))
    (is (= nil (find-first-gap {:x 1 :y 1 :width 2 :height 1} [1 1 1 1])))))

(deftest free-points-test
  (testing "d"
    (is (= [{:x 0 :y 7} {:x 1 :y 7} {:x 2 :y 12}] (free-points 2 [0 7 7 12])))
    (is (= [{:x 0 :y 1} {:x 1 :y 1} {:x 2 :y 1} {:x 3 :y 1}] (free-points 2 [0 1 0 1 0])))
    (is (= [{:x 0 :y 0} {:x 1 :y 7} {:x 2 :y 7} {:x 3 :y 12}] (free-points 1 [0 7 7 12])))))

(deftest layout-iteration-test
  (testing "d"
    (is (= [{:x 0 :y 0 :width 2 :height 42}])
           (layout-iteration 2 [] {:width 2 :height 42}))
    (is (= [{:x 0 :y 0 :width 2 :height 42}, {:x 2 :y 0 :width 2 :height 42}])
           (layout-iteration 4 [{:x 0 :y 0 :width 2 :height 42}] {:width 2 :height 42}))
    (is (= [{:x 0 :y 0 :width 2 :height 42}, {:x 0 :y 42 :width 2 :height 42}])
           (layout-iteration 2 [{:x 0 :y 0 :width 2 :height 42}] {:width 2 :height 42}))
    (is (= (conj layouted-some-rects {:x 0 :y 7 :width 2 :height 42})
           (layout-iteration 4 layouted-some-rects {:width 2 :height 42})))
    (is (= (conj layouted-some-rects {:x 2 :y 6 :width 1 :height 42})
           (layout-iteration 4 layouted-some-rects {:width 1 :height 42})))))

(deftest layout-test
  (testing "d"
    (is (= [] (layout 2 [])))
    (is (= [{:x 0 :y 0 :width 1 :height 1}]
           (layout 2 [{:width 1 :height 1}])))
    (is (= (set rects-for-some-photos)
           (set (layout 2 some-photos))))))

(deftest find-neighbors-test
  (testing "d"
    (is (= {:foo 1} (find-rect (fn [x] true) [{:foo 1}])))
    (is (= nil (find-left-neighbor {:x 1 :y 0} [])))
    (is (= {:x 0 :y 0 :width 1 :height 1}
             (find-left-neighbor {:x 1 :y 0} [{:x 0 :y 0 :width 1 :height 1}])))
    (is (= {:x 1 :y 0 :width 1 :height 1}
             (find-top-neighbor {:x 1 :y 1}
                [{:x 1 :y 0 :width 1 :height 1}, {:x 0 :y 1 :width 2 :height 1}])))
    (is (nil? (find-left-neighbor {:x 1 :y 1} [{:x 0 :y 0 :width 1 :height 2} {:x 1 :y 0 :width 1 :height 1}])))
    (is (= {:x 1 :y 0 :width 1 :height 1}
             (find-top-neighbor {:x 1 :y 1} [{:x 0 :y 0 :width 1 :height 2} {:x 1 :y 0 :width 1 :height 1}])))))

(deftest stretch-test
  (testing "d"
    (is (= [{:x 0 :y 0 :width 2 :height 1}]
           (x-stretch {:x 0 :y 0 :width 1 :height 1}
                     [{:x 0 :y 0 :width 1 :height 1}])))
    (is (= [{:x 0 :y 0 :width 2 :height 1} {:x 1 :y 1 :width 2 :height 2}]
           (x-stretch {:x 0 :y 0 :width 1 :height 1}
             [{:x 0 :y 0 :width 1 :height 1} {:x 1 :y 1 :width 2 :height 2}])))
    (is (= [{:x 1 :y 0 :width 2 :height 2} {:x 0 :y 0 :width 1 :height 3}]
           (y-stretch {:x 1 :y 0 :width 2 :height 1} 1
              [{:x 1 :y 0 :width 2 :height 1} {:x 0 :y 0 :width 1 :height 3}])))
    (is (= [{:x 0 :y 0 :width 1 :height 2} {:x 1 :y 0 :width 1 :height 2}]
            (y-stretch {:x 1 :y 0 :width 1 :height 1} 1
                       [{:x 0 :y 0 :width 1 :height 2} {:x 1 :y 0 :width 1 :height 1}])))))
