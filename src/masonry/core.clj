(ns masonry.core)

(defn contains-column [rect col]
  (and (<= (:x rect) col) (> (+ (:width rect) (:x rect)) col)))

(defn bottom [rect]
  (+ (:y rect) (:height rect)))

(defn find-bottom-in-column [col rects]
  (let [rs (filter #(contains-column % col) rects)]
      (reduce max 0 (map bottom rs))))

(defn lower-edge-shape [grid-width rects]
  (map #(find-bottom-in-column % rects) (range grid-width)))

(defn free-y-at-column-for-width [col w shape]
  (if (> (+ col w) (count shape))
    (throw (Exception. "col + w > shape"))
    (reduce max 0 (take w (drop col shape)))))

(defn free-points [w shape]
  (let [available-xs (range (inc (- (count shape) w)))
        available-ys (map #(free-y-at-column-for-width % w shape) available-xs)]
    (vec (map #(zipmap [:x :y] %) (map vector available-xs available-ys)))))

(defn rect-less [rect1 rect2]
  (if
    (or
      (< (:y rect1) (:y rect2))
      (and (= (:y rect1) (:y rect2)) (< (:x rect1) (:x rect2))))
      rect1
      rect2))

(defn find-first-gap [rect shape]
  (let [gap (reduce rect-less
                      (take (:width rect)
                        (drop (:x rect)
                          (free-points 1 shape))))]
    (if (< (:y gap) (:y rect))
      gap
      nil)))

(defn y-stretch [{:keys [height] :as rect} rects]
  (map #(if (= % rect) (assoc rect :height (inc height)) %) rects))

(defn x-stretch [{:keys [width] :as rect} rects]
  (map #(if (= % rect) (assoc rect :width (inc width)) %) rects))

(declare layout-iteration)

(defn find-rect [pred rects]
  (first (filter pred rects)))

(defn find-top-neighbor [{:keys [x y]} rects]
  (find-rect #(and (= (:x %) x) (= (+ (:y %) (:height %)) y)) rects))

(defn find-left-neighbor [{:keys [x y]} rects]
  (find-rect #(and (= (:y %) y) (= (+ (:x %) (:width %)) x)) rects))

(defn eliminate-gap [grid-width gap rects newrect]
  (let [top-neighbor (find-top-neighbor gap rects)
        left-neighbor (find-left-neighbor gap rects)]
    (cond (not (nil? top-neighbor))
            (conj (y-stretch top-neighbor rects) newrect)
          (not (nil? left-neighbor))
            (layout-iteration
              grid-width (x-stretch left-neighbor rects) newrect)
          :else (throw (Exception. 
                 (str "No neighbors for gap " gap " rects " rects))))))

(defn refine [grid-width rects newrect shape]
  (let [gap (find-first-gap newrect shape)]
    (if (nil? gap)
      (conj rects newrect)
      (eliminate-gap grid-width gap rects newrect))))

(defn layout-iteration [grid-width rects p]
    (let [shape (lower-edge-shape grid-width rects)
          {:keys [x y]} (reduce rect-less
                  (free-points (:width p) shape))
          newrect (assoc p :x x :y y)]
    (refine grid-width rects newrect shape)))

(defn layout [grid-width photos]
  (reduce (fn [rects p] (layout-iteration grid-width rects p)) [] photos))

