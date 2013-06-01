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
    (map vector available-xs available-ys)))

(defn rect-less [[x1 y1] [x2 y2]]
  (cond
    (< y1 y2) [x1 y1]
    (and (= y1 y2) (< x1 x2)) [x1 y2]
    :else [x2 y2]))

(defn find-first-gap [rect shape]
  (let [[gapx gapy] (reduce rect-less
                      (take (:width rect)
                        (drop (:x rect)
                          (free-points 1 shape))))]
    (if (< gapy (:y rect))
      [gapx gapy]
      nil)))

(defn eliminate-gaps [grid-width rects]
  rects)

(defn layout-iteration [grid-width rects p]
    (let [[x y] (reduce rect-less
                  (free-points (:width p) (lower-edge-shape grid-width rects)))]
    (eliminate-gaps grid-width (conj rects (assoc p :x x :y y)))))

(defn layout [grid-width photos]
  (reduce (fn [rects p] (layout-iteration grid-width rects p)) [] photos))

