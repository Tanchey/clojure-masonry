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

(defn y-stretch [{:keys [height] :as rect} rects]
  (map #(if (= % rect) (assoc rect :height (inc height)) %) rects))

(defn x-stretch [{:keys [width] :as rect} rects]
  (map #(if (= % rect) (assoc rect :width (inc width)) %) rects))

(declare layout-iteration)

(defn find-rect [pred rects]
  (first (filter pred rects)))

(defn find-top-neighbor [[x y] rects]
  (find-rect #(and (= (:x %) x) (= (+ (:y %) (:height %)) y)) rects))

(defn find-left-neighbor [[x y] rects]
  (find-rect #(and (= (:y %) y) (= (+ (:x %) (:width %)) x)) rects))

(defn eliminate-gap [grid-width gap rects newrect]
  (let [ left-neighbor (find-left-neighbor gap rects)
         top-neighbor (find-top-neighbor gap rects)]
  (cond (not (nil? top-neighbor))
          (conj (y-stretch top-neighbor rects) newrect)
        (not (nil? left-neighbor))
          (layout-iteration
            grid-width (x-stretch left-neighbor rects) newrect))
        :else (throw (Exception. 
               (str "No neighbors for gap " gap " rects " rects)))))

(defn refine [grid-width rects newrect shape]
  (let [gap (find-first-gap newrect shape)]
    (if (nil? gap)
      (conj rects newrect)
      (eliminate-gap grid-width gap rects newrect))))

(defn layout-iteration [grid-width rects p]
    (let [shape (lower-edge-shape grid-width rects)
          [x y] (reduce rect-less
                  (free-points (:width p) shape))
          newrect (assoc p :x x :y y)]
    (refine grid-width rects newrect shape)))

(defn layout [grid-width photos]
  (reduce (fn [rects p] (layout-iteration grid-width rects p)) [] photos))

