(ns pip
  "Implementation of Perceptually Important Points algorithm"
  (:require [clojure.contrib.math :as math])
  (:use clojure.test))


(defn max-key-index
  "Returns [key, index] where (f (k item)) is the greatest. Most
applicable to vectors."
  [f k & coll]
  (apply max-key f (map #(vec [(k %1) %2]) coll (range (count coll)))))


(defn euclidian-distance
  [[x1 y1] [x2 y2]]
  (math/sqrt (+ (math/expt (- x1 x2) 2)
		(math/expt (- y1 y2) 2))))


(defn distance
  "Receives three points in [x y] format"
  [distance-fn first last point]
  (+ (distance-fn first point) (distance-fn last point)))


(defn max-distance
  "Returns [distance, index of point] where distance is greatest."
  [series from to]
  (let [calc-dist (partial distance euclidian-distance (nth series from) (nth series to))]
    (apply max-key-index first calc-dist (subvec series from (+ 1 to)))))


(def max-distance (memoize max-distance))


(defn max-distance-over-intervals 
  "Find pip for each interval, and returns [[distance index-point-in-interval] index-interval] for the interval where distance of the pip is the greatest."
  [series pips]
  (let [candidate-intervals (filter (fn [[p1 p2]] (> p2 (+ 1 p1))) pips)]
    (when-not (empty? candidate-intervals)  
      (apply max-key-index ffirst #(apply max-distance series %) candidate-intervals))))


(defn pip*
  "Expect series as vector of points. pips is a vector of [start end] intervals which are defined by the important points extracted so far. Given the current intervals, this function chooses a new important point with max distance across all segments. The new point is used to split one of the current segments."
  [series pips]
  (if-let [[[_ relative-index-pip] index-interval] (max-distance-over-intervals series pips)]
    (let [[p1 p2] (nth pips index-interval)
	  index-new-pip (+ p1 relative-index-pip)]
;split segment into two segments
      (conj (assoc pips index-interval
		   [p1 index-new-pip]) [index-new-pip p2]))))

(defn pip-seq
  [series pips]
  (if-let [new-pips (pip* series pips)]
    (cons new-pips
	  (lazy-seq (pip-seq series new-pips)))))

(defn find-pips [series]
  (let [extremes [0 (- (count series) 1)]]
    (conj (pip-seq series [extremes]) extremes)))






;tests TODO: completar!
(deftest test-euclidian-distnace 
  (is (= (euclidian-distance [10 20] [5 15]) (math/sqrt 50) )))

(deftest test-distance distance
  (testing "euclidian distance"
    (is (= (distance euclidian-distance [1 10] [100 26] [3 40]) 128.0716946647587))))





;example
(comment 
  (def series-example-y [1,  2,  4,  10,  20,  1,  3 ,  1,  10,  1,  10,
			 5, 30,  5,  1,  9,  13,  1,  0,  20,  1,  2,  4,  10,  5,  2,  50,
			 0,  7,  8, 5,  4,  3,  2,  20,  1,  3 ,  1,  10,  1,  10,  5, 30,  5,
			 1,  9,  13,  1,  0,  20,  1,  2,  4,  10,  20,  1,  3 ,  1,  10,  1,
			 10,  5, 30,  5,  1,  9,  13,  1,  0,  20,  1,  2,  4,  10,  5,  2,
			 50,  0,  7,  8, 5,  4,  3,  2,  20,  1,  3 ,  1,  10,  1,  10,  5, 30,
			 5,  1,  9,  13,  1,  0, 20])
  (def series-example (vec (map (fn [x y] [x y]) (range (count
							 series-example-y)) series-example-y)))


  (def pips (find-pips series-example))
  (sort-by first (last (take 10 pips)))
;([0 12] [12 25] [25 26] [26 27] [27 59] [59 62] [62 75] [75 76] [76 77] [77 99])

  (sort-by first (last (take 20 pips)))
;([0 12] [12 25] [25 26] [26 27] [27 42] [42 59] [59 62] [62 75] [75 76] [76 77] [77 99] [77 77] [77 77] [77 77] [77 77] [77 77] [77 77] [77 77] [77 77] [77 77])


)

