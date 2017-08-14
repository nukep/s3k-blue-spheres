(ns s3k-blue-spheres.core
  (:require-macros [s3k-blue-spheres.test :as test])
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def WIDTH 32)
(def HEIGHT 32)

(defn parseLevel [data]
  ;; The uncompressed level data in the Sonic 3 ROM is laid out in memory as such:
  ;; <ITEM DATA: 1024 bytes> <METADATA: 8 bytes>
  (let [itemdata (subvec data 0 (* WIDTH HEIGHT))
        metadata (subvec data (* WIDTH HEIGHT))]
     ;; data is a vector of vectors. Rows, then items in the row.
    {:data (->> (partition WIDTH itemdata)
                (map #(map {0 :empty 1 :red 2 :blue 3 :bumper 4 :ring 5 :trampoline} %))
                (map vec)
                (vec))

     ;; The other attributes, such as Sonic's starting position, are based on the level's metadata
     :sonicX    (get metadata 2)
     :sonicY    (get metadata 4)
     :direction ({0x00 :up 0x40 :left 0x80 :down 0xC0 :right} (get metadata 0))}))

;; Load and parse levels
(def levels
  (map (fn [x] {:name (x :name) :level (parseLevel (x :data))})
       (test/loadLevels)))

(def itemToChar
  {:empty      " "
   :red        "▨"
   :blue       "◆"
   :bumper     "✪"
   :ring       "◎"
   :trampoline "^"})

(defn presentLevel [level]
  (->> (level :data)
       (map #(clojure.string/join " " (map itemToChar %)))
       (clojure.string/join "\n")))

(defn debugdata [x]
  [:pre [:text (with-out-str (cljs.pprint/pprint x))]])

;;; Transformation functions.
;;; The point to be transformed is always the last argument,
;;; to allow for pipelining with the thread-last macro (->>).

(defn translate [tx ty [x y]] [(+ x tx) (+ y ty)])
(defn scale
  ([sx sy [x y]] [(* x sx) (* y sy)])
  ([s [x y]]     [(* x s) (* y s)]))
(defn rotate [radius [x y]]
  (let [r (Math/sqrt (+ (* x x) (* y y)))
        theta (Math/atan2 y x)
        new-theta (+ theta radius)]
    [(* r (Math/cos new-theta))
     (* r (Math/sin new-theta))]))
(defn rotate-at [[tx ty] radius point]
  (->> point
       (translate (- tx) (- ty))
       (rotate radius)
       (translate tx ty)))

;; Fisheye distortion. This conforms all points to a spherical shape.
;; The further the point is from the origin, the more the effect is applied.
; https://popscan.blogspot.ca/2012/04/fisheye-lens-equation-simple-fisheye.html
(defn fisheye [[x y]]
  (let [r (Math/sqrt (+ (* x x) (* y y)))
        mr (min 0.98 r)
        theta (Math/atan2 y x)
        new-r (+ mr
                 (* 0.5 (+ -1
                           (Math/pow (- 1 (* mr mr)) 1))))]
    [(* new-r (Math/cos theta))
     (* new-r (Math/sin theta))]))

;; Linear interpolation
;; p ranges from 0 to 1.
;; When p = 0, result is a. When p = 1, result is b. When p is between 0 and 1, result is proportionally between a and b.
(defn lerp [a b p] (+ a (* p (- b a))))

;; Linear interpolation on points
(defn lerp-xy [[x1 y1] [x2 y2] p] [(lerp x1 x2 p) (lerp y1 y2 p)])

;; Adds points between polygon edges.
;; If n = 1, it's the same as doing nothing.
;; If n = 2, it adds an additional point in between each point (2 points per)
(defn polygon-erp [n points]
  (vec
    (mapcat (fn [cur next] (map #(lerp-xy cur next (/ % n)) (range n)))
            points
            (concat (rest points) [(first points)]))))

(def RESOLUTION 14)

;; r is rotation, and ranges from -1 to +1
;; t is y translation, and ranges from -1 to 1
(defn tf [r t input]
  (->> input
       (scale (/ 1 RESOLUTION))
       (translate -0.5 (/ -9 RESOLUTION))
       (scale 2)
      ;  (rotate-at [0 (/ -4 RESOLUTION)] (* r (/ Math/PI 2)))
       (translate 0 (* t (/ 1 RESOLUTION)))
       (fisheye)
       (scale 800)
       (scale 1 1.2)
       (translate 400 570)))

(defn points-to-svg [points]
  (->> points
       (map-indexed (fn [idx [x y]] ^{:key idx} [:circle {:cx x :cy y :r 2 :fill "orange"}]))
       (vec)))

(defn poly-to-svg-d [[[fx fy] & r]]
  (clojure.string/join
   " "
   (concat
    [(str "M " fx " " fy)]
    (map (fn [[x y]] (str "L " x " " y)) r)
    ["Z"])))

; FIXME - max-x doesn't work on odd amounts
(defn checkerboard [max-x max-y]
  (vec (for [y (range max-y)
             xx (range (/ max-x 2))]
         (let [offset (mod y 2)
               x (+ (* 2 xx) offset)]
           [[x y] [(inc x) y] [(inc x) (inc y)] [x (inc y)]]))))


; assert that only one of x or y is a fraction

;; Returns arguments to pass to tf
; (defn position-to-tf [x y r])


(defn threedee-view []
  (let [tick (atom 0)]
    (fn []
      (js/requestAnimationFrame #(swap! tick inc))
      ((comp vec concat)
       [:svg {:width 800 :height 400}]
       [[:ellipse {:cx 400 :cy 570 :rx 400 :ry (* 400 1.2) :fill "#f89018"}]]
       (map (fn [path] [:path {:fill "#682408" :d (poly-to-svg-d (map #(tf (Math/sin (* @tick 0.05)) (Math/sin (* @tick 0.1)) %) (polygon-erp 2 path)))}])
            (checkerboard RESOLUTION RESOLUTION))))))

(def twodee-lookup
  {:empty       (fn [x y] nil)
   :red         (fn [x y] [:rect {:x (* 10 x) :y (* 10 y) :width 10 :height 10 :fill "red"}])
   :blue        (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 4 :fill "blue"}])
   :bumper      (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 4 :fill "#ccc"}])
   :ring        (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 4 :fill "yellow"}])
   :trampoline  (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 4 :fill "orange"}])})

(defn twodee-view []
  (let [levelitem (nth levels 12)
        name (:name levelitem)
        level (:level levelitem)
        leveldata (:data level)]
    ((comp vec concat)
     [:svg {:width 320 :height 320}]
     (remove nil? (for [y (range 32)
                        x (range 32)]
                    (let [row (get leveldata y)
                          item (get row x)]
                      ((twodee-lookup item) x y)))))))

(defn hello-world []
  (debugdata (remove nil? (for [y (range 32)
                                x (range 32)]
                            nil))))

;; Render the application
(reagent/render-component [threedee-view]
                          (. js/document (getElementById "app")))


;; define your app data so that it doesn't get over-written on reload
; (defonce app-state (atom {:text "Hello world!"}))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc))
