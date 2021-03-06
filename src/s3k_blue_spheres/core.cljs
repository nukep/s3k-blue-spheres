(ns s3k-blue-spheres.core
  (:require-macros [s3k-blue-spheres.levels :refer [load-levels]])
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.pprint :refer [pprint]]))

;;;;; Global State ;;;;;
(def keyboard-input (r/atom #{}))
(def sonic-state (r/atom {:queued-turn nil :transition 0 :position [1 1] :direction :north :mode :forward}))
;;;;;;;;;;;;;;;;;;;;;;;;


(enable-console-print!)

(def WIDTH 32)
(def HEIGHT 32)

(defn parse-level [data]
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
(defonce levels
  (vec (for [item (load-levels)]
         {:name (:name item)
          :level (parse-level (:data item))})))

(defn debugdata [x]
  [:pre [:text (with-out-str (pprint x))]])

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


(def turn
  {:left  {:north :west
           :west :south
           :south :east
           :east :north}
   :right {:north :east
           :east :south
           :south :west
           :west :north}
   nil    identity})

(defn move [direction [x y]]
  (let [[nx ny] (condp = direction
                  :north [x (dec y)]
                  :east  [(inc x) y]
                  :south [x (inc y)]
                  :west  [(dec x) y])]
    [(mod nx WIDTH) (mod ny HEIGHT)]))

(def STEPS_BETWEEN_UNIT 8)
(def STEPS_TO_TURN 8)

(defn advance-sonic-state [input state]
  (condp = (:mode state)
    :forward
    (let [transition (inc (:transition state))
          move-now (>= transition STEPS_BETWEEN_UNIT)
          queued-turn (condp #(contains? %2 %1) input
                        ;; left (key) to left (way to turn)
                        :left :left
                        :right :right
                        (:queued-turn state))
          transition (if move-now 0 transition)
          direction (:direction state)
          position (:position state)
          position (if move-now (move direction position) position)]
          ; direction (if move-now
          ;             ((turn queued-turn) direction)
          ;             direction)
          ; queued-turn (if move-now nil queued-turn)]
      (if (and move-now (not (nil? queued-turn)))
        {:mode :turn
         :turn queued-turn
         :position position
         :direction direction
         :transition 0}
        {:mode :forward
         :queued-turn queued-turn
         :transition transition
         :position position
         :direction direction}))

    :turn
    (let [transition (inc (:transition state))]
      (if (>= transition STEPS_TO_TURN)
        {:mode :forward
         :queued-turn nil
         :transition 0
         :position (:position state)
         :direction ((turn (:turn state)) (:direction state))}
        (assoc state :transition transition)))))

(defn position-to-absolute [direction transition [x y]]
  (let [fraction (/ transition STEPS_BETWEEN_UNIT)
        [nx ny] (condp = direction
                 :north [x (- y fraction)]
                 :east  [(+ x fraction) y]
                 :south [x (+ y fraction)]
                 :west  [(- x fraction) y])]
    [(mod nx WIDTH) (mod ny HEIGHT)]))

(def direction-to-angle
  {:north 0
   :east 90
   :south 180
   :west 270})

(defn angle-delta-from-state
  [{turn :turn transition :transition}]
  (let [mult ({:left -90 :right 90} turn)]
    (* mult (/ transition STEPS_TO_TURN))))

(defn sonic-state-to-calculated [state]
  (condp = (:mode state)
    :forward {:position (position-to-absolute (:direction state) (:transition state) (:position state))
              :angle (direction-to-angle (:direction state))}
    :turn    {:position (position-to-absolute (:direction state) 0 (:position state))
              :angle (+ (angle-delta-from-state state)
                        (direction-to-angle (:direction state)))}))

; turn left: 1 -> 0 or 0 -> -1
; turn right: -1 -> 0 or 0 -> 1

(defn calculated-to-floor-tf [{[x y] :position angle :angle}]
  {:rotate 0 ;(if (>= (mod y 2) 1) 1 0)
   :translate-y (mod y 2)})

(defn leveldata-to-items [leveldata]
  (vec (remove nil? (for [y (range HEIGHT)
                          x (range WIDTH)]
                      (let [row (get leveldata y)
                            item (get row x)]
                        (if (= item :empty) nil {:position [x y] :item item}))))))

;; When calculating whether a distance is within a threshold, it's quicker to skip calculating the square root.
;; Profiling shows that whenever hypot is in a critical loop, it becomes expensive!
;;
;; sqrt(x*x + y*y) <= dist
;;   is the same as
;; x*x + y*y <= dist^2
(defn hypot-squared [x y]
  (+ (* x x) (* y y)))

(def YOFF -4)

;; I'm unsure if there are built-ins for this
(defn filter-key
  ([key pred]      (filter (comp pred key)))
  ([key pred coll] (filter (comp pred key) coll)))
(defn map-key
  ([key f]      (map #(update % key f)))
  ([key f coll] (map #(update % key f) coll)))

(defn items-near [items [px py] angle]
  (->> (for [yoff [(- HEIGHT) 0 HEIGHT]
             xoff [(- WIDTH) 0 WIDTH]]
         (into []
               (comp (map-key    :position (fn [[x y]] [(+ xoff x (- px)) (+ yoff y (- py))]))
                       ;; pre-filter to include a superset so that rotating doesn't take as long...
                     (filter-key :position (fn [[x y]] (< (hypot-squared x y) (* 12 12))))
                     (map-key    :position (fn [pos]   (rotate (* Math/PI (/ angle -180)) pos)))
                     (filter-key :position (fn [[x y]] (and (< (hypot-squared x (+ y YOFF)) (* 8 8))
                                                            (< y 2))))
                     (map-key    :position (fn [pos]   (translate 0 YOFF pos))))
               items))
       (apply concat)))

(defn keyboard-input-to-buttons [input]
  (let [keycode-to-button
        {37 :left
         39 :right
         65 :left
         68 :right
         32 :jump
         38 :forward}]
    (disj (set (map keycode-to-button input)) nil)))

(defn tick-sonic-state []
  (let [buttons (keyboard-input-to-buttons @keyboard-input)]
    (js/setTimeout tick-sonic-state (/ 1000 60))
    (swap! sonic-state #(do (advance-sonic-state buttons %)))))

; (defn threedee-view []
;   (let [tick (r/atom 0)]
;     (fn []
;       (let [{rotate :rotate translate-y :translate-y} (calculated-to-floor-tf (sonic-state-to-calculated @sonic-state))]
;         ; (js/requestAnimationFrame #(swap! tick inc))
;         ((comp vec concat)
;          [:svg {:width 800 :height 400}]
;          [[:ellipse {:cx 400 :cy 570 :rx 400 :ry (* 400 1.2) :fill "#f89018"}]]
;          (map-indexed (fn [idx path]
;                         ^{:key (str idx)}
;                         [:path {:fill "#682408" :d (poly-to-svg-d (map #(tf rotate translate-y %) (polygon-erp 2 path)))}])
;               (checkerboard RESOLUTION RESOLUTION))
;          (let [[x y] (tf rotate translate-y [7 6])]
;            [[:circle {:cx x :cy y :r 25}]]))))))

(def twodee-lookup
  {:red         (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 5 :fill "#f88"}])
   :blue        (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 4 :fill "blue"}])
   :bumper      (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 3 :fill "#ccc"}])
   :ring        (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 3 :fill "yellow"}])
   :trampoline  (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 3 :fill "orange"}])})

(def LEVEL_ITEM (nth levels 1))
(def LEVELDATA_ITEMS (leveldata-to-items (-> LEVEL_ITEM :level :data)))

;; This is a separate component to make rerenders faster - and the map itself doesn't change often.
(defn twodee-view-map-bg []
  (into [:g]
        (map (fn [{[x y] :position item :item}]
               ^{:key (str x "x" y)} ((twodee-lookup item) x y))
             LEVELDATA_ITEMS)))

(defn twodee-view []
  (let [{[px py] :position, angle :angle} (sonic-state-to-calculated @sonic-state)]
    [:svg {:width 320 :height 320}
     [twodee-view-map-bg]

     [:path {:d (poly-to-svg-d [[-3 5] [0 -5] [3 5]])
             :transform (str "translate(" (+ 5 (* 10 px)) " " (+ 5 (* 10 py)) ") rotate(" angle ")")}]]))

(defn twodee-view-tf []
  (let [calculated (sonic-state-to-calculated @sonic-state)
        position (:position calculated)
        angle (:angle calculated)]
    ((comp vec concat)
     [:svg {:width 320 :height 320}]
     (map (fn [{position :position item :item}]
            (let [[x y] (translate 16 16 position)]
              ^{:key (str x "x" y)} ((twodee-lookup item) x y)))
          (items-near LEVELDATA_ITEMS position angle))
     [[:circle {:cx 165 :cy (+ 165 (* 10 YOFF)) :r 5 :fill "black"}]])))


(defn tf [input]
  (->> input
       (scale (/ 1 16))
       (scale 2)
       (fisheye)
       (scale 800)
       (scale 1 1.2)
       (translate 400 600)))

(def color-lookup
  {:red         "#f88"
   :blue        "blue"
   :bumper      "#ccc"
   :ring        "yellow"
   :trampoline  "orange"})

(defn threedee-view-tf []
  (let [calculated (sonic-state-to-calculated @sonic-state)
        position (:position calculated)
        angle (:angle calculated)]
    ((comp vec concat)
     [:svg {:width 800 :height 600}]
     (map (fn [{position :position item :item}]
            (let [[x y] (tf position)]
              ^{:key (str x "x" y)} [:circle {:cx x :cy y :r 15 :fill (color-lookup item)}]))
          (items-near LEVELDATA_ITEMS position angle))
     (let [[x y] (tf [0 YOFF])]
       [[:circle {:cx x :cy y :r 5 :fill "black"}]]))))

(defn debug-state-section []
  [:<>
   [:h1 [:text "Keyboard input"]]
   [debugdata @keyboard-input]
   [debugdata (keyboard-input-to-buttons @keyboard-input)]

   [:h1 [:text "Sonic state"]]
   [debugdata @sonic-state]
   [debugdata (sonic-state-to-calculated @sonic-state)]
   [debugdata (calculated-to-floor-tf (sonic-state-to-calculated @sonic-state))]])

(defn app []
  (fn []
    [:div
     [twodee-view]
     [twodee-view-tf]
     [:br]
     [threedee-view-tf]

     [debug-state-section]]))

;; Called on initial page load, and after reloads from file changes.
(defn ^:dev/after-load start []
  (rdom/render [app]
               (. js/document (getElementById "app"))))

;; Called once, during initial page load
(defn ^:export main []
  (set!
   (.-onkeydown js/document)
   (fn [event]
     (let [keycode (.-keyCode event)]
       (swap! keyboard-input #(conj % keycode)))))

  (set!
   (.-onkeyup js/document)
   (fn [event]
     (let [keycode (.-keyCode event)]
       (swap! keyboard-input #(disj % keycode)))))


  (js/setTimeout tick-sonic-state 1000)

  (start))
