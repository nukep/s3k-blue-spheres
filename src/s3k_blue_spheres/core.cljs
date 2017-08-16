(ns s3k-blue-spheres.core
  (:require-macros [s3k-blue-spheres.test :as test])
  (:require [reagent.core :as reagent]))

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


(def keyboard-input (reagent/atom #{}))

(def sonic-state (reagent/atom {:queued-turn nil :transition 0 :position [1 1] :direction :north :mode :forward}))

(def turn-left)


(def turn-right)


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

(def STEPS_BETWEEN_UNIT 16)
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
  {:north 270
   :east 0
   :south 90
   :west 180})

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

(defn hello-world []
  (debugdata (remove nil? (for [y (range 32)
                                x (range 32)]
                            nil))))

; (tick-sonic-state)

(defn threedee-view []
  (let [tick (reagent/atom 0)]
    (fn []
      (js/requestAnimationFrame #(swap! tick inc))
      ((comp vec concat)
       [:svg {:width 800 :height 400}]
       [[:ellipse {:cx 400 :cy 570 :rx 400 :ry (* 400 1.2) :fill "#f89018"}]]
       (map-indexed (fn [idx path]
                      ^{:key (str idx)}
                      [:path {:fill "#682408" :d (poly-to-svg-d (map #(tf (Math/sin (* @tick 0.05)) (Math/sin (* @tick 0.1)) %) (polygon-erp 2 path)))}])
            (checkerboard RESOLUTION RESOLUTION))))))

(def twodee-lookup
  {:empty       (fn [x y] nil)
   :red         (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 5 :fill "#f88"}])
   :blue        (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 4 :fill "blue"}])
   :bumper      (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 3 :fill "#ccc"}])
   :ring        (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 3 :fill "yellow"}])
   :trampoline  (fn [x y] [:circle {:cx (+ 5 (* 10 x)) :cy (+ 5 (* 10 y)) :r 3 :fill "orange"}])})

(defn twodee-view []
  (let [levelitem (nth levels 2)
        name (:name levelitem)
        level (:level levelitem)
        leveldata (:data level)
        {[px py] :position angle :angle} (sonic-state-to-calculated @sonic-state)]
    ((comp vec concat)
     [:svg {:width 320 :height 320}]
     (remove nil? (for [y (range HEIGHT)
                        x (range WIDTH)]
                    (let [row (get leveldata y)
                          item (get row x)]
                      ^{:key (str x "x" y)} ((twodee-lookup item) x y))))
     [[:path {:d (poly-to-svg-d [[-5 -5] [5 0] [-5 5]])
              :transform (str "translate(" (+ 5 (* 10 px)) " " (+ 5 (* 10 py)) ") rotate(" angle ")")}]])))

(defn show-state []
  (fn []
    [:div
     [:h1 [:text "Keyboard input"]]
     (debugdata @keyboard-input)
    ;  [:div [:text (str @keyboard-input)]]
     (debugdata (keyboard-input-to-buttons @keyboard-input))
     [:h1 [:text "Sonic state"]]
     [:pre [:text (str @sonic-state)]]
     [:pre [:text (str (sonic-state-to-calculated @sonic-state))]]
     [twodee-view]
     [threedee-view]]))

;; Render the application
(reagent/render-component [show-state]
                          (. js/document (getElementById "app")))

(defonce setup-stuff
  (do (js/setTimeout tick-sonic-state 1000)))

;; define your app data so that it doesn't get over-written on reload
; (defonce app-state (atom {:text "Hello world!"}))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application

  ;; (swap! app-state update-in [:__figwheel_counter] inc))
