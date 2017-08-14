(ns s3k-blue-spheres.test)


(def WIDTH 32)
(def HEIGHT 32)
(def LEVEL_SIZE (+ (* WIDTH HEIGHT) 8))

(defn byte-to-unsigned [x]
  (long (if (< x 0) (+ x 256) x)))

;; https://stackoverflow.com/questions/23018870/how-to-read-a-whole-binary-file-nippy-into-byte-array-in-clojure
(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (vec (map byte-to-unsigned (.toByteArray out)))))

;; This macro will load the level data from the file system and associate the data with names.
;; To minimise the size of the JS, level data will be parsed on the client-side.
(defmacro loadLevels []
  (->> (concat
         ;; Sonic 3 levels
         (->> (range 1 9)
              (map
                (fn [idx]
                  {:name (str "Sonic 3 - Stage " idx)
                   :data (slurp-bytes (str "Layout/S3 " idx ".bin"))})))

         ;; Sonic 3 & Knuckles levels
         (->> (partition LEVEL_SIZE (slurp-bytes "Layout/SK Set 1.bin.out"))
              (map vec)
              (map-indexed
                (fn [idx x]
                  {:name (str "Sonic 3 & Knuckles - Stage " (+ idx 1))
                   :data x}))))
       (vec)))
