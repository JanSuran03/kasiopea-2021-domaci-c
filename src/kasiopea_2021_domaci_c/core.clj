(ns kasiopea-2021-domaci-c.core
  (:require [clojure.string :as str]))

(defn split-string-and-map-read [string]
  (->> (str/split string #"\ ")
       (map read-string)))

(defn read-input []
  (let [pure-input (->> "input.txt" slurp
                        str/split-lines
                        rest)]
    (loop [input pure-input
           processed-input []]
      (if (seq input)
        (let [[incoming-rows _ _] (split-string-and-map-read (first input))
              next-input (nthrest input (inc incoming-rows))
              this-cake (take incoming-rows (drop 1 input))
              read-str (map #(as-> % <> (str/split <> #"\ ")
                                   (map read-string <>))
                            this-cake)]
          (recur next-input
                 (conj processed-input read-str)))
        processed-input))))

(defn -main [& args]
  (read-input))
