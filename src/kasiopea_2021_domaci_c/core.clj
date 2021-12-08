(ns kasiopea-2021-domaci-c.core
  (:require [clojure.string :as str]))

(defn read-input []
  (let [pure-input (->> "input.txt" slurp
                        str/split-lines
                        rest)]
    (loop [input pure-input
           processed-input []]
      (if (seq input)
        (let [[incoming-rows _total-squares _colors] (as-> input <> (first <>)
                                                           (str/split <> #"\ ")
                                                           (map read-string <>))
              next-input (nthrest input (inc incoming-rows))
              this-cake (take incoming-rows (drop 1 input))
              read-str (map #(as-> % <> (str/split <> #"\ ")
                                   (map read-string <>))
                            this-cake)]
          (recur next-input
                 (conj processed-input read-str)))
        processed-input))))

(defn solve-row [row]
  (let [first-n-uncolored (loop [i 0
                                 [color & more] row]
                            (if (= color -1)
                              (recur (inc i)
                                     more)
                              i))
        [uncolored [first-color :as remaining]] (split-at first-n-uncolored row)
        colored-rest (loop [[current-square & remaining] remaining
                            ret (transient [])
                            current-color nil]
                       (cond (not current-square)
                             (persistent! ret)

                             (= current-square -1)
                             (recur remaining
                                    (conj! ret current-color)
                                    current-color)

                             :else
                             (recur remaining
                                    (conj! ret current-square)
                                    current-square)))]
    (concat (repeat (count uncolored) first-color) colored-rest)))

(defn solve-cake [rows]
  (loop [[{:keys [row-empty? row-data] :as first-row} & remaining-rows] rows
         remember-rows 0
         ret []]
    (cond (not first-row)                                   ; NO DATA ANYMORE -> WE FILL EMPTY ROWS
          (let [first-not-empty (first ret)]
            (concat (repeat remember-rows first-not-empty)
                    ret))

          row-empty?
          (if-let [lr (last ret)]
            (recur remaining-rows                           ; ROW EMPTY, BUT WE CAN COPY THE ROW BEFORE
                   remember-rows
                   (conj ret lr))
            (recur remaining-rows
                   (inc remember-rows)
                   ret))

          :else                                             ; ROW NOT EMPTY
          (let [this-row (solve-row row-data)
                with-this-row (conj ret this-row)]
            (recur remaining-rows
                   0
                   (vec (concat (repeat remember-rows this-row)
                                with-this-row)))))))

(defn explore-rows [rows]
  (let [checked-rows (map (fn [row]
                            (let [row-empty? (every? #(= % -1) row)]
                              {:row-data   row
                               :row-empty? row-empty?}))
                          rows)]
    checked-rows))

(defn -main [& _args]
  (let [inputs (read-input)]
    (->> inputs (map explore-rows)
         (map solve-cake)
         (map #(->> % (map (fn [row]
                             (str/join " " row)))
                    (str/join "\n")))
         (str/join "\n")
         (spit "output.txt"))))
