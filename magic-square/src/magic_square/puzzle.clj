(ns magic-square.puzzle
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn triple+
  [[x y z] sum]
  (l/fresh [s]
           (fd/+ x y s)
           (fd/+ s z sum)))

(defn solve
  [vars domain rows cols diag0 diag1]
  (l/run 1 [q sum]
         (l/== q vars)
         (l/everyg #(fd/in % domain) vars)
         (triple+ diag0 sum)
         (triple+ diag1 sum)
         (l/everyg #(triple+ % sum) rows)
         (l/everyg #(triple+ % sum) cols)
         (fd/distinct vars)))

(defn present-result
  [result]
  (->> result
       (map #(/ % 10.0))
       (partition 3)
       (map vec)
       (into [])))

(defn magic-square
  [values]
  (let [vars (repeatedly 9 l/lvar)
        rows (partition 3 vars)
        cols (apply map vector rows)
        diag0 (map #(nth vars %) [0 4 8])
        diag1 (map #(nth vars %) [2 4 6])
        domain (->> values
                     (map (comp long (partial * 10)))
                     sort
                     (apply fd/domain))
        [[result _]] (solve vars domain rows cols diag0 diag1)]
    (present-result result)))

