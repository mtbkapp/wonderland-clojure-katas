(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :as sets])
  (:import [clojure.lang PersistentQueue]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn- diff-words
  "Returns the number of characters that are different between word1 and word2."
  [word1 word2]
  (->> (map (complement #(= % %2)) word1 word2)
       (reduce
         (fn [word-diff char-diff?]
           (if char-diff?
             (inc word-diff)
             word-diff))
         0)))

(defn- add-word
  [adjacency-map word adjacent-word]
  (update adjacency-map word #((fnil conj #{}) % adjacent-word)))

(defn- add-group
  [adjacency-map group]
  (->> (for [w1 group w2 group :when (= 1 (diff-words w1 w2))]
         [w1 w2])
       (reduce (fn [adjacency-map [w1 w2]]
                 (-> adjacency-map
                     (add-word w1 w2)
                     (add-word w2 w1)))
               adjacency-map)))

(defn- build-word-graph
  "Build a graph of adjacent words. Words are adjacent if they differ by only
  1 character. Returns a mapping between a word and the set of adjacent words."
  [words]
  (reduce add-group {} (vals (group-by count words))))

(defn- bfs
  "Tekes a graph from build-word-graph and does a breadth first search starting
  at start. Returns a mapping of graph nodes to the node the preceeded it in
  the search."
  ([graph start]
   (bfs graph {start nil} (conj (PersistentQueue/EMPTY) start)))
  ([graph tree queue]
   (if (not-empty queue)
     (let [vertex (peek queue)
           unvisited-adj (sets/difference (set (get graph vertex))
                                          (set (keys tree)))]
       (recur graph
              (reduce #(assoc % %2 vertex)
                      tree
                      unvisited-adj)
              (into (pop queue) unvisited-adj)))
     tree)))

(defn- root-to-node-path*
  [tree [head :as path] n]
  (if-let [parent (get tree head)]
    (recur tree (cons parent path) (inc n))
    path))

(defn- root-to-node-path
  "Given a tree obtained from bfs returns the path from the root to the given
  node"
  [tree node]
  (let [path (root-to-node-path* tree [node] 0)]
    (if (< 2 (count path))
      path
      [])))

(defn doublets
  [word1 word2]
  (let [graph (build-word-graph words)
        bfs-tree (bfs graph word1)]
    (root-to-node-path bfs-tree word2)))
