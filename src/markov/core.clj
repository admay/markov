(ns markov.core
  (:gen-class))

(def example "And the Golden Grouse And the Pobble who")

(defn word-chain [word-transitions]
  (reduce (fn [r [p1 p2 s]] (merge-with clojure.set/union r
                                        {[p1 p2] (if s #{s} #{})}))
          {} word-transitions))

(defn text->word-chain
  [text]
  (let [words (clojure.string/split text #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (word-chain word-transitions)))

(defn walk-chain
  [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]]
        (recur new-prefix chain (conj result suffix))))))





(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
