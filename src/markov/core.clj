(ns markov.core
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn word-chain [word-transitions]
  (reduce (fn [r [p1 p2 s]] (merge-with set/union r
                                        {[p1 p2] (if s #{s} #{})}))
          {} word-transitions))

(defn text->word-chain
  [text]
  (let [words (s/split text #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (word-chain word-transitions)))

(defn chain->text
  [chain]
  (apply str (interpose " " chain)))

(defn walk-chain
  [prefix chain result char-limit]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (>= new-result-char-count char-limit)
          result
          (recur new-prefix chain (conj result suffix) char-limit))))))

(defn generate-text
  [start-phrase word-chain char-limit]
  (let [prefix (s/split start-phrase #" ")
        result-chain (walk-chain prefix word-chain prefix char-limit)
        result-text (chain->text result-chain)]
    result-text))

(defn process-file
  [fname]
  (text->word-chain (slurp fname)))

(comment

  Usage

  (def prefixes ["We have" "These institutions" "It will" "So you" "God bless" "I think"])
  (def files ["resource/text1.txt" "resource/text2.txt" "resource/text3.txt"])
  (def functional-raegan (apply merge-with set/union (map process-file files)))
  (def example-chain (generate-text (first (shuffle prefixes)) functional-raegan 300))

  Provided in the resources directory are three transcripts of speeches given by former President Ronald Raegan

  I didn't lint them very well so they might provide some funky behavior.
)













(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
