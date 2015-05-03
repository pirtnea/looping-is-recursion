(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc seq1 seq2]
                 (cond
                  (and (empty? seq1) (empty? seq2)) true
                  (or (empty? seq1) (empty? seq2)) false
                  (= (first seq1) (first seq2)) (recur true (rest seq1) (rest seq2))
                  :else false))]
    (helper false seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0 b-seq a-seq]
    (cond
     (empty? b-seq) nil
     (pred (first b-seq)) index
     :else (recur (inc index) (rest b-seq)))))

(defn avg [a-seq]
  (loop [acc 0 total 0 b-seq a-seq]
    (if (empty? b-seq)
      (/ acc total)
      (recur (+ acc (first b-seq)) (inc total) (rest b-seq)))))

(defn parity [a-seq]
  (loop [a-set #{} b-seq a-seq]
    (cond
     (empty? b-seq) a-set
     (contains? a-set (first b-seq)) (recur (disj a-set (first b-seq)) (rest b-seq))
     :else (recur (conj a-set (first b-seq)) (rest b-seq)))))

(defn fast-fibo [n]
  (loop [acc 0 fibN 0 fibN1 1]
    (if (== acc n)
      fibN
      (recur (inc acc) (+ fibN fibN1) fibN))))

(defn cut-at-repetition [a-seq]
  (loop [acc [] b-seq a-seq]
    (cond
     (contains? (set acc) (first b-seq)) acc
     (nil? (first b-seq)) acc
     :else (recur (conj acc (first b-seq)) (rest b-seq)))))
