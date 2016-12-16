(ns get-wild-89s.core)

(def not-wild-and-tough
  #{\! \[ \] \( \) \space \newline \# \* \_ \`})

(def wild-and-tough
  {[\0 \0] "！"
   [\0 \1] "げっ"
   [\1 \0] "わいー"
   [\1 \1] "えんたー"})

(defn char->bin
  [c]
  (->> c
       int
       Integer/toBinaryString
       (format "%16s")
       (map #(if (= \space %) \0 %))))


(defn get-wild-and-tough
  [c]
  (if (not-wild-and-tough c)
    c
    (->> c
         char->bin
         (partition 2)
         (map #(get wild-and-tough %)))))

(defn get-wild-89s
  [s]
  (->> s
       (map get-wild-and-tough)
       flatten
       (apply str)))

(defn get-back-wild-and-tough
  [s]
  (loop [s s
         res []]
    (if (empty? s)
      res
      (cond
        (= (first s) \！) (recur (rest s) (conj res [\0 \0]))
        (= (take 2 s) (seq "げっ")) (recur (drop 2 s) (conj res [\0 \1]))
        (= (take 3 s) (seq "わいー")) (recur (drop 3 s) (conj res [\1 \0]))
        (= (take 4 s) (seq "えんたー")) (recur (drop 4 s) (conj res [\1 \1]))
        :else (recur (rest s) (conj res (-> s first char->bin)))))))

(defn get-back-wild-89s
  [s]
  (->> s
       seq
       get-back-wild-and-tough
       flatten
       (partition 16)
       (map #(char (Integer/parseInt (apply str %) 2)))
       (apply str)))

(defn -main [& args]
  (print
    (if (= (count args) 2)
      (case (nth args 0)
        "get-wild" (get-wild-89s (slurp (nth args 1)))
        "get-back-wild" (get-back-wild-89s (slurp (nth args 1)))))))
