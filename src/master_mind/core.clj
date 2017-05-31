(ns master-mind.core
  (:gen-class))

(def number-of-colors 8)

(defn gen-code!
  []
  (loop [nums '()
        candidate (rand-int 8) ]
    (if (= 4 (count nums))
      (into [] (map inc nums))
      (if (some #(= candidate %) nums)
        (recur nums (rand-int 8))
        (recur (conj nums candidate) (rand-int 8)))))
  )

;; state [3 5 2 1]
;; guess [3 1 2 6]
;; final? false
;; result [:correct :present :present]


;;  Guess ==> 3  4  6  1
;;    3 4 6 1 :: correct present present
;;  Guess ==> 3  2  6  1
;;    3 2 6 1 :: correct present present present
;;  Guess ==> 3  2  1  6
;;    3 2 1 6 :: correct correct correct present
(defn ask-for-guess
  []
  (loop []
    (print "Guess ==> ")
    (flush)
    (let [raw-input (read-line)
          result-list (try
                        (mapv #(Integer/parseInt %) (-> (clojure.string/trim raw-input)
                                                        (clojure.string/split  #" ")))
                        (catch Exception _ (println "Error! Please try again!"))) ]
      (if (= (count result-list) 4)
        result-list
        (recur)
        ))))

(defn check-guess
  [guess state]
  ;; state = [2 1 3 4]
  (defn present? [elem coll] (some #(= elem %) coll))
  ;; guess = [1 1 2 5]
  (def guess-with-pos (into [] (map vector guess (range 4))))
  ;; guess-with-pos = [[1 0] [1 1] [2 2] [5 2]]
  (def correct-positions (map (fn [[g pos]]
                                (if (= (get state pos) g)
                                  [g pos :correct]
                                  [g pos :undecided]))
                              guess-with-pos))
  ;; correct-position = [[1 0 :correct] [1 1] [2 2] [5 2]]
  (def corrects-and-presents (map (fn [[g pos verdict]]
                                    (if (= verdict :undecided)
                                      (if (present? g state) [g :present] [g :not-present])
                                      [g verdict]))
                                  correct-positions))
  ;; correct-position = [[1 0 :correct] [1 1 :present] [2 2 :present] [5 2 :not-present]]
  (def result (reduce (fn [acc [g verdict]]
                        ;; do not update verdict if it is :correct
                        (if (= (get acc g) :correct)
                          acc
                          (conj acc [g verdict])))
                      {}
                      corrects-and-presents))
  ;; result = {1 :correct, 2 :present, 5 :not-present}
  (if (= guess state)
    [true [:correct :correct :correct :correct]]
    ;;     sorting guarantees that :correct elements come first. This depends on
    ;;     english names used in the repesentation
    [false (sort #(compare (name %1) (name %2))
                 ;; filter out not-present values and keep only the values
                 (filter #(not (= %1 :not-present))
                         (vals result)))]))

(defn print-result
  [guess result]
  (print "  ")
  (apply print guess)
  (print " :: ")
  (apply print (map name result))
  (println))

(defn -main
  [& args]
  (println "Try to guess the correct sequence!\nExample input: 1 2 3 4")
  (let [state (gen-code!)]
    (loop []
      (let [guess (ask-for-guess)
            [final? result] (check-guess guess state)]
        (print-result guess result)
        (if final?
          (println "Congratulations!!!")
          (recur))))))

