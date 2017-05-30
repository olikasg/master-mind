(ns master-mind.core
  (:gen-class))

(def number-of-colors 8)

(defn gen-code!
  []
  (loop [nums '()
        candidate (rand-int 8) ]
    (if (= 4 (count nums))
      (map inc nums)
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
                       (map #(Integer/parseInt %) (-> (clojure.string/trim raw-input)
                                                      (clojure.string/split  #" ")))
                       (catch NumberFormatException e ((println "Error! Please try again!") []))) ]
     (if (= (count result-list) 4)
       result-list
       (recur)))))

(defn check-guess
  [guess state]
  (if (= guess state)
    [true [:correct :correct :correct :correct]]
    [false (sort #(compare (name %1) (name %2))
                 (filter identity
                         (map (fn [g s]
                                (if (= g s) :correct
                                    (if (some #(= g %) state)
                                      :present
                                      nil))) (set guess) state)))]))

(defn print-result
  [guess result]
  (apply print guess)
  (print " :: ")
  (apply print (map name result))
  (println))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [state (gen-code!)]
    (loop []
      (let [guess (ask-for-guess)
            [final? result] (check-guess guess state)]
        (print-result guess result)
        (if final?
          (println "Congratulations!!!")
          (recur))))))

