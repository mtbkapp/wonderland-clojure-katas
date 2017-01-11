(ns wonderland-number.finder)

(defn same-digits?
  [x y]
  (= (set (str x))
     (set (str y))))

(defn wonderland-number 
  []
  (some (fn [x]
          (if (and (same-digits? x (* 2 x))
                   (same-digits? x (* 3 x))
                   (same-digits? x (* 4 x))
                   (same-digits? x (* 5 x))
                   (same-digits? x (* 6 x)))
            x))
    (range 10000 (inc (quot 999999 6)))))
