(ns dft
  (:require [clojure.pprint :as p]))

(defrecord Complex [real img])

(defn arg [c]
  (Math/atan2 (:img c) (:real c)))

(defn modulus [c]
  (let [x (:real c)
        y (:img c)]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn to-polar [{:keys [k] :as c}]
  {:arg (arg c) :mod (modulus c) :k k})

(def PI*2 (* 2 (Math/PI)))

(defn +-complex [& xs]
  (Complex. (apply + (map :real xs))
            (apply + (map :img xs))))

(defn term-k-t [n k x t]
  (let [phi (- (/ (* PI*2 k t) n))]
    (Complex. (* x (Math/cos phi))
              (* x (Math/sin phi)))))

(defn term-k-sum [n xt k]
  (-> (reduce +-complex
        (map (partial term-k-t n k) xt (range n)))
    (assoc :k (inc k))))

(defn dft
  "Takes a function f, an interval between begin and end; and
  a number of sampling points to extract from that interval. Returns
  the discrete components for f in the interval specified."
  [f begin end n]
  (let [delta (/ (- end begin) n)
        t     (range begin end delta)
        xt    (map f t)]
    (map (partial term-k-sum n xt)
         (range n))))

(defn build-original-fn
  "Given the DFT decomposition returns a function
  with the same mapping as the original in the "
  [Xk]
  (let [n         (count Xk)
        normalize #(update-in % [:mod] / n)
        Xk        (map (comp normalize to-polar) Xk)]
    (fn [t]
      (reduce (fn [r {:keys [mod arg k]}]
                  (+ r (* mod (Math/sin (+ arg (/ (* PI*2 t k) n))))))
              0 Xk))))
