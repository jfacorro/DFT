(ns dft
  (:require [clojure.pprint :as p]))

(defrecord Complex [real img])

(defn arg [c]
  (Math/atan2 (:img c) (:real c)))

(defn modulus [c]
  (let [x (:real c)
        y (:img c)]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn to-polar [c]
  {:arg (arg c) :mod (modulus c)})

(def PI*2 (* 2 (Math/PI)))

(defn +-complex [& xs]
  (Complex. (apply + (map :real xs))
            (apply + (map :img xs))))

(defn term-k-t [n k x t]
  (let [phi (- (/ (* PI*2 k t) n))]
    (Complex. (* x (Math/cos phi)) 
              (* x (Math/sin phi)))))

(defn term-k-sum [n xt k]
  (reduce +-complex (map (partial term-k-t n k) xt (range n))))

(defn dft [f begin end n]
  (let [delta (/ (- end begin) n)
        t     (range begin end delta)
        xt    (map f t)]
    (map (partial term-k-sum n xt) (range n))))

(defn normalize [N x]
  (assoc x :mod  (/ (:mod x) (Math/sqrt N))))

(defn freq [i N])

(defn build-original-fn [Xk]
  (letfn [(normalize [x]
            (assoc x :mod  (/ (:mod x) (Math/sqrt (count Xk)))))]
    (map (comp normalize to-polar) Xk)))

(-> #(Math/sin %)
    (dft 0 Math/PI 10)
    build-original-fn
    p/pprint)
