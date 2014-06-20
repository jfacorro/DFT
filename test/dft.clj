(ns test.dft
  (:require [dft :refer [dft build-original-fn to-polar]])
  (:use [incanter core stats charts]))

(let [begin   (* 4 Math/PI -1)
      end     (* 4 Math/PI)
      samples 32
      f       #(+ 0 (Math/sin %))
;;      f      #(+ 0 (* 0.5 %))
      g       (dft f begin end samples)
      f'      (build-original-fn g)
      pairs   (->> g
                (map to-polar)
                (mapcat (juxt :arg :mod))
                vec)]
  (doto 
    (scatter-plot
      (range samples)
      (map :img g)
      :title "Imaginary")
    view)
  (doto 
    (scatter-plot
      (range samples)
      (map :real g)
      :title "Real")
    view)
  (doto
    (function-plot f begin end)
    (add-function f' begin end)
    view))
