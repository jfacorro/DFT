(ns test.dft
  (:require [dft :refer [dft build-original-fn to-polar]])
  (:use [incanter core stats charts]))

(let [begin  0
      end    (* 4 Math/PI)
      f      #(+ 0 (Math/sin %))
      g      (dft f begin end 20)
      f'     (build-original-fn g)
      pairs  (->> g
               (map to-polar)
               (mapcat (juxt :arg :mod))
               vec)]
  (doto 
    (scatter-plot
      (take-nth 2 pairs)
      (take-nth 2 (drop 1 pairs)))
    view)
  (doto
    (function-plot f begin end)
    (add-function f' begin end)
    view))
