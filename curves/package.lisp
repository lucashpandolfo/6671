(defpackage #:sistemas-graficos.curves
  (:nicknames #:sg.curves)
  (:use #:cl)
  (:export 
   :bezier
   :add-control-points
   :control-points
   :get-point
   :discretize))