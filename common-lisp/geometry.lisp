(defun square (x)
  (* x x))

(defun distance (p1 p2)
  "Calculate the Euclidian distance between two points, represented as lists."
  (sqrt (reduce #'+ (mapcar #'- p2 p1) :key #'square)))





