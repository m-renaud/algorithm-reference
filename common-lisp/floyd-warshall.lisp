;;; Floyd Warshall All Pairs Shortest Path
;;;
;;; Graph algorithm for finding all pairs shortest path
;;; in a weighted graph positive or negative edge weights, but no cycles.
;;;
;;; See: http://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm

(defun floyd_warshall (dist)
  "Find all pairs shortest path given by the DIST adjacency matrix."
  (let ((n (1- (array-dimension dist 0))))
    (loop :for k :from 0 :to n :do
       (loop :for i :from 0 :to n :do
          (loop :for j :from 0 :to n :do
             (when (and (not (zerop (* (aref dist i k) (aref dist k j))))
                        (/= i j))
               (when (or (< (+ (aref dist i k) (aref dist k j))
                            (aref dist i j))
                         (zerop (aref dist i j)))
                 (setf (aref dist i j)
                       (+ (aref dist i k) (aref dist k j)))))))))
  dist)
                            
