;;; Levenshtein Distance
;;;
;;; The number of modifications (insert, modify, delete) required for
;;; the first sequence to be equal to the second.

(defun iota (n &key (start 1))
  (loop :for i :from start :to n :collecting i))


(defun levenshtein (a b)
  "Compute the Levenshtein distance between the sequences A and B." 
  (when (zerop (length a)) (RETURN-FROM levenshtein (length b)))
  (when (zerop (length b)) (RETURN-FROM levenshtein (length a)))

  (let ((row (make-array (1+ (length b)) :initial-contents (iota (1+ (length b))))))
    (loop :for a-item :across a :and i :from 1 :to (length a) :do
       (setf (aref row 0) i)
       (let ((corner (1- i)))
         (loop :for b-item :across b :and j :from 1 :to (length b) :do
            (let ((upper (aref row j)))
              (setf (aref row j)
                    (if (eql a-item b-item)
                        corner
                        (1+ (min (aref row (1- j)) upper corner))))
              (setf corner upper)))))
    (aref row (length b))))
       

