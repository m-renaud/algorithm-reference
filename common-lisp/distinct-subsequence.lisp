;;; Count Distinct Subsequences
;;;
;;; A subsequence of a string is a new string which is formed from the
;;; original string by deleting some of the characters without
;;; disturbing the relative positions of the remaining characters.


(defun back (seq)
  "Return the last element in a sequence."
  (elt seq (1- (length seq))))

(defun distinct-subsequences (seq)
  "Calculate the number of distinct subsequences in SEQ."
  (let ((last-occurence (make-hash-table))
        (sum (make-array (length seq)
                         :fill-pointer 0
                         :adjustable t
                         :element-type 'number)))

    ; Empty string has 1 subsequence.
    (vector-push-extend 1 sum) 
    (loop :for item :in seq :do
       (let* ((last (gethash item last-occurence))
              (repeats (if last (elt sum last) 0))
              (new-subsequences (- (back sum) repeats)))

         ; Add the new subsequence to the old sum.
         (vector-push-extend (+ (back sum) new-subsequences) sum)
         (setf (gethash item last-occurence) (- (length sum) 2))))
    (back sum)))

              
         
        
    
