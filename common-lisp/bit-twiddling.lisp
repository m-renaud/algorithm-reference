;;; Common Lisp has tonnes of built in support for bit-vectors.
;;;
;;; The syntax for a bit vector is #*{0,1}+
;;; For example: #*1001 === 9
;;;
;;; These bit vectors can be manipulated through the the bit-xyz
;;; functions. Note that the rank of the bit vectors used as arguments
;;; must be of the same rank.

;;; From http://www.lispforum.com/viewtopic.php?f=2&t=1205
(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

