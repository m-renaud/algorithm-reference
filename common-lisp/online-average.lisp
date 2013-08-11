;;; Online Average Calculation
;;;   From: The Art of Computer Programming Vol. 2, Third Edition, Page 232
;;;
;;;   Original:
;;;     M_1 = x_1;
;;;     M_k = M_{k-1} + (x_k - M_{k-1} / k;
;;;


;;; Implemented the algorithm presented so it is online.
;;;
;;; M : Previous mean.
;;; x : Next value in the sequence.
;;; k : Element number you are processing.
;;;
;;; Note: k nor M are modified by the algorithm, you are responsible
;;;       for doing that.
;;;
;;; Example:
;;;   (defparameter *average* 0.0)
;;;   (defparameter *counter* 1)
;;;   (setf *some-average* (online-avg *some-average* 10 (incf *counter*)))
;;;   (setf *some-average* (online-avg *some-average* 30 (incf *counter*)))
;;;   *average* ===> 20

(defun online-avg(M x k)
  (+ M (/ (- x M) k)))
