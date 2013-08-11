;;; gcd is a built-in function in CL, but I'll write one anyways.
;;; It's fine to write it recursively because CL optimizes away tail
;;; recursion.
(defun my-gcd (m n)
  "Compute the greatest common divisor of the two numbers M and N."
  (let ((remainder (mod m n)))
    (if (zerop remainder)
        n
        (my-gcd n remainder))))

(defun my-lcm (m n)
  "Compute the lowest common multiple of M and N."
  (/ (* m n) (my-gcd m n)))


(defun fib (n)
  "Compute the Nth Fibonacci number."
  (if (or (= n 0) (= n 1))
      1
      (+ (fib (- n 1)) (fib (- n 2)))))
