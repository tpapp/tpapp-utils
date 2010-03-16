(in-package :tpapp-utils)

(defun kronecker (a b &optional (element-type 'double-float))
  "Kronecker product of two matrices."
  (check-type a (array * (* *)))
  (check-type b (array * (* *)))
  (bind (((a-rows a-cols) (array-dimensions a))
	 ((b-rows b-cols) (array-dimensions b))
	 (ab (make-array (list (* a-rows b-rows) (* a-cols b-cols))
			 :element-type element-type)))
    (dotimes (a-row a-rows)
      (dotimes (a-col a-cols)
	(let ((a-elt (aref a a-row a-col)))
	  (iter
	    (for b-row :from 0 :below b-rows)
	    (for ab-row :from (* a-row b-rows))
	    (iter
	      (for b-col :from 0 :below b-cols)
	      (for ab-col :from (* a-col b-cols))
	      ;; (format t "a[~a,~a]=~a  b[~a,~a]=~a  ab[~a,~a]=~a~%"
	      ;; 	      a-row a-col a-elt
	      ;; 	      b-row b-col (aref b b-row b-col)
	      ;; 	      ab-row ab-col (* a-elt (aref b b-row b-col)))
	      (setf (aref ab ab-row ab-col)
		    (coerce (* a-elt (aref b b-row b-col)) element-type)))))))
    ab))

(defun q-combine (a b &optional (element-type 'double-float))
  "Combine Q matrices for two independent processes and return the
result.  Note: Q-matrix properties are not checked, if the hold in the
input, they hold in the output."
  (check-type a (array * (* *)))
  (check-type b (array * (* *)))
  (bind (((a-rows a-cols) (array-dimensions a))
	 ((b-rows b-cols) (array-dimensions b))
	 (ab-rows (* a-rows b-rows))
	 (ab-cols (* a-cols b-cols))
	 (ab (make-array (list ab-rows ab-cols)
			 :element-type element-type
			 :initial-element (coerce 0 element-type))))
    (dotimes (a-row a-rows)
      (dotimes (a-col a-cols)
	(let ((a-elt (aref a a-row a-col))
	      (a-same (= a-row a-col)))
	  (iter
	    (for b-row :from 0 :below b-rows)
	    (for ab-row :from (* a-row b-rows))
	    (iter
	      (for b-col :from 0 :below b-cols)
	      (for ab-col :from (* a-col b-cols))
	      ;; (format t "a[~a,~a]=~a  b[~a,~a]=~a  ab[~a,~a]=~a~%"
	      ;; 	      a-row a-col a-elt
	      ;; 	      b-row b-col (aref b b-row b-col)
	      ;; 	      ab-row ab-col (* a-elt (aref b b-row b-col)))
	      (let ((b-same (= b-row b-col)))
		(when (or a-same b-same) ; jumps in both have 0 rate
		    (when a-same
		      (incf (aref ab ab-row ab-col)
			    (aref b b-row b-col)))
		    (when b-same 
		      (incf (aref ab ab-row ab-col)
			    a-elt)))))))))
    ab))

;; (defparameter *a* #2A((-2d0 2d0) (2d0 -2d0)))
;; (defparameter *b* #2A((-3d0 3d0) (3d0 -3d0)))
;; (q-combine *a* *b*)

(defun matrix-as-vector (matrix)
  (check-type matrix (array * (* *)))
  (displace-array matrix (array-total-size matrix) 0))

(defun vector-as-matrix (vector)
  (check-type vector (array * (*)))
  (displace-array vector (list (length vector) 1) 0))

(defun as-double (a)
  (coerce a 'double-float))

(defmacro-driver (FOR var FROM start BY step LENGTH n)
  "Iteration starting from start, stepping by step, for a total of n steps."
  ;; Note: reason for this macro is that manual calculation for :below
  ;; (+ start (* step n)) is tedious, and could also lead to incorrect
  ;; number of iterations with floating point values.
  (let ((i (gensym))		  ; counts from 0 to (1- n), inclusive
	(j (gensym))		  ; counts from start, by step
	(kwd (if generate 'generate 'for)))
    `(progn
       (with ,i = 0)
       (with ,j = ,start)
       (,kwd ,var next (prog2 (when (>= ,i ,n) (terminate))
			   ,j
			 (incf ,j ,step)
			 (incf ,i))))))

(defun constant-array-p (array)
  "Determine if all array elements are equal."
  (check-type array array)
  (let* ((n (array-total-size array))
	 (first (row-major-aref array 0)))
    (when (<= n 1)
      (return-from constant-array-p t))
    (iter
      (for i :from 1 :below n)
      (unless (equal (row-major-aref array i) first)
	(return-from constant-array-p nil)))
    t))


(defun mean-twopass (array)
  "Calculate the mean of an array using two passes when
necessary (leads to better accuracy).  Return the mean."
  (check-type array array)
  (let* ((n (array-total-size array))
	 (sum1 0))
    (dotimes (i n)
      (incf sum1 (row-major-aref array i)))
    (let ((mean1 (/ sum1 n)))
      (if (rationalp mean1)
	  mean1
	  (let ((sum2 0))
	    (dotimes (i n)
	      (incf sum2 (- (row-major-aref array i) mean1)))
	    (+ mean1 (/ sum2 n)))))))

(defun normalize-around-mean (array)
  "Subtract its mean from a array, returning a new array of the same
dimensions and element-type, and the mean as the second value."
  (check-type array array)
  (let* ((n (array-total-size array))
	 (mean (mean-twopass array))
	 (result (make-array (array-dimensions array)
			     :element-type (array-element-type array))))
    (dotimes (i n)
      (setf (row-major-aref result i) (- (row-major-aref array i) mean)))
    (values result mean)))

(defun covariance-of-normalized (x y)
  "Covariance of two arrays, which were normalized around their means."
  (check-type x array)
  (check-type y array)
  (let ((n (array-total-size x))
	(sum 0))
    (assert (<= 2 n))
    (assert (= n (array-total-size y)))
    (dotimes (i n)
      (incf sum (* (row-major-aref x i) (row-major-aref y i))))
    (/ sum (1- n))))

(defun ac1-of-normalized (x)
  "Autocovariance of a vector normalized around its mean."
  (check-type x (array * (*)))
  (let ((sum 0))
    (iter
      (for a :in-vector x)
      (for a-p :previous a)
      (unless (first-iteration-p)
	(incf sum (* a a-p))))
    (/ sum (- (length x) 2))))

(defun sd-from-cov (cov)
  "Extract the square root of the diagonal from a covariance matrix."
  (let ((n (array-dimension cov 0)))
    (assert (= n (array-dimension cov 1)))
    (let ((sd (make-array n)))
      (dotimes (i n)
	(setf (aref sd i) (let ((d (aref cov i i)))
			    (if d
				(sqrt d)
				nil))))
      sd)))
  
(defun cov-to-corr (cov)
  "Normalize a covariance matrix to a correlation matrix.  Only the
upper triangle is used from cov, and the lower triangle of the
returned cor matrix is nil.  The routine does not check that the
matrix is a proper covariance matrix."
  (check-type cov (array * (* *)))
  (let ((n (array-dimension cov 0)))
    (assert (= n (array-dimension cov 1)))
    (let ((sd (sd-from-cov cov))
	  (corr (make-array (list n n))))
      (dotimes (i n)
	(dotimes (j n)
	  (setf (aref corr i j)
		(cond
		  ((= i j) 1)
		  ((< i j) (let ((sdi (aref sd i))
				 (sdj (aref sd j)))
			     (if (or (zerop sdi) (zerop sdj))
				 0
				 (/ (aref cov i j) sdi sdj))))
		  (t nil)))))
      corr)))

;; (let* ((v (cl-numlib:int-sequence 0 100))
;;        (v-n (normalize-around-mean v))
;;        (cov (covariance-of-normalized v-n v-n))
;;        (ac1 (ac1-of-normalized v-n)))
;;   (list v-n cov (/ ac1 cov)))

