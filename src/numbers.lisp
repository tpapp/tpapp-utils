(in-package :tpapp-utils)

(defun ordnum (n)
  "Return numeric ordinal number (1st, 12th, 103rd, 112th, etc)."
  (format nil "~d~a"
	  n
	  (let ((abs-n (abs n)))
	    (if (<= 11 (rem abs-n 100) 20)
		"th"
		(case (rem abs-n 10)
		  ((1) "st")
		  ((2) "nd")
		  ((3) "rd")
		  (t "th"))))))
