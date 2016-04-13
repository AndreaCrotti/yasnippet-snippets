(defun yas-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))

(defun yas-string-reverse (str)
  (apply #'string
	     (reverse
	      (string-to-list str))))

(defun yas-get-comment-start ()
  (substring comment-start 0
			 (dotimes (i (length comment-start))
			   (unless (or (eq (aref (yas-string-reverse comment-start) i) nil)
						   (eq (aref (yas-string-reverse comment-start) i) 32))
				 (return (- (length comment-start) i))))))

(defun yas-get-comment-end ()
  (if (eq (length comment-end) 0)
	  (yas-string-reverse (yas-get-comment-start))
	(substring comment-end
			   (dotimes (i (length comment-end))
				 (unless (or (eq (aref comment-end i) nil)
							 (eq (aref comment-end i) 32))
				   (return i))))))
