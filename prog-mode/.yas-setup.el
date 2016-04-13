(defun yas-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))

(defun yas-get-comment-start ()
  (substring comment-start 0
			 (dotimes (i (length comment-start))
			   (unless (or (eq (aref (reverse comment-start) i) nil)
						   (eq (aref (reverse comment-start) i) 32))
				 (return (- (length comment-start) i))))))

(defun yas-get-comment-end ()
  (if (eq (length comment-end) 0)
	  (reverse start)
	(substring comment-end
			   (dotimes (i (length comment-end))
				 (unless (or (eq (aref comment-end i) nil)
							 (eq (aref comment-end i) 32))
				   (return i))))))
