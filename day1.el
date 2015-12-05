;; -*- mode: emacs-lisp -*-

(defun count-parens (buffer)
  (with-current-buffer buffer
    (beginning-of-buffer)
    (let ((level 0)
	  (basement 0))
      (while (< (point) (point-max))
	(let ((c (char-after (point))))
	  (cond ((= c ?() (incf level))
		((= c ?)) (decf level))))
	(if (and (= basement 0) (< level 0))
	    (setf basement (point)))
	(forward-char))
      (cons level basement))))

(count-parens "*advent-of-code-01*")
