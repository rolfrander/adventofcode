
(defun count-parens (buffer)
  (with-current-buffer buffer
    (beginning-of-buffer)
    (let ((level 0))
      (while (< (point) (point-max))
	(let ((c (char-after (point))))
	  (cond ((= c ?() (incf level))
		((= c ?)) (decf level))))
	(forward-char))
      level)))

(count-parens "*advent-of-code-01*")

