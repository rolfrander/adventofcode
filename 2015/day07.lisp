;;; Converts input to lisp-functions in circuit-package. Each wire is
;;; represented by a lisp function. This is an elegant solution, as
;;; calculating the value on a wire is done by calling the
;;; corresponding function. However, this is not very efficient, as
;;; the value of each wire is re-calculated on each call. To improve
;;; this situation, each lisp functions use a closure to remember the
;;; actual value.
;;;
;;; Note that this method will only work if there is no recursion in
;;; the input.  For recursion, the simulation needs to include
;;; clock-ticks and some definition of registers or memory. Maybe some
;;; compiler-optimization-trickery could achieve the same thing...

(make-package 'circuit :use nil)

(defun split-string (string)
    "Returns a list of substrings of string divided by ONE space each.
Note: Two consecutive spaces will be seen as if there were an empty
string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;(split-string "dette er en test -> foo bar 42")

(defun isnumber (string)
  (not
   (loop for c across string
      when (not (digit-char-p c)) return c)))

(defun funcall-or-number (string)
  "if string is a number, return the number, otherwise intern it as a
symbol and return it in a list, as a function call"
  (if (isnumber string)
      (parse-integer string)
      (list (intern (string-upcase string) 'circuit))))

(defun lshift (x y)
  "local version of shift, limiting result to 16 bits"
  (logand (ash x y) #xFFFF))

(defun rshift (x y)
  "right shift, defined in terms of left shift"
  (ash x (- y)))

(defun my-not (x)
  "bitwise-16bit-not"
  (logand (lognot x) #xFFFF))

(defun getfun (cmd)
  "mapping input function-names to lisp functions"
  (cond
    ((equal cmd "AND"   ) 'logand)
    ((equal cmd "OR"    ) 'logior)
    ((equal cmd "NOT"   ) 'my-not)
    ((equal cmd "LSHIFT") 'lshift)
    ((equal cmd "RSHIFT") 'rshift)))

(defun fun-assign (name-string value-string)
  (let ((sym (intern (string-upcase name-string) 'circuit))
	(value (funcall-or-number value-string)))
    `(defun ,sym ()
       ;(princ ,(format nil "~A~%" name-string))
       ,value)))

(defun fun-one (name-string fun-string value-string)
  (let ((sym (intern (string-upcase name-string) 'circuit))
	(value (funcall-or-number value-string))
	(fun (getfun fun-string)))
    `(defun ,sym ()
       ;(princ ,(format nil "~A~%" name-string))
       (,fun ,value))))

(defun fun-two (name-string fun-string value1-string value2-string)
  (let ((sym (intern (string-upcase name-string) 'circuit))
	(value1 (funcall-or-number value1-string))
	(value2 (funcall-or-number value2-string))
	(fun (getfun fun-string)))
    `(let ((value nil))
       (defun ,sym ()
	 (when (null value)
	   (setf value (,fun ,value1 ,value2))
	   (princ (format nil "~A -> ~A~%" ,name-string value)))
	 value
	 ))))

(defun parse-line (string)
  "parse a line of input and defines a function in the circuit-package"
  (let ((command-line (split-string string)))
    (case (length command-line)
      (3 (destructuring-bind (y arrow name) command-line
	   (fun-assign name y)))
      (4 (destructuring-bind (cmd y arrow name) command-line
	   (fun-one name cmd y)))
      (5 (destructuring-bind (x cmd y arrow name) command-line
	   (fun-two name cmd x y))))))

(defun parse-file (file)
  (with-open-file (input file)
    (loop for line = (read-line input nil)
       while line do (eval (parse-line line)))))

(parse-file "input07.txt")

(setf answer-a (circuit::a))

;; need to re-read the file as all defined functions remember the
;; value returned the last time
(parse-file "input07.txt")

(defun circuit::b () answer-a)

(circuit::a)

;;; testcode

;;; (eval (parse-line "5 -> x"))
;;; (eval (parse-line "x -> y"))
;;; (eval (parse-line "x OR y -> z"))
;;; (circuit::z)
;;; (parse-line "NOT kt -> ku")
;;; (parse-line "iu LSHIFT 1 -> jn")
;;; (parse-line "lf AND lq -> ls")

