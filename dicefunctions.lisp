(defun dicep (s)
  "Check if s is a string with the format [0-9]*d[0-9]+"
  (labels ((func (index str len d)
	     (if (not (= index len))
		 (let ((ch (char str index)))
		   (cond
		     ((and d (digit-char-p ch)) (func (1+ index) str len d))
		     ((and (not d) (char= #\d ch) (< index (1- len))) (func (1+ index) str len t))
		     ((digit-char-p ch) (func (1+ index) str len d))
		     (t nil)))
		 d)))
    (and (stringp s)
	 (func 0
	       (string-trim '(#\Space #\Tab #\Newline)
			    s)
	       (string-width s)
	       nil)
	 (not (= 0 (parse-integer (cadr (split-string #\d s))))))))

(defun roll (dice)
  "If (dicep dice), return a random result specified by dice, else nil"
  (when (dicep dice)
    (let* ((ls (split-string #\d (dice-format dice)))
	   (num (parse-integer (first ls)))
	   (size (parse-integer (second ls))))
      (unless (zerop size)
	(loop for i from 1 to num
	   sum (1+ (random size)))))))

(defun dice-format (dice)
  "If (dicep dice) and dice begins with d, concat a 1 to the front, else dice"
  (when (dicep dice)
    (if (char= #\d (char dice 0))
	(concatenate 'string "1" dice)
	dice)))
    
(defun split-string (delimiter str)
  "If (stringp STR), return an ordered list of substrings with DELIMITERs removed. If str has x delimiters, split-string will always return a list with x+1 cons cells. e.g. (split-string \#\\s \"Strings\") will return (\"\" \"tring\" \"\")"
  (when (stringp str)
    (let ((index (position delimiter str)))
      (cond
	((null index) (list str))
	(t (cons (subseq str 0 index)
		 (split-string delimiter
			       (subseq str (1+ index)))))))))
