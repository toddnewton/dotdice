(defclass tokenstream ()
  ((tokens
    :initform ()
    :accessor tokens)
   (token-types
    :initform ()
    :accessor token-types)
   (comment-style
    :initarg :comment-style
    :initform nil
    :accessor comment-style))
  (:documentation "Stream of tokens read from file"))

(defmethod next ((ts tokenstream))
  (pop (token-types ts))
  (pop (tokens ts)))

(defmethod next-type ((ts tokenstream))
  (head (token-types ts)))

(defmethod add-token ((ts tokenstream) token type)
  (setf (tokens ts) (append (tokens ts) token))
  (setf (token-types ts) (append (token-types ts) type)))

(defmethod peek ((ts tokenstream))
  (head (tokens ts)))

(defun tokenize (file &optional comment-style)
  (let ((ts (make-instance 'tokenstream :comment-style comment-style)))
    (loop 
