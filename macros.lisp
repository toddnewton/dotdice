(defmacro expand (num size)
  `(loop repeat ,num
      sum (random 1 (1+ ,size))))

;; string to dice
