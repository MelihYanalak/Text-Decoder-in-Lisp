

(defun c2i (x)
	; Convert character to int.
	(- (char-int x) (char-int #\a))
)


(defun i2c (x)
	; Convert int to character.
	(int-char (+ x (char-int #\a)))
)