
; AHMET MELIH YANALAK 
;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"


(defun read-as-list (filename)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
	(setq lst (list ))          ;list of words to fill	
	(setq sublst (list ))	    ;list of letters to fill
	(let ((in (open filename :if-does-not-exist nil)))  ;opening file
	  (when in 
	    (loop for c = (read-char in nil) ;for each character
		 while c do 
		(if(or (eq c #\space)(eq c #\linefeed))   ;is char is whitespace or endofline
			(progn
				(setq sublst (reverse sublst))   ;to make sublist proper
				(setq lst (cons sublst lst))     ;add letter list to word list
				(setq sublst (list )))           ;clear temp sublist
			(setq sublst (cons c sublst))       ; ELSE add character to sublist 
		)

		)
	    (close in)))
	(setq lst (reverse lst))
	lst	;return list
	   	
)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***



(defun spell-checker-0 (word)
	(setq a Nil)        ;sets flag to 0 means word is not found yet
	(setq diclist (read-as-list "dictionary1.txt"))   ;;dictionary list
	(loop for dicword in diclist       ;; for each word in dictionary
		while dicword do
		(if (equal word dicword)     ;;check the word and dictionarywords
			(setq a t)
		)
	)
	a
		
)

(defun spell-checker-1 (word)
 	;you should implement this function
	(setq flag Nil)
	(setq dichash (make-hash-table))  ; create empty hash table
	(setq diclist (read-as-list "dictionary2.txt"))  ;; dictionary list
	(loop for subdiclst in diclist  :for dictionarywords in dictionary
		while subdiclst do
		(setf (gethash (sxhash subdiclst) dichash) subdiclst) ;; add dictionary word to hash table with its hash code by sxhash
	)

	(if (equal (gethash (sxhash word) dichash) word)   ;; if a taken word with using arguments hashcode is equal to argument,it exists 
		(setq flag t)
		(setq flag Nil)
	)
	flag
	
	
)
(defun all-permutations (list)    ; function that returns the list of all permutations for given list,I found it on internet not implemented myself
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

;; -----------------------------------------------------
;; DECODE FUNCTIONS
(defun create-alphabet-list () ; function for creating alphabet to get permutations of it
	;Normally function takes 26 letters but since the computation time is too long
	;I will use small set of alphabet to show that program decode correctly
	(setq lst '(#\a #\b #\c #\d)) 
	lst
)
(defun create-alphabet-list-2 () ; function for creating alphabet to get permutations of it
	(setq lst '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
	lst

)
(defun Gen-Decoder-A (paragraph)  
	(setq flag Nil)
	(setq documentlist (read-as-list "document1.txt")); convert document to list
	(setq permutationlist (all-permutations (create-alphabet-list))) ; create permutation list for given alphabet
	(loop for singleperm in permutationlist   ; for each combination of alphabet 
		while (and singleperm (equal flag Nil)) do 
			;controls if translated document with using current combination english
			(if (equal (is-list-english (helper-for-decoder-a singleperm documentlist) ) t) 
			(progn
				;if translated document is true then save the translated text in list
				(setq decodedText (helper-for-decoder-a singleperm documentlist))				
				(setq flag t)				
			))
			
		)
		(setq decodedText (reverse decodedText))
		(print decodedText) ; decoded text is result 
	
)

(defun Gen-Decoder-B-0 (paragraph)
  	;you should implement this function
)

(defun Gen-Decoder-B-1 (paragraph)
  	;you should implement this function
)

 
(defun Code-Breaker (document decoder)
  	;you should implement this function
	(let ((in (open document :if-does-not-exist nil)))
	  (when in
	    (loop for line = (read-line in nil)
		 while line do 
			(Gen-Decoder-A line)
			
		)
	    (close in)))
	
	
)

(defun helper-for-decoder-a(alphabetlist documentlist) ; this function translate file by using given alphabet and returns it as list
	(setq newList '())
	(setq sublst '())
	(loop for word in documentlist
		while word do 
			(setq sublst (vallahi-helper alphabetlist word))
			(setq newList (cons sublst newList))
			(setq sublst (list ))
	)
	;(setq newList (reverse newList))
	
	newList



)

(defun vallahi-helper (alphabetlist word)  ; this function translate word by using given alphabet (to be used in helper-for-decoder-a)
	(setq sublst '())
	(loop for char in word
		while char do
			(setq sublst (cons (nth (c2i char) alphabetlist) sublst))
					
	)
	(setq sublst (reverse sublst))
	sublst
	

)

(defun is-list-english (list)  ; this function checks if the given list is english by using spell-check
	(setq flag t)
	(loop for word in list
		while word do
			(if (equal (spell-checker-0 word) Nil)
				(setq flag Nil))
			
	)

	flag



)

(defun encode (document) ; this function encodes the given english document and write it to encoded.txt
	(setq alphabetlist (list))
	(setq x 0)
	(loop while (<= x 25) 
  	do
		(setq temp (i2c (random-from-range 0 25)))
		(if (equal (member temp alphabetlist) Nil)
			(progn 
			(push temp alphabetlist)
			(setq x (+ x 1))
			)
		)
	) 

	(with-open-file (instream document :direction :input :element-type '(unsigned-byte 8)
                            :if-does-not-exist nil)
	    (when instream
	      (with-open-file (outstream "encoded.txt" :direction :output :element-type '(unsigned-byte 8)
		                         :if-exists :supersede)
		(loop for byte = (read-byte instream nil)
		   while byte
		   do 
			(if (or (equal byte 10) (equal byte 32))
				(write-byte byte outstream) 
				(progn 
					(setq byte2 (+ (c2i (nth (- byte 97) alphabetlist)) 97 ))
					(write-byte byte2 outstream) 
				))
					
			))))



)
(defun random-from-range (start end) ; helper for encode function
  (+ start (random (+ 1 (- end start)))))

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()

	
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(encode "document2.txt") ; encode document2.txt to encoded.txt,You may check the files
	; I LİMİTED ALPHABET WITH A B C D TO CHECK PROGRAM,IN ORDER TO CHANGE ALPHABET YOU COULD CHECK 2 CREATEALPHABET FUNCTIONS
	;AND I ADD RANDOM 3 WORDS IN DICTIONARY1 LIKE THESE WORDS ARE ENGLISH SO DECODER SHOULD RETURN THEM IN MY TEST
	;I TEST THE PROGRAM WITH DICTIONARY1.TXT.IF YOU WISH TO CHANGE DICTIONARY NAME,YOU SHOULD CHANGE IT IN SPELL-CHECK-0
	(Gen-Decoder-A "document1.txt")
	
)


;; test code...
(test_on_test_data)
