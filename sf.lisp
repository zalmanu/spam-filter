(defpackage :spamfilter (:use :common-lisp))

(ql:quickload :cl-ppcre)

(defclass words-count () 
 ((spam-cnt 
	:initarg :spam-cnt
	:accessor spam-cnt
	:initform 0)
   (ham-cnt 
	:initarg :ham-cnt
	:accessor ham-cnt
	:initform 0)
))

(defvar *words-db* (make-hash-table :test #'equal))

(defun parse-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{2,}" text)
   :test #'string=))

(defun save-word (word) 
	(or (gethash word *words-db*) 
		(setf (gethash word *words-db*) 
			   (make-instance 'words-count :word word))))

(defun init-words-count (text) 
	(mapcar #'save-word (parse-words text)))

(defun print-hash-map (hash-map)
	(loop for value being the hash-values of hash-map
		using (hash-key key) 
			do (
				with-slots (ham-cnt spam-cnt) value
				(format t "~&~A: ham - ~d, spam - ~d" key ham-cnt spam-cnt))))
