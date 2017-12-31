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

(defparameter *words-db* (make-hash-table :test #'equal))
(defparameter *total-spams* 0)
(defparameter *total-hams* 0)

(defun split (text)
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings "[a-zA-Z]{2,}" text)
    :test #'string=))

(defun save-word (word) 
  (or (gethash word *words-db*) 
      (setf (gethash word *words-db*) 
            (make-instance 'words-count))))

(defun extract-words (text) 
  (mapcar #'save-word (split text)))

(defun print-hash-map (hash-map)
  (loop for value being the hash-values of hash-map
    using (hash-key key) 
    do (
      with-slots (ham-cnt spam-cnt) value
      (format t "~&~A: ham - ~d, spam - ~d" key ham-cnt spam-cnt))))

(defun print-words-db () 
 (print-hash-map *words-db*))

(defun increment-word-count (word type)
 (ecase type 
  (ham (incf (ham-cnt word)))
  (spam (incf (spam-cnt word)))))

(defun increment-total-count (type)
 (ecase type
  (ham (incf *total-hams*))
  (spam (incf *total-spams*))))

(defun train(text type)
  (dolist (word (extract-words text))
    (increment-word-count word type))
  (increment-total-count type))
