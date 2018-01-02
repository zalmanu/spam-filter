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
(defparameter *total-spam-words* 0)
(defparameter *total-ham-words* 0)
(defparameter *total-spams* 0)
(defparameter *total-hams* 0)
(defparameter *total-words* 0)
(defparameter *alpha* 1)

(defun split (text)
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings "[a-zA-Z]{2,}" text)
    :test #'string=))

(defun get-or-create-word (word)
  (or (gethash word *words-db*) 
      (setf (gethash word *words-db*) 
            (make-instance 'words-count))))

(defun save-word (word) 
 (get-or-create-word word))

(defun get-word(word) 
 (get-or-create-word word))

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

(defun increment-total-words-count (type)
 (ecase type
  (ham (incf *total-ham-words*))
  (spam (incf *total-spam-words*)))
 (incf *total-words*))

(defun increment-total-count (type) 
 (ecase type 
  (ham (incf *total-hams*))
  (spam (incf *total-spams*))))

(defun train (text type)
  (dolist (word (extract-words text))
    (progn 
		(increment-word-count word type)
		(increment-total-words-count type)))
  (increment-total-count type))

(defun get-word-prob (word type)
 (with-slots (ham-cnt spam-cnt) (get-word word)
  (ecase type
   (ham (/ (+ ham-cnt *alpha*) (+ *total-ham-words* (* *alpha* *total-words*))))
   (spam (/ (+ spam-cnt *alpha*) (+ *total-spam-words* (* *alpha* *total-words*)))))))

(defun get-text-probab (text type)
 (let ((res 1.0))
  (dolist (word (split text))
   (setq res (* res (get-word-prob word type))))
  res))

(defun classify (text)
 (setq spam-prob (/ *total-spams* (+ *total-spams* *total-hams*)))
 (setq ham-prob (/ *total-hams* (+ *total-spams* *total-hams*)))
 (setq is-spam-prob (get-text-probab text 'spam))
 (setq is-ham-prob (get-text-probab text 'ham))
 (if (> (* spam-prob is-spam-prob) (* ham-prob is-ham-prob))
  (return-from classify 'spam)
  (return-from classify 'ham)))
