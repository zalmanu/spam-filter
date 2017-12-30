(defpackage :spamfilter (:use :common-lisp))

(defclass words-count () 
 ((word 
	:initarg :word
	:accessor word
	:initform (error ":word is required"))
   (spam-cnt 
	:initarg :spam-cnt
	:accessor spam-cnt
	:initform 0)
   (ham-cnt 
	:initarg :ham-cnt
	:accessor ham-cnt
	:initform 0)
  ))
