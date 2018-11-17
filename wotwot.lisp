;;;; wotwot.lisp

(in-package #:wotwot)
;;below, () represents a single cons cell?
#+nil 
(((dest . ???) . ((src . ???) . ???)) . next)

(defclass konz ()
  ((kar :initarg kar :initform nil)
   (kdr :initarg kdr :initform nil)
   (flip :initarg flip :initform nil)))

(defmethod kar ((k konz))
  (if (slot-value k 'flip)
      (slot-value k 'kdr)
      (slot-value k 'kar)))

(defmethod kdr ((k konz))
  (if (slot-value k 'flip)
      (slot-value k 'kar)
      (slot-value k 'kdr)))

(defun konz (ze1 ze2)
  (make-instance 'konz 
		 'kar ze1
		 'kdr ze2))
