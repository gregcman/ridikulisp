;;;; wotwot.lisp

(in-package #:wotwot)

(defclass konz ()
  ((kar :initarg kar :initform nil)
   (flip :initarg flip :initform nil)))

(defun konz (ze1 ze2)
  (let ((a (make-instance 'konz 'kar ze1))
	(d (make-instance 'konz 'kar (flip ze2))))
    (setf (slot-value a 'flip) d)
    (setf (slot-value d 'flip) a)
    a))
;;;is konz really just a circular list with 2 cons cells?

(set-pprint-dispatch
 'konz
 (lambda (stream object)
   (write-char #\[ stream)
   (write (kar object) :stream stream)
   (write-char #\  stream)
   (write (kdr object) :stream stream)
   (write-char #\] stream)))

(defmethod kar ((k konz))
  (slot-value k 'kar))
(defmethod (setf kar) (new (k konz))
  (setf (slot-value k 'kar) new))

(defmethod flip ((k konz))
  (slot-value k 'flip))
(defmethod flip ((obj t))
  obj)

(defmethod kdr ((k konz))
  (flip (kar (flip k))))
(defmethod (setf kdr) (new (k konz))
  (setf (kar (flip k))
	(flip new)))

(defun lizt (&rest args)
  (labels ((rec (list)
	     (if list
		 (konz (car list)
		       (rec (cdr list)))
		 nil)))
    (rec args)))

(defun konvert-tree-to-kons (tree)
  "convert a tree of cons cells into a kons tree. each list becomes a kons cell, 
with the first and second element becoming the kar and kdr respectively"
  (if tree
      (if (consp tree)
	  (konz (konvert-tree-to-kons (first tree))
		(konvert-tree-to-kons (second tree)))
	  tree)
      nil))

;;;(konvert-tree-to-kons '((234 245) (2384 (2774 274))))

;;;1 1 -> 0
;;;1 0 -> 1
;;;0 1 -> 1
;;;0 0 -> 0

;;below, () represents a single cons cell?
#+nil 
(((dest ???)
  ((src ???) ???))
 next)

(defun step? (top)
  (setf (kar (kar (kar top)))
	(flip (kar (kar (flip (kar (flip (kar top))))))))
  (kdr top))

#+nil
"if konz cells are stored in an array, and each element of the array can point to other elements 
of the array, and the 'kar and 'kdr are stored as consecutive even and odd cells, then 'flip is just toggling the lowest bit of the index into the array"

#+nil
"what is the minimum number of 'flip's and 'kar's to have a programming language that's not a turing tarpit?"


(defparameter *cells* (make-hash-table))
(defparameter *chunk-size* 64)
(defun huh (n)
  (values (mod n *chunk-size*) ;;;offset into the chunk
	  (logandc1 (- *chunk-size* 1) n))) ;;;chunk number
(defun set-cell (n new)
  (multiple-value-bind (offset num) (huh n)
    (let ((array (gethash num *cells*)))
      (unless array
	(let ((new (make-array *chunk-size*)))
	  (setf (gethash num *cells*) new)
	  (setf array new)))
      (setf (aref array offset) new))))

(defun cell (n)
  (multiple-value-bind (offset num) (huh n)
    (let ((array (gethash num *cells*)))
      (if array
	  (aref array offset)
	  0))))
(defun (setf cell) (new n)
  (set-cell n new))
