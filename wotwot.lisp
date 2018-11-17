;;;; wotwot.lisp

(in-package #:wotwot)

(defclass konz ()
  ((kar :initarg kar :initform nil)
   (flip :initarg flip :initform nil)))

(defun konzoriginal (ze1 ze2)
  (let ((a (make-instance 'konz 'kar ze1))
	(d (make-instance 'konz 'kar (flip ze2))))
    (setf (slot-value a 'flip) d)
    (setf (slot-value d 'flip) a)
    a))

(defun konz (ze1 ze2)
  #+nil
  (konzoriginal ze1 ze2)
  (make-konsnum ze1 ze2))
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

(defun kdr (k)
  (flip (kar (flip k))))
(defun (setf kdr) (new k)
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
(defparameter *heap-pointer* 0) ;;0 is reserved ?? no?
(defun huh (n)
  (values (mod n *chunk-size*) ;;;offset into the chunk
	  (chunk-number n)))		       ;;;chunk number
(defun chunk-number (n)
  (logandc1 (- *chunk-size* 1) n))
(defun set-cell (n new)
  (multiple-value-bind (offset num) (huh n)
    (let ((array (gethash num *cells*)))
      (unless array
	(let ((new (make-array *chunk-size* :initial-element *konznum-nil*)))
	  (setf (gethash num *cells*) new)
	  (setf array new)))
      (setf (aref array offset) new))))

(defun cell (n)
  (multiple-value-bind (offset num) (huh n)
    (let ((array (gethash num *cells*))) 
      (if array
	  (aref array offset)
	  *konznum-nil*))))
(defun (setf cell) (new n)
  (set-cell n new))

(defclass konznum ()
  ((value :initarg value :initform nil)
   (index :initarg index :initform 0)
   (table :initarg table :initform *cells*)))

(set-pprint-dispatch
 'konznum
 ;;nil
; #+nil
 (lambda (stream object)
   (write-char #\[ stream)
   (write (kar object) :stream stream)
   (write-char #\  stream)
   (write (kdr object) :stream stream)
   (write-char #\] stream)))

(defun make-konsnum (ze1 ze2)
  (let ((start *heap-pointer*))
    (assert (evenp start))
    (setf *heap-pointer*
	  (+ start (ecase *heap-direction*
		     ((:plusp) 2)
		     ((:minusp) -2))))
    (let ((place1 (+ 1 start)))
      (setf (cell place1)
	    (make-instance
	     'konznum
	     'table *cells*
	     'index place1
	     'value (flip ze2))))
    (let* ((place0 start)
	   (k (make-instance
	       'konznum
	       'table *cells*
	       'index place0
	       'value ze1)))
      (setf (cell place0)
	    k)
      k)))

(defmethod kar ((k konznum))
  (slot-value k 'value))
(defmethod (setf kar) (new (k konznum))
  (setf (slot-value k 'value) new))
(defmethod flip ((k konznum))
  (cell (togglemod2 (slot-value k 'index))))
(defun togglemod2 (n)
  (logxor 1 n))

(defparameter *heap-direction* :plusp) ;;minusp
(defun heap-pointer-start ()
  (ecase *heap-direction*
    ((:plusp) 0)
    ((:minusp) -2)))
(defun toggle-heap-direction ()
  (setf *heap-direction*
	(ecase *heap-direction*
	  ((:plusp) :minusp)
	  ((:minusp) :plusp)))))

;;;garbage collector
;;;semispace simulator -> positive and negative two halves of semispace?
(defparameter *write-pointer* -2) ;;;write pointer and heap pointer are same?
(defparameter *read-pointer* 2)
(defun collect (&rest roots)
  (toggle-heap-direction)
  (setf *read-pointer* (heap-pointer-start))
  (setf *write-pointer* (heap-pointer-start))
  (let ((cells-collected 0))
    (labels
	(#+nil
	 (logged ()
	   (print (list *read-pointer*
			*write-pointer*
			)))
	 (move-to-space (n)
	   ;;   (print "try move")
	   ;;   (print n)
	   (when (ecase *heap-direction*
		   ((:plusp) (minusp n))
		   ((:minusp) (not (minusp n))))
	     ;;  (print "moved")
	     (move-konznum n
			   *write-pointer*)
	     (incf cells-collected)
	     (incf *write-pointer*
		   (ecase *heap-direction*
		     ((:plusp) 2)
		     ((:minusp) -2))))))
      ;;  (logged)
      (dolist (v roots)
	;;	(print v)
	(when (typep v 'konznum
		     )
	  (move-to-space (slot-value v 'index))))
      ;; (logged)
      (block wot
	(loop
	   (when (>= (abs *read-pointer*)
		     (abs *write-pointer*))
	     (return-from wot nil))
	   ;;  (print "loop time")
	   (let ((obj (cell *read-pointer*)))
	     (when (typep obj 'konznum)
	       ;;  (print "234234234")
	       (let ((karobj (kar obj)))
		 ;; (format t "what: ~a" karobj)
		 (when (typep karobj 'konznum)
		   (move-to-space (slot-value karobj 'index))))
	       (let ((kdrobj (kdr obj)))
		 (when (typep kdrobj 'konznum)
		   (move-to-space (slot-value kdrobj 'index))))))
	   ;;   (logged)
	   (incf *read-pointer*
		 (ecase *heap-direction*
		   ((:plusp) 2)
		   ((:minusp) -2))))))
    (setf *heap-pointer* *write-pointer*)
    (delete-extra-gc-pages)
    cells-collected))

(defun delete-extra-gc-pages ()
  (utility:dohash (k v) *cells*
		  (declare (ignorable v))
		  (when (page-removable k)
		    (remhash k *cells*))))

(defun page-removable (n)
  (let ((heap-pointer-page (chunk-number *heap-pointer*)))
    ;;(print (list n heap-pointer-page))
    (ecase *heap-direction*
      ((:plusp) (or (minusp n)
		    (> n heap-pointer-page)))
      ((:minusp) (or (not (minusp n))
		     (< n heap-pointer-page))))))

(defparameter *konznum-nil* 0)

;;;iterate up to *heap pointer*
;;;heap pointer grows backwards in negative phase

(defmethod update-konzum-index (new-index (k konznum))
  (setf (slot-value k 'index) new-index))

(defun move-konznum (old-n new-n)
  (let ((old-a old-n)
	(new-a new-n))
    (let ((koznum-a (cell old-a)))
      (cond ((typep koznum-a 'konznum)
	     (update-konzum-index new-a koznum-a)
	     (setf (cell new-a) koznum-a
		   (cell old-a) new-a)
	     (let ((new-d (flipfun new-a))
		   (old-d (flipfun old-a)))
	       (let ((koznum-d (cell old-d)))
		 (update-konzum-index new-d koznum-d)
		 (setf (cell new-d) koznum-d
		       (cell old-d) new-d)))
	     t)
	    (t nil)))))

#+nil
(apply #'collect
       (mapcar
	(lambda (x)
	  (apply #'lizt (make-list (random x) :initial-element (random 34))))
	(make-list 9 :initial-element 9)))
