;;;; wotwot.lisp

(in-package #:wotwot)

(defclass konz-parent () ())

(defclass konz (konz-parent)
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

;;;;when nil, keep printing. level means print how deep, similar to *print-level*
(defparameter *konz-print-level* nil)
(defparameter *%konz-print-level* 0)

(defun print-konz (stream object)
  (flet ((next ()
	   (write-char #\[ stream)
	   (write (kar object) :stream stream)
	   (write-char #\  stream)
	   (write (kdr object) :stream stream)
	   (write-char #\] stream)))
    (if *konz-print-level*
	(if (>= *konz-print-level* *%konz-print-level*)
	    (let ((*%konz-print-level* (+ *%konz-print-level* 1)))
	      (next))
	    (princ "<...>" stream))
	(next))))

(set-pprint-dispatch
 'konz
 'print-konz)
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

#+nil
"if konz cells are stored in an array, and each element of the array can point to other elements 
of the array, and the 'kar and 'kdr are stored as consecutive even and odd cells, then 'flip is just toggling the lowest bit of the index into the array"

#+nil
"what is the minimum number of 'flip's and 'kar's to have a programming language that's not a turing tarpit?"

;;;;Virtual memory
(defparameter *cells* (make-hash-table))
(defparameter *chunk-size* 64)
(defun huh (n)
  (values (mod n *chunk-size*) ;;;offset into the chunk
	  (chunk-number n)))		       ;;;chunk number
(defun chunk-number (n)
  ;;;;FIXME::this depends on *chunk-size* being a power of two.
  ;;;;powers of two when subtracting one yield a series of ones. inverted,
  ;;;;that can mask off the lower bits to yield which chunk to put the thing in
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
;;;;

(defclass konznum (konz-parent)
  ((value :initarg value :initform nil)
   (index :initarg index :initform 0)
   (table :initarg table :initform *cells*)))

(set-pprint-dispatch
 'konznum
 'print-konz)

(defparameter *heap-pointer* 0) ;;0 is reserved ?? no?
(defparameter *heap-pointer-start* 0)
(defun heap-size ()
  (abs (- *heap-pointer*
	  *heap-pointer-start*)))

(defparameter *alloc-delta* 2)
;;;;how many array slots does 1 single konznum take?
(defparameter *konznum-cell-count* 2)

(defparameter *max-memory* 10000)
(defun need-to-collect-p (&optional (new-alloc-size 0))
  (>= (+ new-alloc-size (heap-size))
      *max-memory*))

(defun make-konsnum (ze1 ze2)
  #+nil
  ;;;;FIXME::this does not work, because the konz cells that
  ;;;;are being constructed in for example lizt will be collected?
  (when (need-to-collect-p *konznum-cell-count*)
    (collect))
  (let ((start *heap-pointer*))
    (incf *heap-pointer* *alloc-delta*)
    (let ((place1 (togglemod2 start)))
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

;;;;get the counterpart of the pointer to the konz cell
(defun togglemod2 (n)
  (logxor 1 n))

;;;;index into virtual memory depending on
;;;;which iteration the heap is on, indexing from 0
(defun translate-stable-index (n)
  (ecase *heap-direction*
    ((:plusp) (+ *heap-pointer-start* n))
    ((:minusp) (- *heap-pointer-start* n))))
(defun set-cell-stable (n new)
  (set-cell (translate-stable-index n) new))
(defun cell-stable (n)
  (cell (translate-stable-index n)))
(defun (setf cell-stable) (new n)
  (set-cell-stable n new))
;;;;
;;...dadadadadada|<- heap pointers->|adadadadad....

(defparameter *heap-direction* :plusp) ;;minusp
(defun print-heap (&optional (print-level 4))
  (let ((*konz-print-level* print-level))
    (dotimes (ptr (heap-size))
      (print (cell-stable ptr))
    ;;;put some space between consecutive cells
      (when (and (oddp ptr)
		 (plusp ptr))
	(terpri)))))

;;;Detect whether pointer n is in a valid heap
(defun in-heap (n)
  (ecase *heap-direction*
    ((:plusp) (>= (- *heap-pointer* 1) n *heap-pointer-start*))
    ((:minusp) (<= (+ *heap-pointer* 1) n *heap-pointer-start*))))

(defparameter *roots* nil)
;;;garbage collector
;;;semispace simulator
;;;The virtual memory uses all integers, positive and negative
;;;heap grows positively and when time to GC heap then grows negatively
(defun collect (&optional (roots *roots*))
  ;;Toggle the heap direction.
  (setf *heap-direction*
	(ecase *heap-direction*
	  ((:plusp) :minusp)
	  ((:minusp) :plusp)))
  (setf *alloc-delta* (* *konznum-cell-count*
			 (ecase *heap-direction*
			   ((:plusp) 1)
			   ((:minusp) -1))))
  (setf *heap-pointer-start*
	(ecase *heap-direction*
	  ((:plusp) 0)
	  ((:minusp) -1)))
  (setf *heap-pointer* *heap-pointer-start*)
  (let ((cells-collected 0))
    (labels
	(#+nil
	 (logged ()
	   (print (list
			*heap-pointer*
			)))
	 (move-to-space (n)
	   ;;   (print "try move")
	   ;;   (print n)
	   (when
	       ;;;;FIXME::Don't move cells that are already moved
	       (not (in-heap n))
	     ;;  (print "moved")
	     (move-konznum n
			   *heap-pointer*)
	     (incf cells-collected)
	     (incf *heap-pointer*
		   *alloc-delta*))))
      ;;  (logged)
      (dolist (v roots)
	(move-to-space v))
      ;; (logged)
      (block wot
	(let ((read-pointer *heap-pointer-start*))
	  (loop
	     (when (>= (abs read-pointer)
		       (abs *heap-pointer*))
	       (return-from wot nil))
	     ;;  (print "loop time")
	     (let ((obj (cell read-pointer)))
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
	     (incf read-pointer
		   *alloc-delta*)))))
    (when (need-to-collect-p)
      (error "heap exhausted"))
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
      ((:plusp) (or (> *heap-pointer-start* n)
		    (> n heap-pointer-page)))
      ((:minusp) (or (< *heap-pointer-start* n)
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
	     (let ((new-d (togglemod2 new-a))
		   (old-d (togglemod2 old-a)))
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

(defparameter *depth* 1)
(defun gengarbage ()
  (let ((huh
	 (random *depth*)))
    (if (or (zerop huh)
	    (zerop (random 3)))
	(let ((*depth* (+ *depth* 1)))
	  (konz (gengarbage)
		(gengarbage)))
	huh)))

(defun wowtest ()
  (let ((*konz-print-level* 4))
    (print "old:")
    (print-heap)
    (print "clearing:")
    (collect)
    (print-heap)
    (let ((lizt (lizt 0 1 2)))
      (dotimes (x 3)
	(print "repeated collecting:")
	(progn (collect (list (slot-value lizt 'index)))
	       (print-heap))))))

;;;1 1 -> 0
;;;1 0 -> 1
;;;0 1 -> 1
;;;0 0 -> 0

;;below, () represents a single cons cell?
(defparameter *konz-machine-specification*
  '(((dest ???)
     ((src ???) ???))
    next))

;;;;test input for konz-machine generator
#+nil
'(((nil (((dest ???) ???) ???))
   ((sdf ((src ???) ???))) 234)
  ((sdfsdf (next 78)) (??? ???)))

;;;;turn 'kdr into 'flip 'kar 'flip for encapsulate
(defun spread-kdr (list)
  (apply #'concatenate 'list
	 (mapcar (lambda (x)
		   (if (eq 'kdr x)
		       #+nil
		       (list 'flip 'kar 'flip)
		       (list x)
		       (list x)))
		 list)))

(defun konz-machine (&optional (tree *konz-machine-specification*))
   "konz machine generator"
  (let ((konzes (konvert-tree-to-kons tree)))
    (let ((stackspec
	   (konz-machine-spec konzes)))
      (flet ((ref (var)
	       (encapsulate (spread-kdr (second (assoc var stackspec))) 'pk)))
	
	(assert (eval
		 `(let ((pk ,konzes))
		    (and (eq ,(ref 'dest)
			     'dest)
			 (eq ,(ref 'next)
			     'next)
			 (eq ,(ref 'src)
			     'src)))))
	
	`(progn
	   (setf ,(ref 'dest)
		 (flip ,(ref 'src)))
	   ,(ref 'next))))))

(defun encapsulate (list core)
  (if list
      (encapsulate (cdr list)
		   (list (car list) core))
      core))

(defparameter *searching-for* '(dest src next))

;;;;walk the tree which is the specification for the one-instruction-set computer,
;;;;saving the kdrs and kars it takes to get to the location
(defun konz-machine-spec (spec)
  "the spec is a tree of konz cells, indicating where different \"registers\" are located"
  (let ((acc nil))
    (labels ((rec (spec &optional (stack nil))
	       (cond
		 ((symbolp spec)
		  (when (member spec *searching-for*)
		    (push (list spec (reverse stack)) acc)))
		 ((typep spec 'konz-parent)
		  (rec (kar spec) (cons 'kar stack))
		  (rec (kdr spec) (cons 'kdr stack))))))
      (rec spec))
    acc))

;;;;pk = program kounter
(defun step? (pk)
  (setf (kar (kar (kar pk)))
	(flip (kar (kar (flip (kar (flip (kar pk))))))))
  (kdr pk))
