;;;; wotwot.asd

(asdf:defsystem #:wotwot
  :description "a ridiculous lisp"
  :author "terminal625 <625terminal at gmail dot com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:utility
	       #:trivial-garbage)
  :components ((:file "package")
               (:file "wotwot")))
