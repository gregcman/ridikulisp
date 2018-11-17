;;;; wotwot.asd

(asdf:defsystem #:wotwot
  :description "Describe wotwot here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:utility)
  :components ((:file "package")
               (:file "wotwot")))
