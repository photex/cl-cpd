;;;; cl-cpd.asd

(asdf:defsystem #:cpd
  :serial t
  :description "Describe cl-cpd here"
  :author "Chip Collier <photex@lofidelitygames.com>"
  :license "MIT"
  :depends-on (#:cl-ppcre
               #:cxml
               #:sb-cga)
  :components ((:file "package")
               (:file "cpd")))

