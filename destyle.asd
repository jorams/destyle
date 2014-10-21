(asdf:defsystem #:destyle
  :description "A CSS parser"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:alexandria
               #:metabang-bind)
  :components ((:module "tokenizer"
                :components
                ((:file "package")
                 (:file "tokens")
                 (:file "consume")
                 (:file "parse")))))
