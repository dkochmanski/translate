;;;; cl-translate.asd

(asdf:defsystem #:translate
  :name "translate"
  :version "0.1.1"
  :description "Abstraction layer for translations"
  :author "Daniel 'jackdaniel' Kochmański <daniel@turtleware.eu>"
  :license "LLGPLv2"
  :homepage "https://gitlab.common-lisp.net/dkochmanski/translate"
  :serial t
  :depends-on ()
  :components ((:file "package")
               (:file "translate"))
  :in-order-to ((asdf:test-op
                 (asdf:test-op #:translate/test))))

(asdf:defsystem #:translate/test
  :depends-on (#:translate #:fiveam)
  :components ((:file "tests"))
  :perform (asdf:test-op (o s)
             (funcall (intern (string '#:run!) :translate/test)
                      :translate-tests)))
