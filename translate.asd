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
               (:file "translate")))

