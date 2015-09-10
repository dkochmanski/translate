;;;; package.lisp

(defpackage #:translate
  (:use #:cl))

(in-package :translate)

(export '(*language*
          *resolution-time*
          define-language
          add-single-translation
          add-translations
          translate
          missing-translations))
