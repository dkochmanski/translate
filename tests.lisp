
(require 'rt)
(in-package #:rt)

 ;; translations when language is bound to NIL

(setf translate:*resolution-time* :run-time)

(deftest translate.nil-lang.1
    (apply #'values
           (let ((translate:*language* nil))
             (mapcar #'equal
                     (list #t"phrase-1" #t"phrase-2" #t"phrase-3")
                     (list   "phrase-1"   "phrase-2"   "phrase-3"))))
  T T T)

(deftest translate.nil-lang.2
    (apply #'values
           (let ((translate:*language* nil))
           (mapcar #'equal
                   '(#t"phrase-1" #t"phrase-2" #t"phrase-3")
                   '(  "phrase-1"   "phrase-2"   "phrase-3"))))
  NIL NIL NIL)

(setf translate:*resolution-time* :load-time)

(deftest translate.nil-lang.3
    (apply #'values
           (mapcar #'equal
                   (list #t"phrase-1" #t"phrase-2" #t"phrase-3")
                   (list   "phrase-1"   "phrase-2"   "phrase-3")))
  T T T)

(deftest translate.nil-lang.4
    (apply #'values
           (mapcar #'equal
                   '(#t"phrase-1" #t"phrase-2" #t"phrase-3")
                   '(  "phrase-1"   "phrase-2"   "phrase-3")))
  T T T)

 ;; translations for defined language

(setf translate:*resolution-time* :run-time)

(translate:define-language 'en
    "phrase-1" "Phrase one")

(deftest translate.en-lang.1
    (apply #'values
           (let ((translate:*language* 'en))
             (mapcar #'equal
                     (list #t"phrase-1"   #t"phrase-2" #t"phrase-3")
                     (list   "Phrase one"  "{phrase-2}" "{phrase-3}"))))
  T T T)


