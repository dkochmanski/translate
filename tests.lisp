
(defpackage #:translate/test
  (:use #:cl #:5am #:translate))

(in-package :translate/test)

(def-suite :translate-tests :description "Tests for translate library.")
(in-suite :translate-tests)

 ;; Fixtures
(def-fixture time/lang (time lang)
  "Creates null translation environment with specified resolution time
and language"
  (let ((translate::*translations*   nil)
        (translate:*resolution-time* time)
        (translate:*language*        lang))
    (&body)))

 ;; dispatch macro character tests

(test resolution-time.reader
      "Test dispatch macro character with different resolution times"
      (with-fixture time/lang (:run-time nil)
                    (is (equal "phrase"
                               (eval (read-from-string "#t\"phrase\""))))
                    (is (equal '(translate:translate "phrase")
                               (read-from-string "#t\"phrase\""))))
      (with-fixture time/lang (:load-time nil)
                    (is (equal "phrase"
                               (read-from-string "#t\"phrase\""))))
      (signals simple-type-error
               (read-from-string "#t bah")))

 ;; translations for defined language

(test translation.language
      "Test single language definition"
      (with-fixture time/lang (:run-time 'en)
                    (translate:define-language 'en
                        "phrase-1" "Phrase one")
                    (is (equal "Phrase one" #t"phrase-1"))
                    (is (equal "{phrase-2}" #t"phrase-2"))
                    (is (equal "Phrase two"
                               (add-single-translation
                                'en "phrase-2" "Phrase two")))
                    (is (equal "Phrase two" #t"phrase-2"))))

(test translation.lexically-scoped-languages
      (with-fixture time/lang (:run-time nil)
                    (translate:define-language 'en
                        "phrase" "Phrase")
                    (translate:define-language 'pl
                        "phrase" "Fraza")
                    (is (equal "phrase" #t"phrase"))
                    (is (equal "Phrase"
                               (let ((*language* 'en))
                                 #t"phrase")))
                    (is (equal "Fraza"
                               (let ((*language* 'pl))
                                 #t"phrase")))))

(test translation.missing-phrases
      "Test single language definition"
      (with-fixture time/lang (:run-time 'en)
                    (translate:define-language 'en)
                    (is (equal "{phrase}" #t"phrase"))
                    (is (missing-translations))
                    (is (equal "Phrase"
                               (add-single-translation
                                'en "phrase" "Phrase")))
                    (is (null (missing-translations)))))


