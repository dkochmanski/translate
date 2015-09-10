;;;; cl-translate.lisp

(in-package #:translate)

 ;; global variables
(defparameter *language* nil
  "Language for which the translation is done")
(defparameter *translations* nil
  "Table containing further translation tables")
(defparameter *resolution-time* :run-time
  "Controlls translation resulution time - maybe be either :run-time
or :load-time")

 ;; creation
(defun define-language (name &rest translations)
  "Define language NAME with provided TRANSLATIONS

If LANGUAGE exists, a continuable error is signalled, which allows
either dropping the operation or superseding the language which is
already defined. TRANSLATIONS are alternating phrases and their
corresponding objects."
  (when (getf *translations* name)
    (cerror "Supersede language."
            "Language ~A is already defined." name))
  (setf (getf *translations* name)
        (list (make-hash-table :test 'equal)
              nil
              #'(lambda (str)
                  (concatenate 'string
                               "{" str "}"))))
  (apply #'add-translations name translations))

(defun add-single-translation (language phrase translation)
  "Add TRANSLATION of PHRASE for given LANGUAGE

If LANGUAGE doesn't exist, it is implicitly created and a warning is
emmited."
  (check-type phrase string)
  (let ((dict (car (or (getf *translations* language)
                       (progn
                         (warn "Implicitly creating language ~A." language)
                         (define-language language)
                         (getf *translations* language))))))
    (format t "[~a] ~a -> ~a~%" language phrase translation)
    (setf (gethash phrase dict) translation)))

(defun add-translations (language &rest translations)
  "Add any number of TRANSLATIONS for the given LANGUAGE"
  (do* ((tr translations (cddr tr)))
       ((endp tr) T)
    (destructuring-bind (phrase trans &rest ign) tr
      (declare (ignore ign))
      (add-single-translation language phrase trans))))

 ;; resolution
(defun translate (phrase &optional (language *language*))
  "Find the translation of PHRASE in the store associated with LANGUAGE

If LANGUAGE is NIL, then this is the same as the the IDENTITY
function. If the provided LANGUAGE isn't defined, the store is
explicitly created. If no PHRASE is defined for a given language, it
is stored for later translation and replaced by PHRASE surrunded by
curly brackets."
  (check-type phrase string)
  (if language
      (let ((lang
             (or (getf *translations* language)
                 (progn (cerror "Create language."
                                "Language ~A doesn't exist." language)
                        (define-language language)
                        (getf *translations* language)))))
        (destructuring-bind (dictionary missing missing-fn) lang
          (declare (ignore missing))
          (multiple-value-bind (result found?)
              (gethash phrase dictionary
                       (funcall missing-fn phrase))
            (unless found?
              (pushnew phrase (second lang) :test 'equal)
              (warn "Phrase \"~A\" isn't defined for language ~A."
                    phrase language))
            result)))
      phrase))

(set-dispatch-macro-character
 #\# #\t
 #'(lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let ((phrase (read stream)))
       (check-type phrase string)
       (if (eql *resolution-time* :load-time)
	   (translate phrase)
	   `(translate ,phrase)))))

 ;; missing translations
(defun missing-translations ()
  "Creates a list of phrases which aren't translated for the defined
languages. Returns a list of form: ({(LANG ({PHRASE}*))}*)"
  (do* (acc
        (reminder *translations* (cddr reminder))
        (elt (second (cadr reminder))
             (second (cadr reminder))))
      ((null reminder) acc)
    (when elt
      (push (list (car reminder)
                  elt)
            acc))))

#+(or)
(defun missing-translations-template ()
  `(block nil
     ,@(let (acc)
         (dolist (tr (missing-translations) acc)
           (push
            `(add-translations ,(car tr)
                               ,@(mapcar #'(lambda (elt)
                                             (list elt "{FILL-ME}"))
                                         (second tr)))
            acc)))))


