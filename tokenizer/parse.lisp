(in-package :destyle.tokenizer)

(defparameter *token-triggers* ())

(defmacro define-token-trigger (trigger &body body)
  `(appendf *token-triggers*
            (list (list ,trigger
                        #'(lambda () ,@body)))))

(defmacro define-simple-token-triggers (&body char->class)
  `(progn
     ,@(loop for (char class) on char->class by #'cddr
             collect `(define-token-trigger ,char
                        (consume)
                        (make-instance ',class)))))

(defmacro define-match-token-triggers (&body char->class)
  `(progn
     ,@(loop for (char class) on char->class by #'cddr
             collect `(define-token-trigger ,char
                        (cond ((and (characterp (peek 2))
                                    (char= #\= (peek 2)))
                               (progn (consume 2)
                                      (make-instance ',class)))
                              (t (consume 1)
                                 (make-instance '<delim-token> :value ,char)))))))

(defun trigger (char)
  (let ((fun (find-if #'(lambda (trigger)
                          (typecase (first trigger)
                            (character (char= char (first trigger)))
                            (list (member char (first trigger)))
                            (function (funcall (first trigger) char))
                            (boolean trigger)))
                      *token-triggers*)))
    (funcall (second fun))))

(defun parse (input)
  (let ((*input* input)
        (*index* -1))
    (loop for char = (peek 1)
          while char
          collect (trigger char))))

(define-token-trigger #'whitespace-p
  (make-instance '<whitespace-token>
                 :value (consume-while #'whitespace-p)))

(define-token-trigger #\"
  (consume)
  (consume-string #\"))

(define-token-trigger #\'
  (consume)
  (consume-string #\'))

(define-token-trigger #\#
  (or (consume-hash)
      (consume-delim)))

(define-simple-token-triggers
  #\( <\(-token>
  #\) <\)-token>
  #\[ <[-token>
  #\] <]-token>
  #\{ <{-token>
  #\} <}-token>
  #\, <comma-token>
  #\: <colon-token>
  #\; <semicolon-token>)

(define-match-token-triggers
  #\$ <suffix-match-token>
  #\* <substring-match-token>
  #\^ <prefix-match-token>
  #\~ <include-match-token>)

(define-token-trigger #\|
  (or (when (and (characterp (peek 2))
                 (char= #\= (peek 2)))
        (progn (consume 2)
               (make-instance '<dash-match-token>)))
      (when (and (characterp (peek 2))
                 (char= #\| (peek 2)))
        (consume)
        (make-instance '<column-token>))
      (consume-delim)))

(define-token-trigger #\+
  (or (consume-numeric)
      (consume-delim)))

(define-token-trigger #\-
  (or (consume-numeric)
      (consume-ident-like)
      (consume-cdc)
      (consume-delim)))

(define-token-trigger #\.
  (or (consume-numeric)
      (consume-delim)))

;; CONSUME-COMMENT will return T if it consumed a comment, or NIL if not.
;; It will not consume anything more.
(define-token-trigger #\/
  (or (consume-comment)
      (consume-delim)))

(define-token-trigger #\<
  (or (consume-cdo)
      (consume-delim)))

(define-token-trigger #\@
  (or (consume-at-keyword)
      (consume-delim)))

(define-token-trigger #\\
  (or (consume-ident-like)
      (error "Parse error")))

(define-token-trigger #'digit-char-p
  (consume-numeric))

(define-token-trigger '(#\u #\U)
  (or (consume-unicode-range)
      (consume-ident-like)))

(define-token-trigger #'name-start-character-p
  (consume-ident-like))

;; Catch all
(define-token-trigger t
  (consume-delim))
