;;;; The implementation of the tokenizer consists of a few things:
;;;; - DEFTOKEN name &body fields
;;;;   A call to DEFTOKEN specifies that a token type of that name exists and
;;;;   what its values are. DEFTOKEN can also accept a list of symbols as the
;;;;   name, specifying that token types of all of those names share the same
;;;;   characteristics.
;;;; - DEFINE-TOKEN-TRIGGER character[s] &body body
;;;;   A call to DEFINE-TOKEN-TRIGGER specifies that, when the tokenizer
;;;;   encounters a certain character the body should be evaluated.
;;;;   CHARACTER[S] can either be a character, a list of characters, or a
;;;;   predicate taking one character as an argument.
;;;; - Various CONSUME* functions, each of which tries to consume a certain
;;;;   type of token. The function (CONSUME number) just consumes NUMBER
;;;;   characters and returns them.
;;;; - Some DEFINE-X-TRIGGERS macros for specifying triggers that are nearly
;;;;   the same.

(defpackage :destyle.tokenizer
  (:use :cl :alexandria)
  (:import-from :bind :bind))
