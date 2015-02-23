(in-package :destyle.tokenizer)

(defclass token () ())

(defmacro deftoken (name/names &body fields)
  (let ((names (if (listp name/names)
                   name/names
                   (list name/names))))
    `(progn
       ,@(loop
           for name in names
           collect
           `(defclass ,name (token)
              (,@(loop for field in fields
                       collect `(,(first field)
                                 :initarg ,(intern (string (first field))
                                                   :keyword)
                                 :reader ,(first field)
                                 ,@(and (second field)
                                        (list :type (second field)))
                                 ,@(and (third field)
                                        (list :initform (third field)))))))))))



(deftype hash-type ()
  '(member :id :unrestricted))

(deftype number-type ()
  '(member :integer :number))

(deftoken
  (<ident-token>
   <function-token>
   <at-keyword-token>)
  (value string))

(deftoken <hash-token>
  (value string)
  (hash-type hash-token-type :unrestricted))

(deftoken
  (<string-token>
   <url-token>
   <whitespace-token>
   <comment-token>)
  (value string))

(deftoken
  (<bad-string-token>
   <bad-url-token>))

(deftoken <delim-token> (value character))

(deftoken <number-token>
  (representation string)
  (value number)
  (number-type number-type :integer))

(deftoken <percentage-token>
  (representation string)
  (value number))

(deftoken <dimension-token>
  (representation string)
  (value number)
  (number-type number-type :integer)
  (unit string))

(deftoken <unicode-range-token>
  (start integer)
  (end integer))

(deftoken
  (<include-match-token>
   <dash-match-token>
   <dash-match-token>
   <prefix-match-token>
   <suffix-match-token>
   <substring-match-token>
   <column-token>
   <cdo-token>
   <cdc-token>
   <colon-token>
   <semicolon-token>
   <comma-token>
   <[-token>
   <]-token>
   <\(-token>
   <\)-token>
   <{-token>
   <}-token>))
